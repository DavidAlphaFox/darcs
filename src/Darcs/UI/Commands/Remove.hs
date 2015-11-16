--  Copyright (C) 2002-2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}

module Darcs.UI.Commands.Remove ( remove, rm, unadd ) where

import Prelude hiding ( (^) )

import Control.Monad ( when, foldM )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults,
                        commandAlias, commandStub,
                        putWarning
                      , amInHashedRepository
                      )
import Darcs.UI.Commands.Add( expandDirs )
import Darcs.UI.Flags
    ( DarcsFlag, useCache, dryRun, umask, diffAlgorithm, fixSubPaths, verbosity )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking (..) )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , addToPending
    , readRecordedAndPending
    , readUnrecorded
    , listRegisteredFiles
    )
import Darcs.Repository.Diff( treeDiff )
import Darcs.Patch ( RepoPatch, PrimOf, PrimPatch, adddir, rmdir, addfile, rmfile,
                     listTouchedFiles )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+), nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), Gap(..), FreeLeft, unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction, FileType )
import Storage.Hashed.Tree( Tree, TreeItem(..), find, modifyTree, expand, list )
import Darcs.Util.Path( anchorPath, AnchoredPath, fn2fp, SubPath, sp2fn
                      , AbsolutePath )
import Storage.Hashed( floatPath )

import Darcs.Util.Printer ( text )


removeDescription :: String
removeDescription = "Remove files from version control."

removeHelp :: String
removeHelp =
 "The `darcs remove` command exists primarily for symmetry with `darcs\n" ++
 "add`, as the normal way to remove a file from version control is\n" ++
 "simply to delete it from the working tree.  This command is only\n" ++
 "useful in the unusual case where one wants to record a removal patch\n" ++
 "WITHOUT deleting the copy in the working tree (which can be re-added).\n" ++
 "\n" ++
 "Note that applying a removal patch to a repository (e.g. by pulling\n" ++
 "the patch) will ALWAYS affect the working tree of that repository.\n"

removeBasicOpts :: DarcsOption a (Maybe String -> Bool -> a)
removeBasicOpts = O.workingRepoDir ^ O.recursive

removeAdvancedOpts :: DarcsOption a (O.UMask -> a)
removeAdvancedOpts = O.umask

removeOpts :: DarcsOption a
              (Maybe String
               -> Bool
               -> Maybe O.StdCmdAction
               -> Bool
               -> Bool
               -> O.Verbosity
               -> Bool
               -> O.UMask
               -> O.UseCache
               -> Maybe String
               -> Bool
               -> Maybe String
               -> Bool
               -> a)
removeOpts = removeBasicOpts `withStdOpts` removeAdvancedOpts

remove :: DarcsCommand [DarcsFlag]
remove = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "remove"
    , commandHelp = removeHelp
    , commandDescription = removeDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<FILE or DIRECTORY> ..."]
    , commandCommand = removeCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc removeAdvancedOpts
    , commandBasicOptions = odesc removeBasicOpts
    , commandDefaults = defaultFlags removeOpts
    , commandCheckOptions = ocheck removeOpts
    , commandParseOptions = onormalise removeOpts
    }

removeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
removeCmd fps opts relargs = do
    when (null relargs) $
        fail "Nothing specified, nothing removed."
    origfiles <- fixSubPaths fps relargs
    when (null origfiles) $
        fail "No valid arguments were given."
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
        args <- if parseFlags O.recursive opts
                then reverse `fmap` expandDirs False origfiles
                else return origfiles
        Sealed p <- makeRemovePatch opts repository args
        let notQuiet = verbosity opts /= O.Quiet
        when (nullFL p && not (null origfiles) && notQuiet) $
            fail "No files were removed."
        addToPending repository YesUpdateWorking p
        when notQuiet $
            putStr $ unlines $ ["Will stop tracking:"] ++ listTouchedFiles p

-- | makeRemovePatch builds a list of patches to remove the given filepaths.
--   This function does not recursively process directories. The 'Recursive'
--   flag should be handled by the caller by adding all offspring of a directory
--   to the files list.
makeRemovePatch :: (RepoPatch p, ApplyState p ~ Tree)
                => [DarcsFlag] -> Repository p wR wU wT
                -> [SubPath] -> IO (Sealed (FL (PrimOf p) wU))
makeRemovePatch opts repository files =
                          do recorded <- expand =<< readRecordedAndPending repository
                             unrecorded <- readUnrecorded repository $ Just files
                             ftf <- filetypeFunction
                             result <- foldM removeOnePath (ftf,recorded,unrecorded, []) $ map (floatPath . fn2fp . sp2fn) files
                             case result of
                                 (_, _, _, patches) -> return $
                                                         unFreeLeft $ foldr (joinGap (+>+)) (emptyGap NilFL) $ reverse patches
    where removeOnePath (ftf, recorded, unrecorded, patches) f = do
            let recorded' = modifyTree recorded f Nothing
                unrecorded' = modifyTree unrecorded f Nothing
            local <- makeRemoveGap opts ftf recorded unrecorded unrecorded' f
            -- we can tell if the remove succeeded by looking if local is
            -- empty. If the remove succeeded, we should pass on updated
            -- recorded and unrecorded that reflect the removal
            return $ case local of
                       Just gap -> (ftf, recorded', unrecorded', gap : patches)
                       _        -> (ftf, recorded, unrecorded, patches)

-- | Takes a file path and returns the FL of patches to remove that, wrapped in
--   a 'Gap'.
--   Returns 'Nothing' in case the path cannot be removed (if it is not tracked,
--   or if it's a directory and it's not tracked).
--   The three 'Tree' arguments are the recorded state, the unrecorded state
--   excluding the removal of this file, and the unrecorded state including the
--   removal of this file.
makeRemoveGap :: PrimPatch prim => [DarcsFlag] -> (FilePath -> FileType)
                -> Tree IO -> Tree IO -> Tree IO -> AnchoredPath
                -> IO (Maybe (FreeLeft (FL prim)))
makeRemoveGap opts ftf recorded unrecorded unrecorded' f =
    case (find recorded f, find unrecorded f) of
        (Just (SubTree _), Just (SubTree unrecordedChildren)) ->
            if not $ null (list unrecordedChildren)
              then skipAndWarn "it is not empty"
              else return $ Just $ freeGap (rmdir f_fp :>: NilFL)
        (Just (File _), Just (File _)) -> do
            Just `fmap` treeDiff (diffAlgorithm opts) ftf unrecorded unrecorded'
        (Just (File _), _) ->
            return $ Just $ freeGap (addfile f_fp :>: rmfile f_fp :>: NilFL)
        (Just (SubTree _), _) ->
            return  $ Just $ freeGap (adddir f_fp :>: rmdir f_fp :>: NilFL)
        (_, _) -> skipAndWarn "it is not tracked by darcs"
  where f_fp = anchorPath "" f
        skipAndWarn reason =
            do putWarning opts . text $ "Can't remove " ++ f_fp
                                        ++ " (" ++ reason ++ ")"
               return Nothing


rmDescription :: String
rmDescription = "Help newbies find `darcs remove'."

rmHelp :: String
rmHelp =
 "The `darcs rm' command does nothing.\n" ++
 "\n" ++
 "The normal way to remove a file from version control is simply to\n" ++
 "delete it from the working tree.  To remove a file from version\n" ++
 "control WITHOUT affecting the working tree, see `darcs remove'.\n"

rm :: DarcsCommand [DarcsFlag]
rm = commandStub "rm" rmHelp rmDescription remove

unadd :: DarcsCommand [DarcsFlag]
unadd = commandAlias "unadd" Nothing remove

