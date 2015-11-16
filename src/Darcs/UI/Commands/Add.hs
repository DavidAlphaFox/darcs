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

-- |
-- Module      : Darcs.UI.Commands.Add
-- Copyright   : 2002-2004 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

{-# LANGUAGE CPP #-}
module Darcs.UI.Commands.Add
    (
      add
    , expandDirs
    ) where


#include "impossible.h"

import Prelude hiding ( (^), catch )

import Control.Exception ( catch, IOException )
import Control.Monad ( when, unless, liftM )
import Data.List ( (\\), nub )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( isNothing, maybeToList )
import Darcs.Util.Printer ( text )
import Storage.Hashed.Tree ( Tree, findTree, expand )
import Darcs.Util.Path ( floatPath, anchorPath, parents,
                    SubPath, toFilePath, simpleSubPath, toPath, AbsolutePath )
import System.FilePath.Posix ( takeDirectory, (</>) )
import System.Posix.Files ( isRegularFile, isDirectory, isSymbolicLink )
import System.Directory ( getPermissions, readable )

import qualified System.FilePath.Windows as WindowsFilePath

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, putInfo, putWarning, putVerboseWarning
    , nodefaults, amInHashedRepository)
import Darcs.UI.Flags
    ( DarcsFlag
    , includeBoring, doAllowCaseOnly, doAllowWindowsReserved, useCache, dryRun, umask
    , fixSubPaths, verbosity )
import Darcs.UI.Options
    ( DarcsOption
    , (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands.Util.Tree ( treeHas, treeHasDir, treeHasAnycase )
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.Patch ( Patchy, PrimPatch, applyToTree, addfile, adddir )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Repository.State ( readRecordedAndPending, updateIndex )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , listFiles
    , listUnregisteredFiles
    )
import Darcs.Repository.Prefs ( darcsdirFilter, boringFileFilter )
import Darcs.Util.File ( getFileStatus, withCurrentDirectory )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+), nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), Gap(..), FreeLeft, unFreeLeft )


addDescription :: String
addDescription = "Add one or more new files or directories."


addHelp :: String
addHelp =
    "Generally a repository contains both files that should be version\n" ++
    "controlled (such as source code) and files that Darcs should ignore\n" ++
    "(such as executables compiled from the source code).  The `darcs add`\n" ++
    "command is used to tell Darcs which files to version control.\n" ++
    "\n" ++
    "When an existing project is first imported into a Darcs repository, it\n" ++
    "is common to run `darcs add -r *` or `darcs record -l` to add all\n" ++
    "initial source files into darcs.\n"++
    "\n" ++
    "Adding symbolic links (symlinks) is not supported.\n\n"


addHelp' :: String
addHelp' =
    "Darcs will ignore all files and folders that look \"boring\".  The\n" ++
    "`--boring` option overrides this behaviour.\n" ++
    "\n" ++
    "Darcs will not add file if another file in the same folder has the\n" ++
    "same name, except for case.  The `--case-ok` option overrides this\n" ++
    "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
    "files a folder to have the same name except for case (for example,\n" ++
    "`ReadMe` and `README`).  If `--case-ok` is used, the repository might be\n" ++
    "unusable on those systems!\n\n"

addBasicOpts :: DarcsOption a
                (Bool -> Bool -> Bool -> Bool -> Maybe String -> O.DryRun -> a)
addBasicOpts = O.includeBoring
             ^ O.allowProblematicFilenames
             ^ O.recursive
             ^ O.workingRepoDir
             ^ O.dryRun

addAdvancedOpts :: DarcsOption a (O.UMask -> a)
addAdvancedOpts = O.umask

addOpts :: DarcsOption a
           (Bool
            -> Bool
            -> Bool
            -> Bool
            -> Maybe String
            -> O.DryRun
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
addOpts = withStdOpts addBasicOpts addAdvancedOpts

add :: DarcsCommand [DarcsFlag]
add = DarcsCommand
    { commandProgramName          = "darcs"
    , commandName                 = "add"
    , commandHelp                 = addHelp ++ addHelp'
    , commandDescription          = addDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = [ "<FILE or DIRECTORY> ..." ]
    , commandCommand              = addCmd
    , commandPrereq               = amInHashedRepository
    , commandGetArgPossibilities  = listUnregisteredFiles False
                                    -- bash completion should not offer boring files
    , commandArgdefaults          = nodefaults
    , commandAdvancedOptions      = odesc addAdvancedOpts
    , commandBasicOptions         = odesc addBasicOpts
    , commandDefaults             = defaultFlags addOpts
    , commandCheckOptions         = ocheck addOpts
    , commandParseOptions         = onormalise addOpts
    }


addCmd :: (AbsolutePath, AbsolutePath)
       -> [DarcsFlag]
       -> [String]
       -> IO ()
addCmd paths opts args
  | null args = putStrLn $ "Nothing specified, nothing added." ++
      "Maybe you wanted to say `darcs add --recursive .'?"
  | otherwise = do
      fs <- fixSubPaths paths args
      case fs of
        [] -> fail "No valid arguments were given"
        _ -> addFiles opts fs


addFiles :: [DarcsFlag]  -- ^ Command options
         -> [SubPath]
         -> IO ()
addFiles opts origfiles = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
    -- TODO do not expand here, and use findM/findIO or such later
    -- (needs adding to hashed-storage first though)
    cur <- expand =<< readRecordedAndPending repository
    let parlist = getParents cur (map toFilePath origfiles)
    flist' <- if parseFlags O.recursive opts
              then expandDirs (includeBoring opts) origfiles
              else return origfiles
    let flist = nubSort (parlist ++ toFilePath `map` flist')
    nboring <- if includeBoring opts
               then return darcsdirFilter
               else boringFileFilter
    mapM_ (putWarning opts . text . ((msgSkipping msgs ++ " boring file ")++)) $
        flist \\ nboring flist
    Sealed ps <- fmap unFreeLeft $ addp msgs opts cur $ nboring flist
    when (nullFL ps && not (null origfiles) && notQuiet) $
        fail "No files were added"
    unless gotDryRun $
      do addToPending repository YesUpdateWorking ps
         updateIndex repository
  where
    gotDryRun = dryRun opts == O.YesDryRun
    msgs | gotDryRun = dryRunMessages
         | otherwise = normalMessages
    notQuiet = verbosity opts /= O.Quiet


addp :: forall prim . (Patchy prim, PrimPatch prim, ApplyState prim ~ Tree)
     => AddMessages
     -> [DarcsFlag]
     -> Tree IO
     -> [FilePath]
     -> IO (FreeLeft (FL prim))
addp msgs opts cur0 files = do
    (ps, dups) <-
        foldr
             (\f rest cur accPS accDups -> do
                    addResult <- addp' cur f
                    case addResult of
                        -- If a single file fails to add, stop further processing.
                        (_, Nothing, Nothing) -> return ([], [])
                        (cur', mp, mdup) -> rest cur' (maybeToList mp ++ accPS) (maybeToList mdup ++ accDups))
            (\_ ps dups -> return (reverse ps, dups))
            files
            cur0 [] []
    let uniq_dups = nub dups
        caseMsg =
            if gotAllowCaseOnly then ":"
                else ";\nnote that to ensure portability we don't allow\n" ++
                     "files that differ only in case. Use --case-ok to override this:"
    unless (null dups) $ do
       dupMsg <-
         case uniq_dups of
         [f] -> do
           isDir <- doesDirectoryReallyExist f
           if isDir
             then return $
               "The following directory " ++
               msgIs msgs ++ " already in the repository"
             else return $
               "The following file " ++
               msgIs msgs ++ " already in the repository"
         fs   -> do
           areDirs <- mapM doesDirectoryReallyExist fs
           if and areDirs
             then return $
               "The following directories " ++
               msgAre msgs ++ " already in the repository"
             else
               (if or areDirs
                  then return $
                    "The following files and directories " ++
                    msgAre msgs ++ " already in the repository"
                  else return $
                    "The following files " ++
                    msgAre msgs ++ " already in the repository")
       putWarning opts . text $ "WARNING: Some files were not added because they are already in the repository."
       putVerboseWarning opts . text $ dupMsg ++ caseMsg
       mapM_ (putVerboseWarning opts . text) uniq_dups
    return $ foldr (joinGap (+>+)) (emptyGap NilFL) ps
  where
    addp' :: Tree IO
          -> FilePath
          -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe FilePath)
    addp' cur f = do
      already_has <- (if gotAllowCaseOnly then treeHas else treeHasAnycase) cur f
      mstatus <- getFileStatus f
      case (already_has, is_badfilename, mstatus) of
        (True, _, _) -> return (cur, Nothing, Just f)
        (_, True, _) -> do
            putWarning opts . text $
              "The filename " ++ f ++ " is invalid under Windows.\n" ++
              "Use --reserved-ok to allow it."
            return add_failure
        (_, _, Just s)
            | isDirectory s    -> trypatch $ freeGap (adddir f :>: NilFL)
            | isRegularFile s  -> trypatch $ freeGap (addfile f :>: NilFL)
            | isSymbolicLink s -> do
                putWarning opts . text $
                    "Sorry, file " ++ f ++
                    " is a symbolic link, which is unsupported by darcs."
                return add_failure
        _ -> do
            putWarning opts . text $ "File "++ f ++" does not exist!"
            return add_failure
        where
          is_badfilename = not (gotAllowWindowsReserved || WindowsFilePath.isValid f)
          add_failure = (cur, Nothing, Nothing)
          trypatch :: FreeLeft (FL prim)
                   -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe FilePath)
          trypatch p = do
              perms <- getPermissions f
              if not $ readable perms
                then do
                    putWarning opts . text $
                        msgSkipping msgs ++ " '" ++ f ++ "': permission denied "
                    return (cur, Nothing, Nothing)
                else trypatch' p
          trypatch' p = do
              Sealed p' <- return $ unFreeLeft p
              ok <- treeHasDir cur parentdir
              if ok
                then do
                    tree <- applyToTree p' cur
                    putInfo opts . text $
                        msgAdding msgs ++ " '" ++ f ++ "'"
                    return (tree, Just p, Nothing)
                else do
                    putWarning opts . text $
                        msgSkipping msgs ++ " '" ++ f ++
                            "' ... couldn't add parent directory '" ++
                            parentdir ++ "' to repository"
                    return (cur, Nothing, Nothing)
              `catch` \(e :: IOException) -> do
                  putWarning opts . text $
                      msgSkipping msgs ++ " '" ++ f ++ "' ... " ++ show e
                  return (cur, Nothing, Nothing)
          parentdir = takeDirectory f
    gotAllowCaseOnly = doAllowCaseOnly opts
    gotAllowWindowsReserved = doAllowWindowsReserved opts


data AddMessages = AddMessages
    {
      msgSkipping  :: String
    , msgAdding    :: String
    , msgIs        :: String
    , msgAre       :: String
    }


normalMessages :: AddMessages
normalMessages = AddMessages
    {
      msgSkipping  = "Skipping"
    , msgAdding    = "Adding"
    , msgIs        = "is"
    , msgAre       = "are"
    }


dryRunMessages :: AddMessages
dryRunMessages = AddMessages
    {
      msgSkipping  = "Would skip"
    , msgAdding    = "Would add"
    , msgIs        = "would be"
    , msgAre       = "would be"
    }


doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = maybe False isDirectory `fmap` getFileStatus f


expandDirs :: Bool -> [SubPath]
           -> IO [SubPath]
expandDirs doIncludeBoring fs =
  do
    liftM (map (fromJust . simpleSubPath)) $
      concat `fmap` mapM (expandOne doIncludeBoring . toPath) fs


expandOne :: Bool -> FilePath
          -> IO [FilePath]
expandOne doIncludeBoring "" = listFiles doIncludeBoring
expandOne doIncludeBoring f = do
    isdir <- doesDirectoryReallyExist f
    if not isdir
      then return [f]
      else do
        fs <- withCurrentDirectory f (listFiles doIncludeBoring)
        return $ f: map (f </>) fs


getParents :: Tree IO
           -> [FilePath]
           -> [FilePath]
getParents cur = map (anchorPath "") . go . map floatPath
  where
    go fs = filter (isNothing . findTree cur) $ concatMap parents fs

