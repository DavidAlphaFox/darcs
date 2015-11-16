{-# LANGUAGE RankNTypes #-}
--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Move ( move, mv ) where

import Prelude hiding ( (^) )

import Control.Applicative ( (<$>) )
import Control.Monad ( when, unless, forM_, forM )
import Data.Maybe ( fromMaybe )
import Darcs.Util.SignalHandler ( withSignalsBlocked )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, commandAlias, amInHashedRepository )
import Darcs.UI.Flags ( DarcsFlag(Quiet)
    , doAllowCaseOnly, doAllowWindowsReserved, useCache, dryRun, umask
    , maybeFixSubPaths, fixSubPaths )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Flags ( UpdateWorking (..), DiffAlgorithm(..) )
import Darcs.Repository.Prefs ( filetypeFunction )
import System.FilePath.Posix ( (</>), takeFileName )
import System.Directory ( renameDirectory )
import Darcs.Repository.State ( readRecordedAndPending, readRecorded, updateIndex )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , addPendingDiffToPending
    , listFiles
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+) )
import Darcs.Patch.Witnesses.Sealed ( emptyGap, freeGap, joinGap, FreeLeft )
import Darcs.Util.Global ( debugMessage )
import qualified Darcs.Patch
import Darcs.Patch ( RepoPatch, PrimPatch )
import Darcs.Patch.Apply( ApplyState )
import Data.List ( nub, sort )
import qualified System.FilePath.Windows as WindowsFilePath

import Darcs.UI.Commands.Util.Tree ( treeHas, treeHasDir, treeHasAnycase, treeHasFile )
import Storage.Hashed.Tree( Tree, modifyTree )
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Util.Path
    ( floatPath
    , fp2fn
    , fn2fp
    , superName
    , SubPath()
    , toFilePath
    , AbsolutePath
    )
import Darcs.Util.Workaround ( renameFile )

moveDescription :: String
moveDescription = "Move or rename files."

moveHelp :: String
moveHelp =
 "Darcs cannot reliably distinguish between a file being deleted and a\n" ++
 "new one added, and a file being moved.  Therefore Darcs always assumes\n" ++
 "the former, and provides the `darcs mv` command to let Darcs know when\n" ++
 "you want the latter.  This command will also move the file in the\n" ++
 "working tree (unlike `darcs remove`), unless it has already been moved.\n" ++
 "\n" ++
 -- Note that this paragraph is very similar to one in ./Add.lhs.
 "Darcs will not rename a file if another file in the same folder has\n" ++
 "the same name, except for case.  The `--case-ok` option overrides this\n" ++
 "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
 "files a folder to have the same name except for case (for example,\n" ++
 "`ReadMe` and `README`).  If `--case-ok` is used, the repository might be\n" ++
 "unusable on those systems!\n"

moveBasicOpts :: DarcsOption a (Bool -> Bool -> Maybe String -> a)
moveBasicOpts = O.allowProblematicFilenames ^ O.workingRepoDir

moveAdvancedOpts :: DarcsOption a (O.UMask -> a)
moveAdvancedOpts = O.umask

moveOpts :: DarcsOption a
            (Bool
             -> Bool
             -> Maybe String
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
moveOpts = moveBasicOpts `withStdOpts` moveAdvancedOpts

move :: DarcsCommand [DarcsFlag]
move = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "move"
    , commandHelp = moveHelp
    , commandDescription = moveDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<SOURCE> ... <DESTINATION>"]
    , commandCommand = moveCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listFiles False
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc moveAdvancedOpts
    , commandBasicOptions = odesc moveBasicOpts
    , commandDefaults = defaultFlags moveOpts
    , commandCheckOptions = ocheck moveOpts
    , commandParseOptions = onormalise moveOpts
    }

moveCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
moveCmd fps opts args
  | length args < 2 =
      fail "The `darcs move' command requires at least two arguments."
  | length args == 2 = do
      xs <- maybeFixSubPaths fps args
      case xs of
        [Just from, Just to]
          | from == to -> fail "Cannot rename a file or directory onto itself!"
          | toFilePath from == "" -> fail "Cannot move the root of the repository"
          | otherwise -> moveFile opts from to
        _ -> fail "Both source and destination must be valid."
  | otherwise = let (froms, to) = (init args, last args) in do
      x <- head <$> maybeFixSubPaths fps [to]
      case x of
        Nothing -> fail "Invalid destination directory."
        Just to' -> do
          xs <- nub . sort <$> fixSubPaths fps froms
          if to' `elem` xs
            then fail "Cannot rename a file or directory onto itself!"
            else case xs of
              [] -> fail "Nothing to move."
              froms' -> moveFilesToDir opts froms' to'

data FileKind = Dir | File
              deriving (Show, Eq)

data FileStatus =
  Nonexistant
  | Unadded FileKind
  | Shadow FileKind -- ^ known to darcs, but absent in working copy
  | Known FileKind
  deriving Show

fileStatus :: Tree IO -- ^ tree of the working directory
           -> Tree IO -- ^ tree of recorded and pending changes
           -> Tree IO -- ^ tree of recorded changes
           -> FilePath
           -> IO FileStatus
fileStatus work cur recorded fp = do
  existsInCur <- treeHas cur fp
  existsInRec <- treeHas recorded fp
  existsInWork <- treeHas work fp
  case (existsInRec, existsInCur, existsInWork) of
    (_, True, True) -> do
      isDirCur <- treeHasDir cur fp
      isDirWork <- treeHasDir work fp
      unless (isDirCur == isDirWork) . fail $ "don't know what to do with " ++ fp
      return . Known $ if isDirCur then Dir else File

    (_, False, True) -> do
      isDir <- treeHasDir work fp
      if isDir
        then return $ Unadded Dir
        else return $ Unadded File
    (False, False, False) -> return Nonexistant
    (_, _, False) -> do
      isDir <- treeHasDir cur fp
      if isDir
        then return $ Shadow Dir
        else return $ Shadow File

-- | Takes two filenames (as 'Subpath'), and tries to move the first
-- into/onto the second. Needs to guess what that means: renaming or moving
-- into a directory, and whether it is a post-hoc move.
moveFile :: [DarcsFlag] -> SubPath -> SubPath -> IO ()
moveFile opts old new = withRepoAndState opts $ \(repo, work, cur, recorded) -> do
  let old_fp = toFilePath old
      new_fp = toFilePath new
  new_fs <- fileStatus work cur recorded new_fp
  old_fs <- fileStatus work cur recorded old_fp
  let doSimpleMove = simpleMove repo opts cur work old_fp new_fp
  case (old_fs, new_fs) of
    (Nonexistant, _) -> fail $ old_fp ++ " does not exist."
    (Unadded k, _) -> fail $ show k ++ " " ++ old_fp ++ " is unadded."
    (Known _, Nonexistant) -> doSimpleMove
    (Known _, Shadow _) -> doSimpleMove
    (_, Nonexistant) -> fail $ old_fp ++ " is not in the repository."
    (Known _, Known Dir) -> moveToDir repo opts cur work [old_fp] new_fp
    (Known _, Unadded Dir) -> fail $
        new_fp ++ " is not known to darcs; please add it to the repository."
    (Known _, _) -> fail $ new_fp ++ " already exists."
    (Shadow k, Unadded k') | k == k' -> doSimpleMove
    (Shadow File, Known Dir) -> moveToDir repo opts cur work [old_fp] new_fp
    (Shadow Dir, Known Dir) -> doSimpleMove
    (Shadow File, Known File) -> doSimpleMove
    (Shadow k, _) -> fail $
        "cannot move " ++ show k ++ " " ++ old_fp ++ " into " ++ new_fp
        ++ " : " ++ "did you already move it elsewhere?"

moveFilesToDir :: [DarcsFlag] -> [SubPath] -> SubPath -> IO ()
moveFilesToDir opts froms to = withRepoAndState opts $ \(repo, work, cur, _) ->
  moveToDir repo opts cur work (map toFilePath froms) $ toFilePath to

withRepoAndState :: [DarcsFlag]
                 -> (forall p wR wU .
                        (ApplyState p ~ Tree, RepoPatch p) =>
                            (Repository p wR wU wR, Tree IO, Tree IO, Tree IO)
                                -> IO ())
                 -> IO ()
withRepoAndState opts f =
    withRepoLock dr uc YesUpdateWorking um $ RepoJob $ \repo -> do
        work <- readPlainTree "."
        cur <- readRecordedAndPending repo
        recorded <- readRecorded repo
        f (repo, work, cur, recorded)
  where
    dr = dryRun opts
    uc = useCache opts
    um = umask opts

simpleMove :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
           -> [DarcsFlag] -> Tree IO -> Tree IO -> FilePath -> FilePath
           -> IO ()
simpleMove repository opts cur work old_fp new_fp = do
    doMoves repository opts cur work [(old_fp, new_fp)]
    unless (Quiet `elem` opts) $
        putStrLn $ unwords ["Moved:", old_fp, "to:", new_fp]

moveToDir :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
          -> [DarcsFlag] -> Tree IO -> Tree IO -> [FilePath] -> FilePath
          -> IO ()
moveToDir repository opts cur work moved finaldir = do
    let movetargets = map ((finaldir </>) . takeFileName) moved
        moves = zip moved movetargets
    doMoves repository opts cur work moves
    unless (Quiet `elem` opts) $
      putStrLn $ unwords $ ["Moved:"] ++ moved ++ ["to:", finaldir]

doMoves :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
          -> [DarcsFlag] -> Tree IO -> Tree IO
          -> [(FilePath, FilePath)] -> IO ()
doMoves repository opts cur work moves = do
  patches <- forM moves $ \(old, new) -> do
        prePatch <- generatePreMovePatches opts cur work (old,new)
        return (prePatch, old, new)
  withSignalsBlocked $ do
    forM_ patches $ \(prePatch, old, new) -> do
      let -- Add any pre patches before the move patch
          pendingDiff = joinGap (+>+)
            (fromMaybe (emptyGap NilFL) prePatch)
            (freeGap $ Darcs.Patch.move old new :>: NilFL)
      addPendingDiffToPending repository YesUpdateWorking pendingDiff
      moveFileOrDir work old new
    updateIndex repository

-- Take the recorded/ working trees and the old and intended new filenames;
-- check if the new path is safe on windows. We potentially need to create
-- extra patches that are required to keep the repository consistent, in order
-- to allow the move patch to be applied.
generatePreMovePatches :: PrimPatch prim => [DarcsFlag] -> Tree IO -> Tree IO
                       -> (FilePath, FilePath)
                       -> IO (Maybe (FreeLeft (FL prim)))
generatePreMovePatches opts cur work (old,new) = do
    -- Only allow Windows-invalid paths if we've been told to do so
    unless newIsOkWindowsPath $ fail newNotOkWindowsPathMsg
    -- Check if the first directory above the new path is in the repo (this
    -- is the new path if itself is a directory), handling the case where
    -- a user moves a file into a directory not known by darcs.
    let dirPath = fn2fp $ superName $ fp2fn new
    haveNewParent <- treeHasDir cur dirPath
    unless haveNewParent $
        fail $ "The target directory " ++ dirPath
                ++ " isn't known in the repository, did you forget to add it?"
    newInRecorded <- hasNew cur
    newInWorking <- hasNew work
    oldInWorking <- treeHas work old
    if oldInWorking -- We need to move the object
        then do
            -- We can't move if the target already exists in working
            when newInWorking $ fail $ alreadyExists "working directory"
            if newInRecorded
                then Just <$> deleteNewFromRepoPatches
                else return Nothing
        else do
          unless (Quiet `elem` opts) $
              putStrLn "Detected post-hoc move."
          -- Post-hoc move - user has moved/deleted the file in working, so
          -- we can hopefully make a move patch to make the repository
          -- consistent.
          -- If we don't have the old or new in working, we're stuck
          unless newInWorking $
              fail $ "Cannot determine post-hoc move target, "
                     ++ "no file/dir named:\n" ++ new
          Just <$> if newInRecorded
                       then deleteNewFromRepoPatches
                       else return $ emptyGap NilFL
  where
    newIsOkWindowsPath =
        doAllowWindowsReserved opts || WindowsFilePath.isValid new

    newNotOkWindowsPathMsg =
        "The filename " ++ new ++ " is not valid under Windows.\n"
        ++ "Use --reserved-ok to allow such filenames."

    -- If we're moving to a file/dir that was recorded, but has been deleted,
    -- we need to add patches to pending that remove the original.
    deleteNewFromRepoPatches = do
        unless (Quiet `elem` opts) $
            putStrLn $ "Existing recorded contents of " ++ new
                       ++ " will be overwritten."
        ftf <- filetypeFunction
        let curNoNew = modifyTree cur (floatPath new) Nothing
        -- Return patches to remove new, so that the move patch
        -- can move onto new
        treeDiff MyersDiff ftf cur curNoNew

    -- Check if the passed tree has the new filepath. The old path is removed
    -- from the tree before checking if the new path is present.
    hasNew s = treeHas_case (modifyTree s (floatPath old) Nothing) new
    treeHas_case = if doAllowCaseOnly opts then treeHas else treeHasAnycase

    alreadyExists inWhat =
        if doAllowCaseOnly opts
            then "A file or dir named "++new++" already exists in "
                  ++ inWhat ++ "."
            else "A file or dir named "++new++" (or perhaps differing "
                 ++ "only in case)\nalready exists in "++ inWhat ++ ".\n"
                 ++ "Use --case-ok to allow files differing only in case."

moveFileOrDir :: Tree IO -> FilePath -> FilePath -> IO ()
moveFileOrDir work old new = do
  has_file <- treeHasFile work old
  has_dir <- treeHasDir work old
  when has_file $ do debugMessage $ unwords ["renameFile",old,new]
                     renameFile old new
  when has_dir $ do debugMessage $ unwords ["renameDirectory",old,new]
                    renameDirectory old new

mv :: DarcsCommand [DarcsFlag]
mv = commandAlias "mv" Nothing move

