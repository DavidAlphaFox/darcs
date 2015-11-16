--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.UI.Commands.Revert ( revert ) where

import Prelude hiding ( (^), catch )

import Control.Applicative ( (<$>) )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )
import Data.List ( sort )

import Darcs.UI.Flags
    ( DarcsFlag( Debug ), diffingOpts, verbosity, diffAlgorithm, isInteractive, isUnified
    , dryRun, umask, useCache, fixSubPaths )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Commands.Unrevert ( writeUnrevert )
import Darcs.Util.Path ( toFilePath, AbsolutePath )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , applyToWorking
    , readRecorded
    , unrecordedChanges
    , listRegisteredFiles
    )
import Darcs.Patch ( invert, effectOnFilePaths, commute )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), nullFL, (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(Last)
    , selectionContextPrim
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Patch.TouchesFiles ( chooseTouching )


revertDescription :: String
revertDescription = "Discard unrecorded changes."

revertHelp :: String
revertHelp =
 "The `darcs revert` command discards unrecorded changes the working\n" ++
 "tree.  As with `darcs record`, you will be asked which hunks (changes)\n" ++
 "to revert.  The `--all` switch can be used to avoid such prompting. If\n" ++
 "files or directories are specified, other parts of the working tree\n" ++
 "are not reverted.\n" ++
 "\n" ++
 "In you accidentally reverted something you wanted to keep (for\n" ++
 "example, typing `darcs rev -a` instead of `darcs rec -a`), you can\n" ++
 "immediately run `darcs unrevert` to restore it.  This is only\n" ++
 "guaranteed to work if the repository has not changed since `darcs\n" ++
 "revert` ran.\n"

revertBasicOpts :: DarcsOption a
                   (Maybe Bool -> Maybe String -> O.WithContext -> O.DiffAlgorithm -> a)
revertBasicOpts
    = O.interactive -- True
    ^ O.workingRepoDir
    ^ O.withContext
    ^ O.diffAlgorithm

revertAdvancedOpts :: DarcsOption a (O.UseIndex -> O.UMask -> a)
revertAdvancedOpts = O.useIndex ^ O.umask

revertOpts :: DarcsOption a
              (Maybe Bool
               -> Maybe String
               -> O.WithContext
               -> O.DiffAlgorithm
               -> Maybe O.StdCmdAction
               -> Bool
               -> Bool
               -> O.Verbosity
               -> Bool
               -> O.UseIndex
               -> O.UMask
               -> O.UseCache
               -> Maybe String
               -> Bool
               -> Maybe String
               -> Bool
               -> a)
revertOpts = revertBasicOpts `withStdOpts` revertAdvancedOpts

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = []
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = O.NoSummary -- option not supported, use default
    , S.withContext = isUnified flags
    }

revert :: DarcsCommand [DarcsFlag]
revert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "revert"
    , commandHelp = revertHelp
    , commandDescription = revertDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = revertCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc revertAdvancedOpts
    , commandBasicOptions = odesc revertBasicOpts
    , commandDefaults = defaultFlags revertOpts
    , commandCheckOptions = ocheck revertOpts
    , commandParseOptions = onormalise revertOpts
    }

revertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
revertCmd fps opts args =
 withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  files <- if null args then return Nothing
    else Just . sort <$> fixSubPaths fps args
  announceFiles files "Reverting changes in"
  changes <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -}) repository files
  let pre_changed_files = effectOnFilePaths (invert changes) . map toFilePath <$> files
  recorded <- readRecorded repository
  Sealed touching_changes <- return (chooseTouching pre_changed_files changes)
  (case touching_changes of
    NilFL -> putStrLn "There are no changes to revert!"
    _ -> do
      let context = selectionContextPrim
                          Last "revert" (patchSelOpts opts)
                          (Just reversePrimSplitter) pre_changed_files (Just recorded)
      (norevert:>p) <- runSelection (selectChanges changes) context
      if nullFL p
       then putStrLn $ "If you don't want to revert after all," ++
                        " that's fine with me!"
       else do
                 addToPending repository YesUpdateWorking $ invert p
                 when (Debug `elem` opts) $ putStrLn "About to write the unrevert file."
                 case commute (norevert:>p) of
                   Just (p':>_) -> writeUnrevert repository p' recorded NilFL
                   Nothing -> writeUnrevert repository (norevert+>+p) recorded NilFL
                 when (Debug `elem` opts) $ putStrLn "About to apply to the working directory."
                 _ <- applyToWorking repository (verbosity opts) (invert p) `catch` \(e :: IOException) ->
                     fail ("Unable to apply inverse patch!" ++ show e)
                 return ()) :: IO ()
  putStrLn "Finished reverting."

