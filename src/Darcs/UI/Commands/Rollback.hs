--  Copyright (C) 2002-2004,2007 David Roundy
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

module Darcs.UI.Commands.Rollback ( rollback ) where

import Prelude hiding ( (^), catch )

import Control.Applicative ( (<$>) )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )
import Data.List ( sort )
import Storage.Hashed.Tree( Tree )
import System.Exit ( exitSuccess )

import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Match ( firstMatch )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch ( RepoPatch, invert, effect, fromPrims, sortCoalesceFL,
                     canonize, anonymous, PrimOf )
import Darcs.Patch.Set ( PatchSet(..), newset2FL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), RL(..), concatFL,
                                       nullFL, mapFL_FL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Repository.Flags ( AllowConflicts (..), UseIndex(..), Reorder(..),
                                ScanKnown(..), UpdateWorking(..), DryRun(NoDryRun))
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..),
                          applyToWorking, readRepo,
                          finalizeRepositoryChanges, tentativelyAddToPending,
                          considerMergeToWorking, listRegisteredFiles )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, setEnvDarcsPatches,
                           amInHashedRepository )
import Darcs.UI.Commands.Unrecord ( getLastPatches )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Flags as F ( DarcsFlag(Quiet), verbosity, umask, useCache,
                        compression, externalMerge, wantGuiPause,
                        diffAlgorithm, fixSubPaths, isInteractive )
import Darcs.UI.Options
    ( DarcsOption, (^), odesc, ocheck, onormalise
    , defaultFlags, parseFlags
    )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.SelectChanges ( selectChanges, WhichChanges(..),
                                selectionContext, selectionContextPrim,
                                runSelection )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Path ( toFilePath, AbsolutePath )
import Darcs.Util.Progress ( debugMessage )

rollbackDescription :: String
rollbackDescription =
 "Apply the inverse of recorded changes to the working copy."

rollbackHelp :: String
rollbackHelp = unlines
    [ "Rollback is used to undo the effects of some changes from patches"
    , "in the repository. The selected changes are undone in your working"
    , "copy, but the repository is left unchanged. First you are offered a"
    , "choice of which patches to undo, then which changes within the"
    , "patches to undo."
    , ""
    , "Before doing `rollback`, you may want to temporarily undo the changes"
    , "of your working copy (if there are) and save them for later use."
    , "To do so, you can run `revert`, then run `rollback`, record a patch,"
    , "and run `unrevert` to restore the saved changes into your working copy."
    ]

rollbackBasicOpts :: DarcsOption a
                     ([O.MatchFlag]
                      -> Maybe Bool -> Maybe String -> O.DiffAlgorithm -> a)
rollbackBasicOpts
    = O.matchSeveralOrLast
    ^ O.interactive -- True
    ^ O.workingRepoDir
    ^ O.diffAlgorithm

rollbackAdvancedOpts :: DarcsOption a (O.UMask -> a)
rollbackAdvancedOpts = O.umask

rollbackOpts :: DarcsOption a
                ([O.MatchFlag]
                 -> Maybe Bool
                 -> Maybe String
                 -> O.DiffAlgorithm
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
rollbackOpts = rollbackBasicOpts `withStdOpts` rollbackAdvancedOpts

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveralOrLast flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps
    , S.summary = O.NoSummary
    , S.withContext = O.NoContext
    }

rollback :: DarcsCommand [DarcsFlag]
rollback = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "rollback"
    , commandHelp = rollbackHelp
    , commandDescription = rollbackDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = rollbackCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc rollbackAdvancedOpts
    , commandBasicOptions = odesc rollbackBasicOpts
    , commandDefaults = defaultFlags rollbackOpts
    , commandCheckOptions = ocheck rollbackOpts
    , commandParseOptions = onormalise rollbackOpts
    }

exitIfNothingSelected :: FL p wX wY -> String -> IO ()
exitIfNothingSelected ps what =
    when (nullFL ps) $ putStrLn ("No " ++ what ++ " selected!") >> exitSuccess

rollbackCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
rollbackCmd fps opts args = withRepoLock NoDryRun (useCache opts)
    YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
        files <- if null args
                     then return Nothing
                     else Just . sort <$> fixSubPaths fps args
        when (files == Just []) $
            fail "No valid arguments were given."
        announceFiles files "Rolling back changes in"
        allpatches <- readRepo repository
        let matchFlags = parseFlags O.matchSeveralOrLast opts
        (_ :> patches) <- return $
            if firstMatch matchFlags
                then getLastPatches matchFlags allpatches
                else PatchSet NilRL NilRL :> newset2FL allpatches
        let filesFps = map toFilePath <$> files
            patchCtx = selectionContext LastReversed "rollback" (patchSelOpts opts) Nothing filesFps
        (_ :> ps) <-
            runSelection (selectChanges patches) patchCtx
        exitIfNothingSelected ps "patches"
        setEnvDarcsPatches ps
        let hunkContext = selectionContextPrim Last "rollback" (patchSelOpts opts)
                              (Just reversePrimSplitter) filesFps Nothing
            hunks = concatFL . mapFL_FL (canonize $ F.diffAlgorithm opts) . sortCoalesceFL . effect $ ps
        whatToUndo <- runSelection (selectChanges hunks) hunkContext
        undoItNow opts repository whatToUndo

undoItNow :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
          => [DarcsFlag] -> Repository p wR wU wT
          -> (q :> FL (PrimOf p)) wA wT -> IO ()
undoItNow opts repo (_ :> prims) = do
    exitIfNothingSelected prims "changes"
    rbp <- n2pia `fmap` anonymous (fromPrims $ invert prims)
    Sealed pw <- considerMergeToWorking repo "rollback"
                     YesAllowConflictsAndMark YesUpdateWorking
                     (externalMerge opts) (wantGuiPause opts)
                     (compression opts) (verbosity opts) NoReorder
                     (UseIndex, ScanKnown, F.diffAlgorithm opts)
                     NilFL (rbp :>: NilFL)
    tentativelyAddToPending repo YesUpdateWorking pw
    finalizeRepositoryChanges repo YesUpdateWorking
        (compression opts)
    _ <- applyToWorking repo (verbosity opts) pw
            `catch`
            \(e :: IOException) -> fail $
                "error applying rolled back patch to working directory\n"
                ++ show e
    debugMessage "Finished applying unrecorded rollback patch"
    when (F.Quiet `notElem` opts) $
        putStrLn "Changes rolled back in working directory"

