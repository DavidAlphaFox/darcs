--  Copyright (C) 2003-2005 David Roundy
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

{-# LANGUAGE CPP, GADTs #-}


module Darcs.UI.Commands.Unrevert ( unrevert, writeUnrevert ) where

import Prelude hiding ( (^), catch )

import Control.Exception ( catch, IOException )
import System.Exit ( exitSuccess )
import Storage.Hashed.Tree( Tree )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Flags
    ( diffingOpts, verbosity, useCache, umask, compression, diffAlgorithm
    , isInteractive, isUnified )
import Darcs.Repository.Flags
    ( UseIndex(..), ScanKnown (..), Reorder(..), AllowConflicts(..), ExternalMerge(..)
    , WantGuiPause(..), UpdateWorking(..), DryRun(NoDryRun) )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository ( SealedPatchSet, Repository, withRepoLock, RepoJob(..),
                          unrevertUrl, considerMergeToWorking,
                          tentativelyAddToPending, finalizeRepositoryChanges,
                          readRepo,
                          readRecorded,
                          applyToWorking, unrecordedChanges )
import Darcs.Patch ( RepoPatch, PrimOf, commute, namepatch, fromPrims )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), (+>+) )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(First)
    , runSelection
    , selectionContextPrim
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import qualified Data.ByteString as B
import Darcs.Repository.Lock ( writeDocBinFile, removeFileMayNotExist )
import Darcs.Patch.Depends ( mergeThem )
import Darcs.UI.External ( catchall )
import Darcs.Util.Prompt ( askUser )
import Darcs.Patch.Bundle ( scanBundle, makeBundleN )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )
#include "impossible.h"

unrevertDescription :: String
unrevertDescription =
 "Undo the last revert (may fail if changes after the revert)."

unrevertHelp :: String
unrevertHelp =
 "Unrevert is a rescue command in case you accidentally reverted\n" ++
 "something you wanted to keep (for example, typing `darcs rev -a`\n" ++
 "instead of `darcs rec -a`).\n" ++
 "\n" ++
 "This command may fail if the repository has changed since the revert\n" ++
 "took place.  Darcs will ask for confirmation before executing an\n" ++
 "interactive command that will DEFINITELY prevent unreversion.\n"

unrevertBasicOpts :: DarcsOption a
                     (O.UseIndex
                      -> Maybe Bool
                      -> Maybe String
                      -> O.WithContext
                      -> O.DiffAlgorithm
                      -> a)
unrevertBasicOpts
    = O.useIndex
    ^ O.interactive -- True
    ^ O.workingRepoDir
    ^ O.withContext
    ^ O.diffAlgorithm

unrevertAdvancedOpts :: DarcsOption a (O.UMask -> a)
unrevertAdvancedOpts = O.umask

unrevertOpts :: DarcsOption a
                (UseIndex
                 -> Maybe Bool
                 -> Maybe String
                 -> O.WithContext
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
unrevertOpts = unrevertBasicOpts `withStdOpts` unrevertAdvancedOpts

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

unrevert :: DarcsCommand [DarcsFlag]
unrevert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unrevert"
    , commandHelp = unrevertHelp
    , commandDescription = unrevertDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unrevertCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc unrevertAdvancedOpts
    , commandBasicOptions = odesc unrevertBasicOpts
    , commandDefaults = defaultFlags unrevertOpts
    , commandCheckOptions = ocheck unrevertOpts
    , commandParseOptions = onormalise unrevertOpts
    }

unrevertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unrevertCmd _ opts [] =
 withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
  us <- readRepo repository
  Sealed them <- unrevertPatchBundle repository
  recorded <- readRecorded repository
  unrecorded <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -}) repository Nothing
  Sealed h_them <- return $ mergeThem us them
  Sealed pw <- considerMergeToWorking repository "unrevert"
                      YesAllowConflictsAndMark YesUpdateWorking
                      NoExternalMerge NoWantGuiPause
                      (compression opts) (verbosity opts) NoReorder
                      ( UseIndex, ScanKnown, diffAlgorithm opts )
                      NilFL h_them
  let context = selectionContextPrim First "unrevert" (patchSelOpts opts) Nothing Nothing (Just recorded)
  (p :> skipped) <- runSelection (selectChanges pw) context
  tentativelyAddToPending repository YesUpdateWorking p
  withSignalsBlocked $
      do finalizeRepositoryChanges repository YesUpdateWorking (compression opts)
         _ <- applyToWorking repository (verbosity opts) p `catch` \(e :: IOException) ->
             fail ("Error applying unrevert to working directory...\n"
                   ++ show e)
         debugMessage "I'm about to writeUnrevert."
         writeUnrevert repository skipped recorded (unrecorded+>+p)
  debugMessage "Finished unreverting."
unrevertCmd _ _ _ = impossible

writeUnrevert :: (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT -> FL (PrimOf p) wX wY
              -> Tree IO -> FL (PrimOf p) wR wX -> IO ()
writeUnrevert repository NilFL _ _ = removeFileMayNotExist $ unrevertUrl repository
writeUnrevert repository ps recorded pend =
  case commute (pend :> ps) of
    Nothing -> do really <- askUser "You will not be able to unrevert this operation! Proceed? "
                  case really of ('y':_) -> return ()
                                 _ -> exitSuccess
                  writeUnrevert repository NilFL recorded pend
    Just (p' :> _) -> do
        rep <- readRepo repository
        date <- getIsoDateTime
        np <- namepatch date "unrevert" "anon" [] (fromRepoPrims repository p')
        bundle <- makeBundleN (Just recorded) rep (np :>: NilFL)
        writeDocBinFile (unrevertUrl repository) bundle
        where fromRepoPrims :: RepoPatch p => Repository p wR wU wT -> FL (PrimOf p) wR wY -> FL p wR wY
              fromRepoPrims _ = fromPrims

unrevertPatchBundle :: RepoPatch p => Repository p wR wU wT -> IO (SealedPatchSet p Origin)
unrevertPatchBundle repository = do
  pf <- B.readFile (unrevertUrl repository)
        `catchall` fail "There's nothing to unrevert!"
  case scanBundle pf of
      Right ps -> return ps
      Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err

