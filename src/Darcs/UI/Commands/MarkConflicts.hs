--  Copyright (C) 2002-2003,2005 David Roundy
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

module Darcs.UI.Commands.MarkConflicts ( markconflicts ) where

import Prelude hiding ( (^), catch )

import System.Exit ( exitSuccess )
import Data.List.Ordered ( nubSort )
import Control.Monad ( when, unless )
import Control.Exception ( catch, IOException )

import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Printer( putDocLn, putDocLnWith, text, redText, ($$) )
import Darcs.Util.Printer.Color (fancyPrinters)

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Flags ( DarcsFlag, diffingOpts, verbosity, dryRun, umask, useCache )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking (..) )
import Darcs.Repository ( withRepoLock, RepoJob(..), addToPending,
                    applyToWorking,
                    readRepo, unrecordedChanges, Repository
                    )
import Darcs.Patch ( invert, PrimOf, listTouchedFiles )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository.Resolution ( patchsetConflictResolutions )
#include "impossible.h"

markconflictsDescription :: String
markconflictsDescription =
 "Mark unresolved conflicts in working tree, for manual resolution."

markconflictsHelp :: String
markconflictsHelp = unlines
 ["Darcs requires human guidance to unify changes to the same part of a"
 ,"source file.  When a conflict first occurs, darcs will add the"
 ,"initial state and both choices to the working tree, delimited by the"
 ,"markers `v v v`, `=====`,  `* * *` and `^ ^ ^`, as follows:"
 ,""
 ,"    v v v v v v v"
 ,"    Initial state."
 ,"    ============="
 ,"    First choice."
 ,"    *************"
 ,"    Second choice."
 ,"    ^ ^ ^ ^ ^ ^ ^"
 ,""
 ,"However, you might revert or manually delete these markers without"
 ,"actually resolving the conflict.  In this case, `darcs mark-conflicts`"
 ,"is useful to show where are the unresolved conflicts.  It is also"
 ,"useful if `darcs apply` is called with `--apply-conflicts`,"
 ,"where conflicts aren't marked initially."
 ,""
 ,"Unless you use the `--dry-run` flag, any unrecorded changes to the"
 ,"working tree WILL be lost forever when you run this command!"
 ,"You will be prompted for confirmation before this takes place."
 ]

markconflictsBasicOpts :: DarcsOption a
                          (O.UseIndex
                           -> Maybe String
                           -> O.DiffAlgorithm
                           -> O.DryRun
                           -> O.XmlOutput
                           -> a)
markconflictsBasicOpts
    = O.useIndex
    ^ O.workingRepoDir
    ^ O.diffAlgorithm
    ^ O.dryRunXml

markconflictsAdvancedOpts :: DarcsOption a (O.UMask -> a)
markconflictsAdvancedOpts = O.umask

markconflictsOpts :: DarcsOption a
                     (O.UseIndex
                      -> Maybe String
                      -> O.DiffAlgorithm
                      -> O.DryRun
                      -> O.XmlOutput
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
markconflictsOpts = markconflictsBasicOpts `withStdOpts` markconflictsAdvancedOpts

markconflicts :: DarcsCommand [DarcsFlag]
markconflicts = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "mark-conflicts"
    , commandHelp = markconflictsHelp
    , commandDescription = markconflictsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = markconflictsCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc markconflictsAdvancedOpts
    , commandBasicOptions = odesc markconflictsBasicOpts
    , commandDefaults = defaultFlags markconflictsOpts
    , commandCheckOptions = ocheck markconflictsOpts
    , commandParseOptions = onormalise markconflictsOpts
    }

markconflictsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
markconflictsCmd _ opts [] = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \(repository :: Repository p wR wU wR) -> do
  pend <- unrecordedChanges (diffingOpts opts) repository Nothing
  r <- readRepo repository
  Sealed res <- return $ patchsetConflictResolutions r
  case nubSort $ listTouchedFiles res of
    []  -> putStrLn "No conflicts to mark." >> exitSuccess
    cfs -> putDocLnWith fancyPrinters $
              redText "Conflicts found in the following files:" $$ text (unlines cfs)
  when (dryRun opts == O.YesDryRun) $ do
      putDocLn $ text "Conflicts will not be marked: this is a dry run."
      exitSuccess
  let undoUnrec :: FL (PrimOf p) wR wU -> IO (Repository p wR wR wR)
      undoUnrec NilFL = return repository
      undoUnrec pend' =
              do putStrLn ("This will trash any unrecorded changes"++
                          " in the working directory.")
                 confirmed <- promptYorn "Are you sure? "
                 unless confirmed exitSuccess
                 applyToWorking repository (verbosity opts) (invert pend') `catch` \(e :: IOException) ->
                    bug ("Can't undo pending changes!" ++ show e)
  repository' <- undoUnrec pend
  withSignalsBlocked $
    do addToPending repository' YesUpdateWorking res
       _ <- applyToWorking repository' (verbosity opts) res `catch` \(e :: IOException) ->
           bug ("Problem marking conflicts in mark-conflicts!" ++ show e)
       return ()
  putStrLn "Finished marking conflicts."
markconflictsCmd _ _ _ = impossible
