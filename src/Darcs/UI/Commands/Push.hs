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

{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.UI.Commands.Push ( push ) where

import Prelude hiding ( (^) )

import System.Exit ( exitWith, ExitCode( ExitSuccess, ExitFailure ), exitSuccess )
import Control.Monad ( when, unless )
import Data.Char ( toUpper )
import Data.Maybe ( isJust, isNothing )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putVerbose
    , putInfo
    , abortRun
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , formatPath
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Flags
    ( DarcsFlag
    , isInteractive, verbosity, isUnified, hasSummary, diffAlgorithm
    , hasXmlOutput, selectDeps, applyAs
    , doReverse, dryRun, useCache, remoteRepos, setDefault, fixUrl )
import Darcs.UI.Options
    ( DarcsOption, (^), odesc, ocheck, onormalise
    , defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( DryRun (..) )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Repository ( Repository, withRepository, RepoJob(..), identifyRepositoryFor,
                          readRepo, checkUnrelatedRepos )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), RL, FL, nullRL,
    nullFL, reverseFL, mapFL_FL, mapRL )
import Darcs.Repository.Prefs ( addRepoSource, getPreflist )
import Darcs.UI.External ( maybeURLCmd, signString )
import Darcs.Util.URL ( isHttpUrl, isValidLocalPath )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Patch.Bundle ( makeBundleN )
import Darcs.Patch.Patchy( ShowPatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Util.Printer ( Doc, vcat, empty, text, ($$) )
import Darcs.UI.RemoteApply ( remoteApply )
import Darcs.UI.Email ( makeEmail )
import Darcs.Util.English (englishNum, Noun(..))
import Darcs.Util.Workaround ( getCurrentDirectory )
import Storage.Hashed.Tree( Tree )
#include "impossible.h"


pushDescription :: String
pushDescription =
 "Copy and apply patches from this repository to another one."

pushHelp :: String
pushHelp = unlines
 [ "Push is the opposite of pull.  Push allows you to copy patches from the"
 , "current repository into another repository."
 , ""
 , "If you give the `--apply-as` flag, darcs will use sudo to apply the"
 , "patches as a different user.  This can be useful if you want to set up a"
 , "system where several users can modify the same repository, but you don't"
 , "want to allow them full write access.  This isn't secure against skilled"
 , "malicious attackers, but at least can protect your repository from clumsy,"
 , "inept or lazy users."
 , ""
 , "Darcs push will by default compress the patch data before sending it to a"
 , "remote location via ssh. This works as long as the remote darcs is not"
 , "older than version 2.5. If you get errors that indicate a corrupt patch"
 , "bundle, you should try again with the `--no-compress` option to send the"
 , "data in un-compressed form (which is a lot slower for large patches, but"
 , "should always work)."
 ]

pushBasicOpts :: DarcsOption a
                 ([O.MatchFlag]
                  -> O.SelectDeps
                  -> Maybe Bool
                  -> O.Sign
                  -> O.DryRun
                  -> O.XmlOutput
                  -> Maybe O.Summary
                  -> Maybe String
                  -> Maybe Bool
                  -> Bool
                  -> a)
pushBasicOpts
    = O.matchSeveral
    ^ O.selectDeps
    ^ O.interactive
    ^ O.sign
    ^ O.dryRunXml
    ^ O.summary
    ^ O.workingRepoDir
    ^ O.setDefault
    ^ O.allowUnrelatedRepos

pushAdvancedOpts :: DarcsOption a
                    (Maybe String -> O.RemoteRepos -> Bool -> O.Compression -> O.NetworkOptions -> a)
pushAdvancedOpts
    = O.applyAs
    ^ O.remoteRepos
    ^ O.changesReverse
    ^ O.compress
    ^ O.network

pushOpts :: DarcsOption a
            ([O.MatchFlag]
             -> O.SelectDeps
             -> Maybe Bool
             -> O.Sign
             -> DryRun
             -> O.XmlOutput
             -> Maybe O.Summary
             -> Maybe String
             -> Maybe Bool
             -> Bool
             -> Maybe O.StdCmdAction
             -> Bool
             -> Bool
             -> O.Verbosity
             -> Bool
             -> Maybe String
             -> O.RemoteRepos
             -> Bool
             -> O.Compression
             -> O.NetworkOptions
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
pushOpts = pushBasicOpts `withStdOpts` pushAdvancedOpts

push :: DarcsCommand [DarcsFlag]
push = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "push"
    , commandHelp = pushHelp
    , commandDescription = pushDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["[REPOSITORY]"]
    , commandCommand = pushCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = getPreflist "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc pushAdvancedOpts
    , commandBasicOptions = odesc pushBasicOpts
    , commandDefaults = defaultFlags pushOpts
    , commandCheckOptions = ocheck pushOpts
    , commandParseOptions = onormalise pushOpts
    }

pushCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
pushCmd _ _ [""] = impossible
pushCmd (_,o) opts [unfixedrepodir] =
 do
 repodir <- fixUrl o unfixedrepodir
 -- Test to make sure we aren't trying to push to the current repo
 here <- getCurrentDirectory
 checkOptionsSanity opts repodir
 when (repodir == here) $
       fail "Cannot push from repository to itself."
       -- absolute '.' also taken into account by fix_filepath
 bundle <- withRepository (useCache opts) $ RepoJob $
                          prepareBundle opts repodir
 sbundle <- signString (parseFlags O.sign opts) bundle
 let body = if isValidLocalPath repodir
            then sbundle
            else makeEmail repodir [] Nothing Nothing sbundle Nothing
 rval <- remoteApply opts repodir body
 case rval of ExitFailure ec -> do putStrLn "Apply failed!"
                                   exitWith (ExitFailure ec)
              ExitSuccess -> putInfo opts $ text "Push successful."
pushCmd _ _ _ = impossible

prepareBundle :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> String -> Repository p wR wU wT -> IO Doc
prepareBundle opts repodir repository = do
  old_default <- getPreflist "defaultrepo"
  when (old_default == [repodir]) $
       let pushing = if dryRun opts == YesDryRun then "Would push" else "Pushing"
       in  putInfo opts $ text $ pushing++" to "++formatPath repodir++"..."
  them <- identifyRepositoryFor repository (useCache opts) repodir >>= readRepo
  addRepoSource repodir (dryRun opts) (remoteRepos opts) (setDefault False opts)
  us <- readRepo repository
  common :> us' <- return $ findCommonWithThem us them
  prePushChatter opts us (reverseFL us') them
  let direction = if doReverse opts then FirstReversed else First
      context = selectionContext direction "push" (pushPatchSelOpts opts) Nothing Nothing
  runSelection (selectChanges us') context
                   >>= bundlePatches opts common

prePushChatter :: forall p a wX wY wT . (RepoPatch p, ShowPatch a) =>
                 [DarcsFlag] -> PatchSet p Origin wX ->
                 RL a wT wX -> PatchSet p Origin wY -> IO ()
prePushChatter opts us us' them = do
  checkUnrelatedRepos (parseFlags O.allowUnrelatedRepos opts) us them
  let num_to_pull = snd $ countUsThem us them
      pull_reminder = if num_to_pull > 0
                      then text $ "The remote repository has " ++ show num_to_pull
                      ++ " " ++ englishNum num_to_pull (Noun "patch") " to pull."
                      else empty
  putVerbose opts $ text "We have the following patches to push:" $$ vcat (mapRL description us')
  unless (nullRL us') $ putInfo opts pull_reminder
  when (nullRL us') $ do putInfo opts $ text "No recorded local patches to push!"
                         exitSuccess

bundlePatches :: forall t p wZ wW wA. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet p wA wZ
              -> (FL (PatchInfoAnd p) :> t) wZ wW
              -> IO Doc
bundlePatches opts common (to_be_pushed :> _) =
    do
      setEnvDarcsPatches to_be_pushed
      printDryRunMessageAndExit "push"
        (verbosity opts)
        (hasSummary O.NoSummary opts)
        (dryRun opts)
        (hasXmlOutput opts)
        (isInteractive True opts)
        to_be_pushed
      when (nullFL to_be_pushed) $ do
          putInfo opts $
            text "You don't want to push any patches, and that's fine with me!"
          exitSuccess
      makeBundleN Nothing common (mapFL_FL hopefully to_be_pushed)

checkOptionsSanity :: [DarcsFlag] -> String -> IO ()
checkOptionsSanity opts repodir =
  if isHttpUrl repodir then do
       when (isJust $ applyAs opts) $
           abortRun opts $ text "Cannot --apply-as when pushing to URLs"
       maybeapply <- maybeURLCmd "APPLY" repodir
       when (isNothing maybeapply) $
         let lprot = takeWhile (/= ':') repodir
             prot = map toUpper lprot
             msg = text ("Pushing to "++lprot++" URLs is not supported.\n"++
                         "You may be able to hack this to work"++
                         " using DARCS_APPLY_"++prot) in
         abortRun opts msg
   else when (parseFlags O.sign opts /= O.NoSign) $
        abortRun opts $ text "Signing doesn't make sense for local repositories or when pushing over ssh."


pushPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
pushPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps flags
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = isUnified flags
    }
