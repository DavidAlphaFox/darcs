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

{-# LANGUAGE CPP #-}

module Darcs.UI.Commands.Pull ( -- * Commands.
                                pull, fetch,
                                pullCmd, StandardPatchApplier,
                                -- * Utility functions.
                                fetchPatches, revertable
                              ) where

import Prelude hiding ( (^) )

import System.Exit ( exitSuccess )
import Control.Monad ( when, unless, (>=>) )
import Data.List ( nub )
import Data.Maybe ( fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putInfo
    , setEnvDarcsPatches
    , formatPath
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Flags
    ( DarcsFlag
        ( AllowConflicts
        , Complement
        , DryRun
        , Intersection
        , MarkConflicts
        , NoAllowConflicts
        , SkipConflicts
        , Verbose
        , XMLOutput
        , Quiet
        , AllowUnrelatedRepos
        )
    , fixUrl, getOutput
    , doReverse, verbosity,  dryRun, umask, useCache, selectDeps
    , remoteRepos, reorder, setDefault
    , isUnified, hasSummary
    , diffAlgorithm, isInteractive )
import Darcs.UI.Options
    ( DarcsOption, (^), odesc, ocheck, onormalise
    , defaultFlags, parseFlags
    )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking (..) )
import Darcs.Repository
    ( Repository
    , identifyRepositoryFor
    , withRepoLock
    , RepoJob(..)
    , readRepo
    , checkUnrelatedRepos
    , modifyCache
    , modifyCache
    , Cache(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , filterOutConflicts
    )

import qualified Darcs.Repository.Cache as DarcsCache
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, hopefully, patchDesc )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Bundle( makeBundleN, patchFilename )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Set ( Origin, PatchSet(..), SealedPatchSet )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), (:\/:)(..), FL(..), RL(..)
    , mapFL, nullFL, reverseFL, mapFL_FL )
import Darcs.Patch.Permutations ( partitionFL )
import Darcs.Repository.Prefs ( addToPreflist, addRepoSource, getPreflist )
import Darcs.Repository.Motd (showMotd )
import Darcs.Patch.Depends ( findUncommon, findCommonWithThem,
                             newsetIntersection, newsetUnion )
import Darcs.UI.ApplyPatches ( PatchApplier(..), StandardPatchApplier(..) )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , runSelection
    , selectionContext
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Printer ( putDocLn, vcat, ($$), text, putDoc )
import Darcs.Repository.Lock ( writeDocBinFile )
import Darcs.Util.Path ( useAbsoluteOrStd, stdOut, AbsolutePath )
import Darcs.Util.Workaround ( getCurrentDirectory )
import Storage.Hashed.Tree( Tree )
#include "impossible.h"


pullDescription :: String
pullDescription =
 "Copy and apply patches from another repository to this one."

fetchDescription :: String
fetchDescription =
 "Fetch patches from another repository, but don't apply them."

pullHelp :: String
pullHelp = unlines
 [ "Pull is used to bring patches made in another repository into the current"
 , "repository (that is, either the one in the current directory, or the one"
 , "specified with the `--repodir` option). Pull allows you to bring over all or"
 , "some of the patches that are in that repository but not in this one. Pull"
 , "accepts arguments, which are URLs from which to pull, and when called"
 , "without an argument, pull will use the repository from which you have most"
 , "recently either pushed or pulled."
 , ""
 , "The default (`--union`) behavior is to pull any patches that are in any of"
 , "the specified repositories.  If you specify the `--intersection` flag, darcs"
 , "will only pull those patches which are present in all source repositories."
 , "If you specify the `--complement` flag, darcs will only pull elements in the"
 , "first repository that do not exist in any of the remaining repositories."
 , ""
 , "If `--reorder` is supplied, the set of patches that exist only in the current"
 , "repository is brought at the top of the current history. This will work even"
 , "if there are no new patches to pull."
 , ""
 , "See `darcs help apply` for detailed description of many options."
 ]

fetchHelp :: String
fetchHelp = unlines
 [ "Fetch is similar to `pull` except that it does not apply any patches"
 , "to the current repository. Instead, it generates a patch bundle that"
 , "you can apply later with `apply`."
 , ""
 , "Fetch's behaviour is essentially similar to pull's, so please consult"
 , "the help of `pull` to know more."
 ]

pullBasicOpts :: DarcsOption a
                 ([O.MatchFlag]
                  -> O.Reorder
                  -> Maybe Bool
                  -> Maybe O.AllowConflicts
                  -> O.ExternalMerge
                  -> O.RunTest
                  -> O.DryRun
                  -> O.XmlOutput
                  -> Maybe O.Summary
                  -> O.SelectDeps
                  -> Maybe Bool
                  -> Maybe String
                  -> Bool
                  -> O.DiffAlgorithm
                  -> a)
pullBasicOpts
    = O.matchSeveral
    ^ O.reorder
    ^ O.interactive -- True
    ^ O.conflicts O.YesAllowConflictsAndMark
    ^ O.useExternalMerge
    ^ O.test
    ^ O.dryRunXml
    ^ O.summary
    ^ O.selectDeps
    ^ O.setDefault
    ^ O.workingRepoDir
    ^ O.allowUnrelatedRepos
    ^ O.diffAlgorithm

pullAdvancedOpts :: DarcsOption a
                    (O.RepoCombinator
                     -> O.Compression
                     -> O.UseIndex
                     -> O.RemoteRepos
                     -> O.SetScriptsExecutable
                     -> O.UMask
                     -> Bool
                     -> Bool
                     -> O.WantGuiPause
                     -> O.NetworkOptions
                     -> a)
pullAdvancedOpts
    = O.repoCombinator
    ^ O.compress
    ^ O.useIndex
    ^ O.remoteRepos
    ^ O.setScriptsExecutable
    ^ O.umask
    ^ O.restrictPaths
    ^ O.changesReverse
    ^ O.pauseForGui
    ^ O.network

pullOpts :: DarcsOption a
            ([O.MatchFlag]
             -> O.Reorder
             -> Maybe Bool
             -> Maybe O.AllowConflicts
             -> O.ExternalMerge
             -> O.RunTest
             -> O.DryRun
             -> O.XmlOutput
             -> Maybe O.Summary
             -> O.SelectDeps
             -> Maybe Bool
             -> Maybe String
             -> Bool
             -> O.DiffAlgorithm
             -> Maybe O.StdCmdAction
             -> Bool
             -> Bool
             -> O.Verbosity
             -> Bool
             -> O.RepoCombinator
             -> O.Compression
             -> O.UseIndex
             -> O.RemoteRepos
             -> O.SetScriptsExecutable
             -> O.UMask
             -> Bool
             -> Bool
             -> O.WantGuiPause
             -> O.NetworkOptions
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
pullOpts = pullBasicOpts `withStdOpts` pullAdvancedOpts

fetchBasicOpts :: DarcsOption a
                  ([O.MatchFlag]
                   -> Maybe Bool
                   -> O.DryRun
                   -> Maybe O.Summary
                   -> O.SelectDeps
                   -> Maybe Bool
                   -> Maybe String
                   -> Maybe O.Output
                   -> Bool
                   -> O.DiffAlgorithm
                   -> a)
fetchBasicOpts
    = O.matchSeveral
    ^ O.interactive -- True
    ^ O.dryRun
    ^ O.summary
    ^ O.selectDeps
    ^ O.setDefault
    ^ O.workingRepoDir
    ^ O.output
    ^ O.allowUnrelatedRepos
    ^ O.diffAlgorithm

fetchAdvancedOpts :: DarcsOption a
                     (O.RepoCombinator -> O.RemoteRepos -> O.NetworkOptions -> a)
fetchAdvancedOpts
    = O.repoCombinator
    ^ O.remoteRepos
    ^ O.network

fetchOpts :: DarcsOption a
             ([O.MatchFlag]
              -> Maybe Bool
              -> O.DryRun
              -> Maybe O.Summary
              -> O.SelectDeps
              -> Maybe Bool
              -> Maybe String
              -> Maybe O.Output
              -> Bool
              -> O.DiffAlgorithm
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> O.RepoCombinator
              -> O.RemoteRepos
              -> O.NetworkOptions
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)
fetchOpts = fetchBasicOpts `withStdOpts` fetchAdvancedOpts

fetch :: DarcsCommand [DarcsFlag]
fetch = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "fetch"
    , commandHelp = fetchHelp
    , commandDescription = fetchDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = fetchCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = getPreflist "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc fetchAdvancedOpts
    , commandBasicOptions = odesc fetchBasicOpts
    , commandDefaults = defaultFlags fetchOpts
    , commandCheckOptions = ocheck fetchOpts
    , commandParseOptions = onormalise fetchOpts
    }

pull :: DarcsCommand [DarcsFlag]
pull = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "pull"
    , commandHelp = pullHelp
    , commandDescription = pullDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = pullCmd StandardPatchApplier
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = getPreflist "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc pullAdvancedOpts
    , commandBasicOptions = odesc pullBasicOpts
    , commandDefaults = defaultFlags pullOpts
    , commandCheckOptions = ocheck pullOpts
    , commandParseOptions = onormalise pullOpts
    }

mergeOpts :: [DarcsFlag] -> [DarcsFlag]
mergeOpts opts | NoAllowConflicts `elem` opts = opts
                | AllowConflicts   `elem` opts = opts
                | otherwise                    = MarkConflicts : opts

pullCmd :: PatchApplier pa => pa -> (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
pullCmd patchApplier (_,o) opts repos =
  do
    pullingFrom <- mapM (fixUrl o) repos
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $
     repoJob patchApplier opts $ \patchProxy initRepo -> do

      let repository = modifyCache initRepo $ addReposToCache pullingFrom
      (_, Sealed (us' :\/: to_be_pulled))
          <- fetchPatches o opts' repos "pull" repository
      let from_whom = error "Internal error: pull shouldn't need a 'from' address"
      applyPatches patchApplier patchProxy "pull" opts' from_whom repository us' to_be_pulled
    where
      opts' = mergeOpts opts
      addReposToCache repos' (Ca cache) = Ca $ [ toReadOnlyCache r | r <- repos' ] ++  cache
      toReadOnlyCache = Cache DarcsCache.Repo NotWritable


fetchCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fetchCmd (_,o) opts repos =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $
        fetchPatches o opts repos "fetch" >=> makeBundle opts

fetchPatches :: forall p wR wU . (RepoPatch p, ApplyState p ~ Tree)
             => AbsolutePath -> [DarcsFlag] -> [String] -> String
             -> Repository p wR wU wR
             -> IO (SealedPatchSet p Origin,
                    Sealed ((FL (PatchInfoAnd p)  :\/: FL (PatchInfoAnd p)) wR))
fetchPatches o opts unfixedrepodirs@(_:_) jobname repository = do
  here <- getCurrentDirectory
  repodirs <- (nub . filter (/= here)) `fmap` mapM (fixUrl o) unfixedrepodirs
  -- Test to make sure we aren't trying to pull from the current repo
  when (null repodirs) $
        fail "Can't pull from current repository!"
  old_default <- getPreflist "defaultrepo"
  when (old_default == repodirs && XMLOutput `notElem` opts) $
      let pulling = if DryRun `elem` opts then "Would pull" else "Pulling"
      in  putInfo opts $ text $ pulling++" from "++concatMap formatPath repodirs++"..."
  (Sealed them, Sealed compl) <- readRepos repository opts repodirs
  addRepoSource (head repodirs) (dryRun opts) (remoteRepos opts) (setDefault False opts)
  mapM_ (addToPreflist "repos") repodirs
  unless (Quiet `elem` opts || XMLOutput `elem` opts) $ mapM_ showMotd repodirs
  us <- readRepo repository
  checkUnrelatedRepos (AllowUnrelatedRepos `elem` opts) us them

  common :> _ <- return $ findCommonWithThem us them
  us' :\/: them' <- return $ findUncommon us them
  _   :\/: compl' <- return $ findUncommon us compl

  let avoided = mapFL info compl'
  ps :> _ <- return $ partitionFL (not . (`elem` avoided) . info) them'
  when (Verbose `elem` opts) $
       do case us' of
            (x@(_:>:_)) -> putDocLn $ text "We have the following new (to them) patches:"
                                                             $$ vcat (mapFL description x)
            _ -> return ()
          unless (nullFL ps) $ putDocLn $ text "They have the following patches to pull:"
                                                             $$ vcat (mapFL description ps)
  (hadConflicts, Sealed psFiltered)
    <- if SkipConflicts `elem` opts
        then filterOutConflicts (reverseFL us') repository ps
        else return (False, Sealed ps)
  when hadConflicts $ putStrLn "Skipping some patches which would cause conflicts."
  when  (nullFL psFiltered) $ do putInfo opts $ text "No remote patches to pull in!"
                                 setEnvDarcsPatches psFiltered
                                 when (reorder opts /= O.Reorder) exitSuccess
  let direction = if doReverse opts then FirstReversed else First
      context = selectionContext direction jobname (pullPatchSelOpts opts) Nothing Nothing
  (to_be_pulled :> _) <- runSelection (selectChanges psFiltered) context
  return (seal common, seal $ us' :\/: to_be_pulled)

fetchPatches _ _ [] jobname _ = fail $
  "No default repository to " ++ jobname ++ " from, please specify one"

makeBundle :: forall p wR . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag]
           -> (SealedPatchSet p Origin,
               Sealed ((FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wR))
           -> IO ()
makeBundle opts (Sealed common, Sealed (_ :\/: to_be_fetched)) =
    do
      bundle <- makeBundleN Nothing (unsafeCoercePEnd common) $
                 mapFL_FL hopefully to_be_fetched
      let fname = case to_be_fetched of
                    (x:>:_)-> patchFilename $ patchDesc x
                    _ -> impossible
          o = fromMaybe stdOut (getOutput opts fname)
      useAbsoluteOrStd writeDocBinFile putDoc o bundle

revertable :: IO a -> IO a
revertable x =
    x `clarifyErrors` unlines
          ["Error applying patch to the working directory.","",
           "This may have left your working directory an inconsistent",
           "but recoverable state. If you had no un-recorded changes",
           "by using 'darcs revert' you should be able to make your",
           "working directory consistent again."]

{- Read in the specified pull-from repositories.  Perform
Intersection, Union, or Complement read.  In patch-theory terms
(stated in set algebra, where + is union and & is intersection
and \ is complement):

    Union =         ((R1 + R2 + ... + Rn) \ Rc)
    Intersection =  ((R1 & R2 & ... & Rn) \ Rc)
    Complement =    (R1 \ Rc) \ ((R2 + R3 + ... + Rn) \ Rc)

                        where Rc = local repo
                              R1 = 1st specified pull repo
                              R2, R3, Rn = other specified pull repo

Since Rc is not provided here yet, the result of readRepos is a
tuple: the first patchset(s) to be complemented against Rc and then
the second patchset(s) to be complemented against Rc.
-}

readRepos :: (RepoPatch p, ApplyState p ~ Tree)
          => Repository p wR wU wT -> [DarcsFlag] -> [String]
          -> IO (SealedPatchSet p Origin,SealedPatchSet p Origin)
readRepos _ _ [] = impossible
readRepos to_repo opts us =
    do rs <- mapM (\u -> do r <- identifyRepositoryFor to_repo (useCache opts) u
                            ps <- readRepo r
                            return $ seal ps) us
       return $ if Intersection `elem` opts
                then (newsetIntersection rs, seal (PatchSet NilRL NilRL))
                else if Complement `elem` opts
                     then (head rs, newsetUnion $ tail rs)
                     else (newsetUnion rs, seal (PatchSet NilRL NilRL))

pullPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
pullPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps flags
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = isUnified flags
    }
