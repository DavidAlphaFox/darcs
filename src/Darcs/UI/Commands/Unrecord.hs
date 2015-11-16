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

{-# LANGUAGE CPP, OverloadedStrings #-}

module Darcs.UI.Commands.Unrecord
    ( unrecord
    , unpull
    , obliterate
    , getLastPatches
    , matchingHead
    ) where

import Prelude hiding ( (^), catch )

import Control.Exception ( catch, IOException )
import Control.Monad ( when )
import Data.Maybe( isJust, mapMaybe )
import Data.List ( intercalate )
import Storage.Hashed.Tree( Tree )
import System.Exit ( exitSuccess )

import Darcs.Patch ( RepoPatch, invert, commute, effect )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Bundle ( makeBundleN, contextPatches, minContext )
import Darcs.Patch.Depends ( findCommonWithThem, newsetUnion )
import Darcs.Patch.Match ( firstMatch, matchFirstPatchset, matchAPatchread, MatchFlag )
import Darcs.Patch.PatchInfoAnd ( hopefully, patchDesc )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), appendPSFL, Origin,
                         SealedPatchSet )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import Darcs.Patch.Witnesses.Ordered ( RL(..), (:>)(..), (+<+), mapFL_FL,
                                       nullFL, reverseRL, mapRL, FL(..) )
import Darcs.Util.Path( useAbsoluteOrStd, AbsolutePath, toFilePath, doesPathExist )
import Darcs.Util.SignalHandler ( catchInterrupt )
import Darcs.Repository ( PatchInfoAnd, withRepoLock, RepoJob(..), Repository,
                          tentativelyRemovePatches, finalizeRepositoryChanges,
                          tentativelyAddToPending, applyToWorking, readRepo,
                          invalidateIndex, unrecordedChanges,
                          identifyRepositoryFor )
import Darcs.Repository.Flags( UseIndex(..), ScanKnown(..), UpdateWorking(..), DryRun(NoDryRun) )
import Darcs.Repository.Lock( writeDocBinFile )
import Darcs.Repository.Prefs ( getDefaultRepoPath )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, commandAlias
                         , putVerbose, printDryRunMessageAndExit
                         , setEnvDarcsPatches, amInHashedRepository
                         , putInfo )
import Darcs.UI.Commands.Util ( getUniqueDPatchName )
import Darcs.UI.Flags
    ( doReverse, compression, verbosity, getOutput
    , useCache, dryRun, umask, DarcsFlag ( NotInRemote ), minimize
    , diffAlgorithm, hasXmlOutput, hasSummary, isInteractive, selectDeps )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import Darcs.UI.Options.All ( notInRemoteFlagName )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.SelectChanges ( selectChanges, WhichChanges(..),
                                selectionContext, runSelection )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.English ( presentParticiple )
import Darcs.Util.Printer ( text, putDoc )
import Darcs.Util.Progress ( debugMessage )

unrecordDescription :: String
unrecordDescription =
    "Remove recorded patches without changing the working copy."

unrecordHelp :: String
unrecordHelp = unlines
 [ "Unrecord does the opposite of record: it deletes patches from"
 , "the repository, without changing the working copy."
 , "Deleting patches from the repository makes active changes again"
 , "which you may record or revert later."
 , "Beware that you should not use this command if there is a"
 , "possibility that another user may have already pulled the patch."
 ]

unrecordBasicOpts :: DarcsOption a
                     ([MatchFlag] -> O.SelectDeps -> Maybe Bool -> Maybe String -> a)
unrecordBasicOpts
    = O.matchSeveralOrLast
    ^ O.selectDeps
    ^ O.interactive -- True
    ^ O.workingRepoDir

unrecordAdvancedOpts :: DarcsOption a (O.Compression -> O.UMask -> Bool -> a)
unrecordAdvancedOpts
    = O.compress
    ^ O.umask
    ^ O.changesReverse

unrecordOpts :: DarcsOption a
                ([MatchFlag]
                 -> O.SelectDeps
                 -> Maybe Bool
                 -> Maybe String
                 -> Maybe O.StdCmdAction
                 -> Bool
                 -> Bool
                 -> O.Verbosity
                 -> Bool
                 -> O.Compression
                 -> O.UMask
                 -> Bool
                 -> O.UseCache
                 -> Maybe String
                 -> Bool
                 -> Maybe String
                 -> Bool
                 -> a)
unrecordOpts = unrecordBasicOpts `withStdOpts` unrecordAdvancedOpts

unrecord :: DarcsCommand [DarcsFlag]
unrecord = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unrecord"
    , commandHelp = unrecordHelp
    , commandDescription = unrecordDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unrecordCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc unrecordAdvancedOpts
    , commandBasicOptions = odesc unrecordBasicOpts
    , commandDefaults = defaultFlags unrecordOpts
    , commandCheckOptions = ocheck unrecordOpts
    , commandParseOptions = onormalise unrecordOpts
    }

unrecordCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unrecordCmd _ opts _ =
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $
        RepoJob $ \repository -> do
            allpatches <- readRepo repository
            let matchFlags = parseFlags O.matchSeveralOrLast opts
            (_ :> patches) <- return $
                if firstMatch matchFlags
                    then getLastPatches matchFlags allpatches
                    else matchingHead matchFlags allpatches
            let direction = if doReverse opts then Last else LastReversed
                context = selectionContext direction "unrecord" (patchSelOpts opts) Nothing Nothing
            (_ :> to_unrecord) <- runSelection (selectChanges patches) context
            when (nullFL to_unrecord) $ do
                putStrLn "No patches selected!"
                exitSuccess
            putVerbose opts $
                text "About to write out (potentially) modified patches..."
            setEnvDarcsPatches to_unrecord
            invalidateIndex repository
            _ <- tentativelyRemovePatches repository (compression opts)
                     YesUpdateWorking to_unrecord
            finalizeRepositoryChanges repository YesUpdateWorking (compression opts)
            putStrLn "Finished unrecording."

getLastPatches :: RepoPatch p => [MatchFlag] -> PatchSet p Origin wR
               -> (PatchSet p :> FL (PatchInfoAnd p)) Origin wR
getLastPatches matchFlags ps = case matchFirstPatchset matchFlags ps of
                                   Sealed p1s -> findCommonWithThem ps p1s

unpullDescription :: String
unpullDescription =
    "Opposite of pull; unsafe if patch is not in remote repository."

unpullHelp :: String
unpullHelp = unlines
 [ "Unpull completely removes recorded patches from your local repository."
 , "The changes will be undone in your working copy and the patches"
 , "will not be shown in your changes list anymore. Beware that if the"
 , "patches are not still present in another repository you will lose"
 , "precious code by unpulling!"
 , ""
 , "One way to save unpulled patches is to use the -O flag. A patch"
 , "bundle will be created locally, that you will be able to apply"
 , "later to your repository with `darcs apply`."
 ]

unpull :: DarcsCommand [DarcsFlag]
unpull = (commandAlias "unpull" Nothing obliterate)
             { commandHelp = unpullHelp
             , commandDescription = unpullDescription
             , commandCommand = unpullCmd
             }

unpullCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unpullCmd = genericObliterateCmd "unpull"

obliterateDescription :: String
obliterateDescription =
    "Delete selected patches from the repository. (UNSAFE!)"

obliterateHelp :: String
obliterateHelp = unlines
 [ "Obliterate completely removes recorded patches from your local"
 , "repository. The changes will be undone in your working copy and the"
 , "patches will not be shown in your changes list anymore. Beware that"
 , "you can lose precious code by obliterating!"
 , ""
 , "One way to save obliterated patches is to use the -O flag. A patch"
 , "bundle will be created locally, that you will be able to apply"
 , "later to your repository with `darcs apply`."
 ]

obliterateBasicOpts :: DarcsOption a
                       ([Maybe String]
                        -> [MatchFlag]
                        -> O.SelectDeps
                        -> Maybe Bool
                        -> Maybe String
                        -> Maybe O.Summary
                        -> Maybe O.Output
                        -> Bool
                        -> O.DiffAlgorithm
                        -> O.DryRun
                        -> O.XmlOutput
                        -> a)
obliterateBasicOpts
    = O.notInRemote
    ^ O.matchSeveralOrLast
    ^ O.selectDeps
    ^ O.interactive
    ^ O.workingRepoDir
    ^ O.summary
    ^ O.output
    ^ O.minimize
    ^ O.diffAlgorithm
    ^ O.dryRunXml

obliterateAdvancedOpts :: DarcsOption a
                          (O.Compression -> O.UseIndex -> O.UMask -> Bool -> a)
obliterateAdvancedOpts
    = O.compress
    ^ O.useIndex
    ^ O.umask
    ^ O.changesReverse

obliterateOpts :: DarcsOption a
                  ([Maybe String]
                   -> [MatchFlag]
                   -> O.SelectDeps
                   -> Maybe Bool
                   -> Maybe String
                   -> Maybe O.Summary
                   -> Maybe O.Output
                   -> Bool
                   -> O.DiffAlgorithm
                   -> DryRun
                   -> O.XmlOutput
                   -> Maybe O.StdCmdAction
                   -> Bool
                   -> Bool
                   -> O.Verbosity
                   -> Bool
                   -> O.Compression
                   -> UseIndex
                   -> O.UMask
                   -> Bool
                   -> O.UseCache
                   -> Maybe String
                   -> Bool
                   -> Maybe String
                   -> Bool
                   -> a)
obliterateOpts = obliterateBasicOpts `withStdOpts` obliterateAdvancedOpts

obliterate :: DarcsCommand [DarcsFlag]
obliterate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "obliterate"
    , commandHelp = obliterateHelp
    , commandDescription = obliterateDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = obliterateCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc obliterateAdvancedOpts
    , commandBasicOptions = odesc obliterateBasicOpts
    , commandDefaults = defaultFlags obliterateOpts
    , commandCheckOptions = ocheck obliterateOpts
    , commandParseOptions = onormalise obliterateOpts
    }

obliterateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
obliterateCmd = genericObliterateCmd "obliterate"

data NotInRemoteLocation = NotInDefaultRepo
                         | NotInRemotePath String

-- | genericObliterateCmd is the function that executes the "obliterate" and
-- "unpull" commands. The first argument is the name under which the command is
-- invoked (@unpull@ or @obliterate@).
genericObliterateCmd :: String
                     -> (AbsolutePath, AbsolutePath)
                     -> [DarcsFlag]
                     -> [String]
                     -> IO ()
genericObliterateCmd cmdname _ opts _ =
    let cacheOpt = useCache opts
    in withRepoLock (dryRun opts) cacheOpt YesUpdateWorking (umask opts) $
        RepoJob $ \repository -> do
            -- FIXME we may need to honour --ignore-times here, although this
            -- command does not take that option (yet)
            pend <- unrecordedChanges (UseIndex, ScanKnown, diffAlgorithm opts) repository Nothing
            allpatches <- readRepo repository
            let collectNotIns (NotInRemote nir) = case nir of
                    Just p -> Just $ NotInRemotePath p
                    Nothing -> Just NotInDefaultRepo
                collectNotIns _ = Nothing
                notIns = mapMaybe collectNotIns opts
            (auto_kept :> removal_candidates) <- case notIns of
                [] -> do
                    let matchFlags = parseFlags O.matchSeveralOrLast opts
                    return $ if firstMatch matchFlags
                                   then getLastPatches matchFlags allpatches
                                   else matchingHead matchFlags allpatches
                nirs -> do
                    (Sealed thems) <-
                      getNotInRemotePatches cacheOpt repository nirs
                    return $ findCommonWithThem allpatches thems

            let direction = if doReverse opts then Last else LastReversed
                context = selectionContext direction cmdname (patchSelOpts opts) Nothing Nothing
            (kept :> removed) <-
                runSelection (selectChanges removal_candidates) context
            when (nullFL removed) $ do
                putStrLn "No patches selected!"
                exitSuccess
            case commute (effect removed :> pend) of
                Nothing -> fail $ "Can't " ++ cmdname
                                  ++ " patch without reverting some "
                                  ++ "unrecorded change."
                Just (_ :> p_after_pending) -> do
                    printDryRunMessageAndExit "obliterate"
                      (verbosity opts)
                      (hasSummary O.NoSummary opts)
                      (dryRun opts)
                      (hasXmlOutput opts)
                      (isInteractive True opts)
                      removed
                    setEnvDarcsPatches removed
                    when (isJust $ getOutput opts "") $
                        savetoBundle opts (auto_kept `appendPSFL` kept) removed
                    invalidateIndex repository
                    _ <- tentativelyRemovePatches repository
                        (compression opts) YesUpdateWorking removed
                    tentativelyAddToPending repository
                        YesUpdateWorking $ invert $ effect removed
                    finalizeRepositoryChanges repository
                        YesUpdateWorking (compression opts)
                    debugMessage "Applying patches to working directory..."
                    _ <- applyToWorking repository (verbosity opts)
                        (invert p_after_pending)
                         `catch` \(e :: IOException) -> fail $
                            "Couldn't undo patch in working dir.\n"
                            ++ show e
                    putStrLn $ "Finished " ++ presentParticiple cmdname ++ "."

-- | Get the union of the set of patches in each specified location
getNotInRemotePatches :: (RepoPatch p, ApplyState p ~ Tree) => O.UseCache
                      -> Repository p wX wU wT -> [NotInRemoteLocation]
                      -> IO (SealedPatchSet p Origin)
getNotInRemotePatches cacheOpt repository nirs = do
    putStrLn $ "Determining patches not in" ++ pluralExtra ++ ":\n" ++ names
    nirsPaths <- mapM getNotInRemotePath nirs
    newsetUnion `fmap` mapM readNir nirsPaths
  where
    toName (NotInRemotePath s) = "'" ++ s ++ "'"
    toName NotInDefaultRepo = "Default push/pull repo"

    pluralExtra = if length names > 1 then " any of" else ""
    names = intercalate "\n" $ map ((leader ++) . toName) nirs
    leader = "    - "

    readNir n = do
        r <- identifyRepositoryFor repository cacheOpt n
        rps <- readRepo r
        return $ seal rps

    getNotInRemotePath (NotInRemotePath p) = return p
    getNotInRemotePath NotInDefaultRepo = do
        defaultRepo <- getDefaultRepoPath
        let err = fail $ "No default push/pull repo configured, please pass a "
                         ++ "repo name to --" ++ notInRemoteFlagName
        maybe err return defaultRepo

-- | matchingHead returns the repository up to some tag. The tag t is the last
-- tag such that there is a patch after t that is matched by the user's query.
matchingHead :: forall p wR. RepoPatch p => [MatchFlag] -> PatchSet p Origin wR
             -> (PatchSet p :> FL (PatchInfoAnd p)) Origin wR
matchingHead matchFlags set =
    case mh set of
        (start :> patches) -> start :> reverseRL patches
  where
    mh :: forall wX . PatchSet p Origin wX
       -> (PatchSet p :> RL (PatchInfoAnd p)) Origin wX
    mh s@(PatchSet x _)
        | or (mapRL (matchAPatchread matchFlags) x) = contextPatches s
    mh (PatchSet x (Tagged t _ ps :<: ts)) =
        case mh (PatchSet (t :<: ps) ts) of
            (start :> patches) -> start :> x +<+ patches
    mh ps = ps :> NilRL

savetoBundle :: (RepoPatch p, ApplyState p ~ Tree) => [DarcsFlag]
             -> PatchSet p Origin wZ -> FL (PatchInfoAnd p) wZ wT -> IO ()
savetoBundle opts kept removed@(x :>: _) = do
    let genFullBundle = makeBundleN Nothing kept (mapFL_FL hopefully removed)
    bundle <- if not (minimize opts)
               then genFullBundle
               else do putInfo opts "Minimizing context, to generate bundle with full context hit ctrl-C..."
                       ( case minContext kept removed of
                           Sealed (kept' :> removed') -> makeBundleN Nothing kept' (mapFL_FL hopefully removed') )
                      `catchInterrupt` genFullBundle
    filename <- getUniqueDPatchName (patchDesc x)
    let Just outname = getOutput opts filename
    exists <- useAbsoluteOrStd (doesPathExist . toFilePath) (return False) outname
    when exists $ fail $ "Directory or file named '" ++ (show outname) ++ "' already exists."
    useAbsoluteOrStd writeDocBinFile putDoc outname bundle
savetoBundle _ _ NilFL = return ()

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveralOrLast flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps flags
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = O.NoContext
    }
