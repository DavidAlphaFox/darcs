--  Copyright (C) 2009 Ganesh Sittampalam
--
--  BSD3

{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.UI.Commands.Rebase ( rebase ) where

import Prelude hiding ( (^), catch, log )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , normalCommand, hiddenCommand
    , commandAlias
    , defaultRepo, nodefaults
    , putInfo, putVerbose
    , setEnvDarcsPatches
    , printDryRunMessageAndExit
    , amInHashedRepository
    )
import Darcs.UI.Commands.Amend ( updatePatchHeader )
import Darcs.UI.Commands.Apply ( applyCmd )
import Darcs.UI.Commands.Log ( changelog, getLogInfo )
import Darcs.UI.Commands.Pull ( pullCmd, revertable )
import Darcs.UI.Commands.Unrecord ( getLastPatches, matchingHead )
import Darcs.UI.CommandsAux ( checkPaths )
import Darcs.UI.Flags
    ( DarcsFlag
      ( AllowConflicts
      , NoAllowConflicts
      , MarkConflicts
      , SkipConflicts
      , SetScriptsExecutable)
    , externalMerge, allowConflicts
    , compression, diffingOpts
    , dryRun, reorder, verbosity
    , useCache, wantGuiPause
    , umask, toMatchFlags, doReverse
    , DarcsFlag(XMLOutput)
    , showChangesOnlyToFiles
    , diffAlgorithm, maxCount, hasSummary, isInteractive
    , selectDeps, hasXmlOutput
    )
import Darcs.UI.Options
    ( DarcsOption, (^), oid, odesc, ocheck, onormalise
    , defaultFlags, parseFlags
    )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository
    ( Repository, RepoJob(..), withRepoLock, withRepository
    , tentativelyAddPatch, finalizeRepositoryChanges
    , invalidateIndex
    , tentativelyRemovePatches, readRepo
    , tentativelyAddToPending, unrecordedChanges, applyToWorking
    , revertRepositoryChanges
    , setScriptsExecutablePatches
    , listFiles
    )
import Darcs.Repository.Flags ( UpdateWorking(..), ExternalMerge(..) )
import Darcs.Repository.Internal ( announceMergeConflicts )
import Darcs.Repository.Merge ( tentativelyMergePatches )
import Darcs.Repository.Prefs ( getPreflist )
import Darcs.Repository.Resolution ( standardResolution )
import Darcs.Patch ( invert, effect, commute, RepoPatch, description )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( commuterIdFL )
import Darcs.Patch.Info ( showPatchInfo )
import Darcs.Patch.Match ( firstMatch, secondMatch, splitSecondFL )
import Darcs.Patch.Named
    ( patchcontents
    , Named, fmapNamed, fmapFL_Named
    , patch2patchinfo
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, n2pia, hopefully )
import Darcs.Patch.Prim ( PrimOf, canonizeFL, fromPrim )
import Darcs.Patch.Rebase
    ( Rebasing(..), RebaseItem(..)
    , mkSuspended
    , simplifyPush, simplifyPushes
    , takeHeadRebase, takeHeadRebaseFL
    )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..), flToNamesPrims )
import Darcs.Patch.Rebase.Name ( RebaseName(..), commuteNameNamed )
import Darcs.Patch.Rebase.Viewing
    ( RebaseSelect(RSFwd), rsToPia
    , toRebaseSelect, fromRebaseSelect, extractRebaseSelect, reifyRebaseSelect
    , partitionUnconflicted
    , WithDroppedDeps(..), WDDNamed, commuterIdWDD
    , toRebaseChanges
    )
import Darcs.Patch.Permutations ( partitionConflictingFL )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Set ( PatchSet(..), appendPSFL )
import Darcs.Patch.Show ( showNicely )
import Darcs.Patch.Split ( primSplitter )
import Darcs.UI.ApplyPatches ( PatchApplier(..), PatchProxy(..) )
import Darcs.UI.SelectChanges
    ( selectChanges, runSelection
    , selectionContext, selectionContextGeneric, selectionContextPrim
    , WhichChanges(First, Last, LastReversed)
    , viewChanges
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (+>+), mapFL_FL
    , concatFL, mapFL, nullFL, lengthFL
    , (:>)(..)
    , RL(..), reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), seal, unseal
    , FlippedSeal(..)
    , Sealed2(..)
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.English ( englishNum, Noun(Noun) )
import Darcs.Util.Printer
    ( vcat, text, ($$)
    , putDocLnWith, simplePrinters
    , renderString, RenderMode(..)
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )

import Storage.Hashed.Tree ( Tree )

import Control.Applicative ( (<$>) )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )
import System.Exit ( exitSuccess )

#include "impossible.h"

rebaseDescription :: String
rebaseDescription = "Edit several patches at once."

rebaseHelp :: String
rebaseHelp =
 "The `darcs rebase' command is used to edit a collection of darcs patches.\n"

rebase :: DarcsCommand [DarcsFlag]
rebase = SuperCommand
    { commandProgramName = "darcs"
    , commandName = "rebase"
    , commandHelp = rebaseHelp
    , commandDescription = rebaseDescription
    , commandPrereq = amInHashedRepository
    , commandSubCommands =
        [ normalCommand pull
        , normalCommand apply
        , normalCommand suspend
        , normalCommand unsuspend
        , hiddenCommand reify
        , hiddenCommand inject
        , normalCommand obliterate
        , normalCommand log
        , hiddenCommand changes
        ]
    }

suspendBasicOpts :: DarcsOption a
                    ([O.MatchFlag]
                     -> O.SelectDeps
                     -> Maybe Bool
                     -> Maybe O.Summary
                     -> O.DiffAlgorithm
                     -> a)
suspendBasicOpts
    = O.matchSeveralOrLast
    ^ O.selectDeps
    ^ O.interactive
    ^ O.summary
    ^ O.diffAlgorithm

suspendAdvancedOpts :: DarcsOption a (Bool -> O.UseIndex -> a)
suspendAdvancedOpts
    = O.changesReverse
    ^ O.useIndex

suspendOpts :: DarcsOption a
               ([O.MatchFlag]
                -> O.SelectDeps
                -> Maybe Bool
                -> Maybe O.Summary
                -> O.DiffAlgorithm
                -> Maybe O.StdCmdAction
                -> Bool
                -> Bool
                -> O.Verbosity
                -> Bool
                -> Bool
                -> O.UseIndex
                -> O.UseCache
                -> Maybe String
                -> Bool
                -> Maybe String
                -> Bool
                -> a)
suspendOpts = suspendBasicOpts `withStdOpts` suspendAdvancedOpts

suspend :: DarcsCommand [DarcsFlag]
suspend = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "suspend"
    , commandHelp = "Select patches to move into a suspended state at the end of the repo.\n"
    , commandDescription = "Select patches to move into a suspended state at the end of the repo."
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = suspendCmd
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc suspendAdvancedOpts
    , commandBasicOptions = odesc suspendBasicOpts
    , commandDefaults = defaultFlags suspendOpts
    , commandCheckOptions = ocheck suspendOpts
    , commandParseOptions = onormalise suspendOpts
    }

suspendCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
suspendCmd _ opts _args =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $
    StartRebaseJob (compression opts) (verbosity opts) YesUpdateWorking $ \repository -> do
    allpatches <- readRepo repository
    (rOld, Sealed qs, allpatches_tail) <- return $ takeHeadRebase allpatches
    (_ :> patches) <-
        return $ if firstMatch (parseFlags O.matchSeveralOrLast opts)
                 then getLastPatches (parseFlags O.matchSeveralOrLast opts) allpatches_tail
                 else matchingHead (parseFlags O.matchSeveralOrLast opts) allpatches_tail
    let direction = if doReverse opts then Last else LastReversed
        patches_context = selectionContext direction "suspend" (patchSelOpts True opts) Nothing Nothing
    (_ :> psToSuspend) <-
        runSelection
            (selectChanges patches)
            patches_context
    when (nullFL psToSuspend) $ do
        putStrLn "No patches selected!"
        exitSuccess
    repository' <- doSuspend opts repository qs rOld psToSuspend
    finalizeRepositoryChanges repository' YesUpdateWorking (compression opts)
    return ()

doSuspend
    :: forall p wR wU wT wX wY
     . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
    => [DarcsFlag]
    -> Repository (Rebasing p) wR wU wT
    -> FL (RebaseItem p) wT wY
    -> PatchInfoAnd (Rebasing p) wT wT
    -> FL (PatchInfoAnd (Rebasing p)) wX wT
    -> IO (Repository (Rebasing p) wR wU wX)
doSuspend opts repository qs rOld psToSuspend = do
    pend <- unrecordedChanges (diffingOpts opts) repository Nothing
    FlippedSeal psAfterPending <-
        let effectPsToSuspend = effect psToSuspend in
        case commute (effectPsToSuspend :> pend) of
            Just (_ :> res) -> return (FlippedSeal res)
            Nothing -> do
                putVerbose opts $
                    let invPsEffect = invert effectPsToSuspend
                        doPartition = partitionConflictingFL (commuterIdFL selfCommuter)
                    in
                    case (doPartition invPsEffect pend, doPartition pend invPsEffect) of
                        (_ :> invSuspendedConflicts, _ :> pendConflicts) ->
                            let suspendedConflicts = invert invSuspendedConflicts in
                            text "these changes in the suspended patches:" $$
                            showNicely suspendedConflicts $$
                            text "conflict with these local changes:" $$
                            showNicely pendConflicts
                fail $ "Can't suspend selected patches without reverting some unrecorded change. Use --verbose to see the details."


    rNew <- mkSuspended (mapFL_FL (ToEdit . fmapNamed unNormal . hopefully) psToSuspend +>+ qs)
    invalidateIndex repository
    repository' <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking (psToSuspend +>+ (rOld :>: NilFL))
    tentativelyAddToPending repository' YesUpdateWorking $ invert $ effect psToSuspend
    repository'' <- tentativelyAddPatch repository' (compression opts) (verbosity opts) YesUpdateWorking (n2pia rNew)
    _ <- applyToWorking repository'' (verbosity opts) (invert psAfterPending)
            `catch` \(e :: IOException) -> fail ("Couldn't undo patch in working dir.\n" ++ show e)
    return repository''
  where unNormal :: Rebasing p wA wB -> p wA wB
        unNormal (Normal q) = q
        unNormal (Suspended _) = error "Can't suspend a rebase patch"

unsuspendBasicOpts :: DarcsOption a
                      (Maybe O.AllowConflicts
                       -> [O.MatchFlag]
                       -> Maybe Bool
                       -> Maybe O.Summary
                       -> ExternalMerge
                       -> Bool
                       -> Maybe String
                       -> O.DiffAlgorithm
                       -> a)
unsuspendBasicOpts
    = O.conflicts O.YesAllowConflictsAndMark
    ^ O.matchSeveralOrFirst
    ^ O.interactive
    ^ O.summary
    ^ O.useExternalMerge
    ^ O.keepDate
    ^ O.author
    ^ O.diffAlgorithm

unsuspendAdvancedOpts :: DarcsOption a (O.UseIndex -> a)
unsuspendAdvancedOpts = O.useIndex

unsuspendOpts :: DarcsOption a
                 (Maybe O.AllowConflicts
                  -> [O.MatchFlag]
                  -> Maybe Bool
                  -> Maybe O.Summary
                  -> ExternalMerge
                  -> Bool
                  -> Maybe String
                  -> O.DiffAlgorithm
                  -> Maybe O.StdCmdAction
                  -> Bool
                  -> Bool
                  -> O.Verbosity
                  -> Bool
                  -> O.UseIndex
                  -> O.UseCache
                  -> Maybe String
                  -> Bool
                  -> Maybe String
                  -> Bool
                  -> a)
unsuspendOpts = unsuspendBasicOpts `withStdOpts` unsuspendAdvancedOpts

unsuspend :: DarcsCommand [DarcsFlag]
unsuspend = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unsuspend"
    , commandHelp = "Selected patches to restore from a suspended state to the end of the repo.\n"
    , commandDescription = "Select suspended patches to restore to the end of the repo."
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unsuspendCmd False
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc unsuspendAdvancedOpts
    , commandBasicOptions = odesc unsuspendBasicOpts
    , commandDefaults = defaultFlags unsuspendOpts
    , commandCheckOptions = ocheck unsuspendOpts
    , commandParseOptions = onormalise unsuspendOpts
    }

reifyBasicOpts :: DarcsOption a
                  ([O.MatchFlag] -> Maybe Bool -> Bool -> Maybe String -> O.DiffAlgorithm -> a)
reifyBasicOpts
    = O.matchSeveralOrFirst
    ^ O.interactive
    ^ O.keepDate
    ^ O.author
    ^ O.diffAlgorithm

reifyOpts :: DarcsOption a
             ([O.MatchFlag]
              -> Maybe Bool
              -> Bool
              -> Maybe String
              -> O.DiffAlgorithm
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)
reifyOpts = reifyBasicOpts `withStdOpts` oid

reify :: DarcsCommand [DarcsFlag]
reify = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "reify"
    , commandHelp = "Select suspended patches to restore to the end of the repo, reifying any fixup patches.\n"
    , commandDescription = "Select suspended patches to restore to the end of the repo, reifying any fixup patches."
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unsuspendCmd True
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc reifyBasicOpts
    , commandDefaults = defaultFlags reifyOpts
    , commandCheckOptions = ocheck reifyOpts
    , commandParseOptions = onormalise reifyOpts
    }

unsuspendCmd :: Bool -> (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unsuspendCmd reifyFixups _ opts _args =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $
    RebaseJob (compression opts) (verbosity opts) YesUpdateWorking $
    \(repository :: Repository (Rebasing p) wR wU wR) -> (do
    patches <- readRepo repository
    pend <- unrecordedChanges (diffingOpts opts) repository Nothing
    let checkChanges :: FL (PrimOf p) wA wB -> IO (EqCheck wA wB)
        checkChanges NilFL = return IsEq
        checkChanges _ = error "can't unsuspend when there are unrecorded changes"
    IsEq <- checkChanges pend :: IO (EqCheck wR wU)
    (rOld, Sealed ps, _) <- return $ takeHeadRebase patches

    let selects = toRebaseSelect ps

    let matchFlags = toMatchFlags opts
    inRange :> outOfRange <-
        return $
            if secondMatch matchFlags then
            splitSecondFL rsToPia matchFlags selects
            else selects :> NilFL

    offer :> dontoffer <-
        return $
            if SkipConflicts `elem` opts
            then partitionUnconflicted inRange
            else inRange :> NilRL

    let warnSkip :: RL q wX wY -> IO ()
        warnSkip NilRL = return ()
        warnSkip _ = putStrLn "Skipping some patches which would cause conflicts."

    warnSkip dontoffer

    let patches_context = selectionContextGeneric rsToPia First "unsuspend" (patchSelOpts True opts) Nothing
    (chosen :> keep) <- runSelection (selectChanges offer) patches_context
    when (nullFL chosen) $ do putStrLn "No patches selected!"
                              exitSuccess

    (ps_to_unsuspend :: FL (WDDNamed p) wR wZ) :> chosen_fixups
           <- (if reifyFixups then reifyRebaseSelect else return . extractRebaseSelect) chosen

    let da = diffAlgorithm opts
        ps_to_keep = simplifyPushes da chosen_fixups .
                     fromRebaseSelect $
                     keep +>+ reverseRL dontoffer +>+ outOfRange

    Sealed standard_resolved_p <- return $ standardResolution $ concatFL
                                         $ progressFL "Examining patches for conflicts"
                                         $ mapFL_FL (patchcontents . wddPatch) ps_to_unsuspend
                                    :: IO (Sealed (FL (PrimOf p) wZ))

    let merge_opts | NoAllowConflicts `elem` opts = opts
                   | AllowConflicts   `elem` opts = opts
                   | otherwise                    = MarkConflicts : opts

    have_conflicts <- announceMergeConflicts "unsuspend" (allowConflicts merge_opts) (externalMerge merge_opts) standard_resolved_p
    Sealed (resolved_p  :: FL (PrimOf p) wA wB) <-
          case (externalMerge opts, have_conflicts) of
          (NoExternalMerge,_) -> return $ if AllowConflicts `elem` opts -- i.e. don't mark them
                                           then seal NilFL
                                           else seal standard_resolved_p
          (_,False) -> return $ seal standard_resolved_p
          (YesExternalMerge _, True) -> error "external resolution for unsuspend not implemented yet"

    let effect_to_apply = concatFL (mapFL_FL effect ps_to_unsuspend) +>+ resolved_p
    invalidateIndex repository
    repository' <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking (rOld :>: NilFL)
    -- TODO should catch logfiles (fst value from updatePatchHeader) and clean them up as in AmendRecord
    tentativelyAddToPending repository' YesUpdateWorking effect_to_apply
    (repository'', renames) <- doAdd repository' ps_to_unsuspend
    rNew <- unseal mkSuspended . unseal (simplifyPushes da (mapFL_FL NameFixup renames)) $ ps_to_keep
    repository''' <- tentativelyAddPatch repository'' (compression opts) (verbosity opts) YesUpdateWorking (n2pia rNew)
    finalizeRepositoryChanges repository''' YesUpdateWorking (compression opts)
    _ <- applyToWorking repository''' (verbosity opts) effect_to_apply `catch` \(e :: IOException) ->
        fail ("couldn't apply patch in working dir.\n" ++ show e)
    return ()
   ) :: IO ()
    where doAdd :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository (Rebasing p) wR wU wT
                -> FL (WDDNamed p) wT wT2
                -> IO (Repository (Rebasing p) wR wU wT2, FL (RebaseName p) wT2 wT2)
          doAdd repo NilFL = return (repo, NilFL)
          doAdd repo ((p :: WDDNamed p wT wU) :>:ps) = do
              case wddDependedOn p of
                  [] -> return ()
                  deps -> do
                      -- It might make sense to only print out this message once, but we might find
                      -- that the dropped dependencies are interspersed with other output,
                      -- e.g. if running with --ask-deps
                      putStr $ "Warning: dropping the following explicit "
                                 ++ englishNum (length deps) (Noun "dependency") ":\n\n"
                      let printIndented n =
                              mapM_ (putStrLn . (replicate n ' '++)) . lines .
                              renderString Encode . showPatchInfo
                      putStrLn . renderString Encode . showPatchInfo .
                              patch2patchinfo $ wddPatch p
                      putStr " depended on:\n"
                      mapM_ (printIndented 2) deps
                      putStr "\n"

              -- TODO should catch logfiles (fst value from updatePatchHeader) and clean them up as in AmendRecord
              p' <- snd <$> updatePatchHeader
                      False -- askDeps
                      (patchSelOpts True opts)
                      (parseFlags O.keepDate opts)
                      (parseFlags O.selectAuthor opts)
                      (parseFlags O.author opts)
                      (parseFlags O.patchname opts)
                      (parseFlags O.askLongComment opts)
                      repo (n2pia (fmapNamed Normal (wddPatch p))) NilFL
              repo' <- tentativelyAddPatch repo (compression opts) (verbosity opts) YesUpdateWorking p'
              -- create a rename that undoes the change we just made, so the contexts match up
              let rename :: RebaseName p wU wU
                  rename = Rename (info p') (patch2patchinfo (wddPatch p))
              -- push it through the remaining patches to fix them up
              Just (ps2 :> (rename2 :: RebaseName p wV wT2)) <- return (commuterIdFL (commuterIdWDD commuteNameNamed) (rename :> ps))
              -- assert that the rename still has a null effect on the context after commuting
              IsEq <- return (unsafeCoerceP IsEq :: EqCheck wV wT2)
              (repo'', renames) <- doAdd repo' ps2
              -- return the renames so that the suspended patch can be fixed up
              return (repo'', rename2 :>: renames)


injectBasicOpts :: DarcsOption a (Bool -> Maybe String -> O.DiffAlgorithm -> a)
injectBasicOpts = O.keepDate ^ O.author ^ O.diffAlgorithm

injectOpts :: DarcsOption a 
              (Bool
               -> Maybe String
               -> O.DiffAlgorithm
               -> Maybe O.StdCmdAction
               -> Bool
               -> Bool
               -> O.Verbosity
               -> Bool
               -> O.UseCache
               -> Maybe String
               -> Bool
               -> Maybe String
               -> Bool
               -> a)
injectOpts = injectBasicOpts `withStdOpts` oid

inject :: DarcsCommand [DarcsFlag]
inject = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "inject"
    , commandHelp = "Merge a change from the fixups of a patch into the patch itself.\n"
    , commandDescription = "Merge a change from the fixups of a patch into the patch itself."
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = injectCmd
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc injectBasicOpts
    , commandDefaults = defaultFlags injectOpts
    , commandCheckOptions = ocheck injectOpts
    , commandParseOptions = onormalise injectOpts
    }

injectCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
injectCmd _ opts _args =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $
    RebaseJob (compression opts) (verbosity opts) YesUpdateWorking $
    \(repository :: Repository (Rebasing p) wR wU wR) -> do
    patches <- readRepo repository

    (rOld, Sealed ps, _) <- return $ takeHeadRebase patches

    let selects = toRebaseSelect ps

    -- TODO this selection doesn't need to respect dependencies
    -- TODO we only want to select one patch: generalise withSelectedPatchFromRepo
    let patches_context = selectionContextGeneric rsToPia First "inject into" (patchSelOpts True opts) Nothing
    (chosens :> rest_selects) <- runSelection (selectChanges selects) patches_context

    let extractSingle :: FL (RebaseSelect p) wX wY -> (FL (RebaseFixup p) :> Named p) wX wY
        extractSingle (RSFwd fixups toedit :>: NilFL) = fixups :> toedit
        extractSingle (_ :>: NilFL) = impossible
        extractSingle _ = error "You must select precisely one patch!"

    fixups :> toedit <- return $ extractSingle chosens

    name_fixups :> prim_fixups <- return $ flToNamesPrims fixups

    let changes_context = selectionContextPrim Last "inject" (patchSelOpts True opts) (Just primSplitter) Nothing Nothing
    (rest_fixups :> injects) <- runSelection (selectChanges prim_fixups) changes_context

    when (nullFL injects) $ do
        putStrLn "No changes selected!"
        exitSuccess

    -- Don't bother to update patch header since unsuspend will do that later
    let da = diffAlgorithm opts
        toeditNew = fmapFL_Named (mapFL_FL fromPrim . canonizeFL da . (injects +>+) . effect) toedit
    rNew <- unseal mkSuspended $ unseal (simplifyPushes da (mapFL_FL NameFixup name_fixups))
                               $ simplifyPushes da (mapFL_FL PrimFixup rest_fixups)
                               $ ToEdit toeditNew :>: fromRebaseSelect rest_selects

    repository' <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking (rOld :>: NilFL)
    repository'' <- tentativelyAddPatch repository' (compression opts) (verbosity opts) YesUpdateWorking (n2pia rNew)
    finalizeRepositoryChanges repository'' YesUpdateWorking (compression opts)
    return ()

obliterateBasicOpts :: DarcsOption a (O.DiffAlgorithm -> a)
obliterateBasicOpts = O.diffAlgorithm

obliterateOpts :: DarcsOption a
                  (O.DiffAlgorithm
                   -> Maybe O.StdCmdAction
                   -> Bool
                   -> Bool
                   -> O.Verbosity
                   -> Bool
                   -> O.UseCache
                   -> Maybe String
                   -> Bool
                   -> Maybe String
                   -> Bool
                   -> a)
obliterateOpts = obliterateBasicOpts `withStdOpts` oid

obliterate :: DarcsCommand [DarcsFlag]
obliterate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "obliterate"
    , commandHelp = "Obliterate a patch that is currently suspended.\n"
    , commandDescription = "Obliterate a patch that is currently suspended.\n"
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = obliterateCmd
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc obliterateBasicOpts
    , commandDefaults = defaultFlags obliterateOpts
    , commandCheckOptions = ocheck obliterateOpts
    , commandParseOptions = onormalise obliterateOpts
    }

obliterateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
obliterateCmd _ opts _args =
    withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $
    RebaseJob (compression opts) (verbosity opts) YesUpdateWorking $
    \(repository :: Repository (Rebasing p) wR wU wR) -> (do
    patches <- readRepo repository

    (rOld, Sealed ps, _) <- return $ takeHeadRebase patches

    let selects = toRebaseSelect ps

    -- TODO this selection doesn't need to respect dependencies
    let patches_context = selectionContextGeneric rsToPia First "obliterate" (obliteratePatchSelOpts opts) Nothing
    (chosen :> keep) <- runSelection (selectChanges selects) patches_context
    when (nullFL chosen) $ do putStrLn "No patches selected!"
                              exitSuccess

    let da = diffAlgorithm opts
        do_obliterate :: FL (RebaseItem p) wX wY -> FL (RebaseItem p) wY wZ -> Sealed (FL (RebaseItem p) wX)
        do_obliterate NilFL = Sealed
        do_obliterate (Fixup f :>: qs) = unseal (simplifyPush da f) . do_obliterate qs
        do_obliterate (ToEdit e :>: qs) = -- since Named doesn't have any witness context for the
                                          -- patch names, the AddName here will be inferred to be wX wX
                                          unseal (simplifyPush da (NameFixup (AddName (patch2patchinfo e)))) .
                                          unseal (simplifyPushes da (mapFL_FL PrimFixup (effect (patchcontents e)))) .
                                          do_obliterate qs

    let ps_to_keep = do_obliterate (fromRebaseSelect chosen) (fromRebaseSelect keep)
    rNew <- unseal mkSuspended ps_to_keep

    repository' <- tentativelyRemovePatches repository (compression opts) YesUpdateWorking (rOld :>: NilFL)
    repository'' <- tentativelyAddPatch repository' (compression opts) (verbosity opts) YesUpdateWorking (n2pia rNew)
    finalizeRepositoryChanges repository'' YesUpdateWorking (compression opts)
    return ()
   ) :: IO ()


pullDescription :: String
pullDescription =
 "Copy and apply patches from another repository, suspending any local patches that conflict."

pullHelp :: String
pullHelp =
 "Copy and apply patches from another repository, suspending any local patches that conflict."

pullBasicOpts :: DarcsOption a
                 ([O.MatchFlag]
                  -> O.Reorder
                  -> Maybe Bool
                  -> Maybe O.AllowConflicts
                  -> ExternalMerge
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
    ^ O.interactive
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
    ^ O.network

pullOpts :: DarcsOption a
            ([O.MatchFlag]
             -> O.Reorder
             -> Maybe Bool
             -> Maybe O.AllowConflicts
             -> ExternalMerge
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
             -> O.NetworkOptions
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
pullOpts = pullBasicOpts `withStdOpts` pullAdvancedOpts

pull :: DarcsCommand [DarcsFlag]
pull = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "pull"
    , commandHelp = pullHelp
    , commandDescription = pullDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]..."]
    , commandCommand = pullCmd RebasePatchApplier
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = getPreflist "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc pullAdvancedOpts
    , commandBasicOptions = odesc pullBasicOpts
    , commandDefaults = defaultFlags pullOpts
    , commandCheckOptions = ocheck pullOpts
    , commandParseOptions = onormalise pullOpts
    }

applyDescription :: String
applyDescription = "Apply a patch bundle, suspending any local patches that conflict."

applyHelp :: String
applyHelp = "Apply a patch bundle, suspending any local patches that conflict."

stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x

apply :: DarcsCommand [DarcsFlag]
apply = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "apply"
    , commandHelp = applyHelp
    , commandDescription = applyDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["<PATCHFILE>"]
    , commandCommand = applyCmd RebasePatchApplier
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listFiles False
    , commandArgdefaults = const stdindefault
    , commandAdvancedOptions = odesc applyAdvancedOpts
    , commandBasicOptions = odesc applyBasicOpts
    , commandDefaults = defaultFlags applyOpts
    , commandCheckOptions = ocheck applyOpts
    , commandParseOptions = onormalise applyOpts
    }

applyBasicOpts :: DarcsOption a
                  (O.Verify
                   -> O.Reorder
                   -> Maybe Bool
                   -> O.DryRun
                   -> O.XmlOutput
                   -> [O.MatchFlag]
                   -> Maybe String
                   -> O.DiffAlgorithm
                   -> a)
applyBasicOpts
    = O.verify
    ^ O.reorder
    ^ O.interactive
    ^ O.dryRunXml
    ^ O.matchSeveral
    ^ O.workingRepoDir
    ^ O.diffAlgorithm

applyAdvancedOpts :: DarcsOption a
                     (Maybe String
                      -> Maybe String
                      -> Bool
                      -> (Bool, Maybe String)
                      -> O.UseIndex
                      -> O.Compression
                      -> O.SetScriptsExecutable
                      -> O.UMask
                      -> Bool
                      -> Bool
                      -> O.WantGuiPause
                      -> a)
applyAdvancedOpts
    = O.reply
    ^ O.ccApply
    ^ O.happyForwarding
    ^ O.sendmail
    ^ O.useIndex
    ^ O.compress
    ^ O.setScriptsExecutable
    ^ O.umask
    ^ O.restrictPaths
    ^ O.changesReverse
    ^ O.pauseForGui

applyOpts :: DarcsOption a
             (O.Verify
              -> O.Reorder
              -> Maybe Bool
              -> O.DryRun
              -> O.XmlOutput
              -> [O.MatchFlag]
              -> Maybe String
              -> O.DiffAlgorithm
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> Maybe String
              -> Maybe String
              -> Bool
              -> (Bool, Maybe String)
              -> O.UseIndex
              -> O.Compression
              -> O.SetScriptsExecutable
              -> O.UMask
              -> Bool
              -> Bool
              -> O.WantGuiPause
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)

applyOpts = applyBasicOpts `withStdOpts` applyAdvancedOpts

data RebasePatchApplier = RebasePatchApplier

instance PatchApplier RebasePatchApplier where
    type CarrierType RebasePatchApplier p = Rebasing p

    repoJob RebasePatchApplier opts f =
        StartRebaseJob (compression opts) (verbosity opts) YesUpdateWorking (f PatchProxy)
    applyPatches RebasePatchApplier PatchProxy = applyPatchesForRebaseCmd

applyPatchesForRebaseCmd
    :: forall p wR wU wX wT wZ
     . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
    => String
    -> [DarcsFlag]
    -> String
    -> Repository (Rebasing p) wR wU wT
    -> FL (PatchInfoAnd (Rebasing p)) wX wT
    -> FL (PatchInfoAnd (Rebasing p)) wX wZ
    -> IO ()
applyPatchesForRebaseCmd cmdName opts _from_whom repository us' to_be_applied = do
    printDryRunMessageAndExit cmdName
        (verbosity opts)
        (hasSummary O.NoSummary opts)
        (dryRun opts)
        (hasXmlOutput opts)
        (isInteractive True opts)
        to_be_applied
    setEnvDarcsPatches to_be_applied
    when (nullFL to_be_applied) $ do
        putStrLn $ "You don't want to " ++ cmdName ++ " any patches, and that's fine with me!"
        exitSuccess
    checkPaths opts to_be_applied
    putVerbose opts $ text $ "Will " ++ cmdName ++ " the following patches:"
    putVerbose opts $ vcat $ mapFL description to_be_applied
    usOk :> usConflicted <- return $ partitionConflictingFL (commuterIdFL selfCommuter) us' to_be_applied

    when (lengthFL usConflicted > 0) $
        putInfo opts $ text "The following local patches are in conflict:"

    -- TODO: we assume the options apply only to the main
    -- command, review if there are any we should keep
    let patches_context = selectionContext LastReversed "suspend" applyPatchSelOpts Nothing Nothing

    (usKeep :> usToSuspend) <- runSelection (selectChanges usConflicted) patches_context

    (rOld, Sealed qs, _) <- return $ takeHeadRebaseFL us'
    repository' <- doSuspend opts repository qs rOld usToSuspend

    -- TODO This is a nasty hack, caused by the fact that readUnrecorded
    -- claims to read the tentative state but actual reads the committed state
    -- as a result we have to commit here so that tentativelyMergePatches does
    -- the right thing.
    finalizeRepositoryChanges repository' YesUpdateWorking (compression opts)
        >> revertRepositoryChanges repository' YesUpdateWorking

    Sealed pw <-
        tentativelyMergePatches
            repository' cmdName
            (allowConflicts opts) YesUpdateWorking
            (externalMerge opts)
            (wantGuiPause opts) (compression opts) (verbosity opts)
            (reorder opts) (diffingOpts opts)
            (usOk +>+ usKeep) to_be_applied

    invalidateIndex repository
    finalizeRepositoryChanges repository' YesUpdateWorking (compression opts)
    _ <- revertable $ applyToWorking repository' (verbosity opts) pw
    when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches pw
    putInfo opts $ text $ "Finished " ++ cmdName ++ "ing."

-- TODO I doubt this is right, e.g. withContext should be inherited
applyPatchSelOpts :: S.PatchSelectionOptions
applyPatchSelOpts = S.PatchSelectionOptions
    { S.verbosity = O.NormalVerbosity
    , S.matchFlags = []
    , S.diffAlgorithm = O.PatienceDiff
    , S.interactive = True
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = O.NoSummary
    , S.withContext = O.NoContext
    }

obliteratePatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
obliteratePatchSelOpts opts = (patchSelOpts True opts)
    { S.selectDeps = O.NoDeps
    }

patchSelOpts :: Bool -> [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts defInteractive flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveralOrLast flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive defInteractive flags
    , S.selectDeps = selectDeps flags
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = O.NoContext
    }

log :: DarcsCommand [DarcsFlag]
log = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "log"
    , commandHelp = "List the currently suspended changes.\n"
    , commandDescription = "List the currently suspended changes"
    , commandPrereq = amInHashedRepository
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = logCmd
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc logAdvancedOpts
    , commandBasicOptions = odesc logBasicOpts
    , commandDefaults = defaultFlags logOpts
    , commandCheckOptions = ocheck logOpts
    , commandParseOptions = onormalise logOpts
    }

logBasicOpts :: DarcsOption a
                (Maybe O.Summary
                 -> Maybe Bool
                 -> a)
logBasicOpts
    = O.summary
    ^ O.interactive -- False

logAdvancedOpts :: DarcsOption a a
logAdvancedOpts = oid

logOpts :: DarcsOption a
                       (Maybe O.Summary
                        -> Maybe Bool
                        -> Maybe O.StdCmdAction
                        -> Bool
                        -> Bool
                        -> O.Verbosity
                        -> Bool
                        -> O.UseCache
                        -> Maybe String
                        -> Bool
                        -> Maybe String
                        -> Bool
                        -> a)

logOpts = logBasicOpts `withStdOpts` logAdvancedOpts

logCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
logCmd _ opts _files =
    withRepository (useCache opts) $
    RebaseJob (compression opts) (verbosity opts) YesUpdateWorking $ \repository -> do
        patches <- readRepo repository
        (_, Sealed ps, _) <- return $ takeHeadRebase patches
        let psToShow = toRebaseChanges ps
        if isInteractive False opts
            then viewChanges (patchSelOpts False opts) (mapFL Sealed2 psToShow)
            else do
                debugMessage "About to print the changes..."
                let printers = if XMLOutput `elem` opts then simplePrinters else fancyPrinters
                    emptyPatchSet = PatchSet NilRL NilRL
                    patchSet = appendPSFL emptyPatchSet psToShow
                logInfo <-
                    getLogInfo
                         (maxCount opts)
                         (toMatchFlags opts)
                         (showChangesOnlyToFiles opts)
                         Nothing
                         (\_ qs -> return qs)
                         patchSet
                let logDoc = changelog opts patchSet logInfo
                putDocLnWith printers logDoc

-- | changes is an alias for log
changes :: DarcsCommand [DarcsFlag]
changes = commandAlias "changes" Nothing log

{-
TODO:

 - amend-record shows the diff between the conflicted state and the resolution, which is unhelpful
 - testing
 - make aggregate commands
 - argument handling
 - what should happen to patch comment on unsuspend?
 - don't just drop explicit dependencies:
    - turn patchnames/explicit deps into patch type and use commutation
 - repo representation
 - seem to be able to get a messed up unrevert context
 - darcs pull/get can setup a rebase patch in a remote repo without the right format
    - rebase patches seem to parse as empty rather than failing??
 - warn about suspending conflicts
 - indication of expected conflicts on unsuspend
    - why isn't ! when you do x accurate?
 - rebase obliterate for more efficient removing of suspended patches
 - rebase pull needs more UI work
    - automatically answer yes re suspension
    - offer all patches (so they can be kept in order)
       - or perhaps rebase suspend --complement?
 - rebase changes for viewing suspended patch
 - matching options for rebase unsuspend (etc)
 - make unsuspend actually display the patch helpfully like normal selection
 - amended patches will often be in both the target repo and in the rebase context, detect?
 - can we be more intelligent about conflict resolutions?
 - --all option to unsuspend
 - review other conflict options for unsuspend
 - warning message on suspend about not being able to unsuspend with unrecorded changes
 - aborting during a rebase pull or rebase suspend causes it to leave the repo marked for rebase
 - rebase suspend needs --match
 - patch count: get English right in <n> suspended patch(es)
 - darcs check should check integrity of rebase patch
 - review existence of reify and inject commands - bit of an internals hack
 - need to move rebase to front before adding amend-record hint (and test this)
 - print something while moving rebase to front
-}

