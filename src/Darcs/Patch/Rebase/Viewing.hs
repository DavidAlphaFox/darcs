--  Copyright (C) 2009 Ganesh Sittampalam
--
--  BSD3
{-# LANGUAGE CPP, GADTs, UndecidableInstances #-}
module Darcs.Patch.Rebase.Viewing
    ( RebaseSelect(..)
    , toRebaseSelect, fromRebaseSelect, extractRebaseSelect, reifyRebaseSelect
    , partitionUnconflicted
    , rsToPia
    , WithDroppedDeps(..), WDDNamed, commuterIdWDD
    , RebaseChange(..), toRebaseChanges
    ) where

import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( CommuteFn, commuterIdFL, commuterRLId, MergeFn
                             , totalCommuterIdFL
                             )
import Darcs.Patch.Conflict
    ( Conflict(..), CommuteNoConflicts(..)
    , IsConflictedPrim
    )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Invert ( invertFL, invertRL )
import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.MaybeInternal ( MaybeInternal(..) )
import Darcs.Patch.Merge ( Merge(..), selfMerger )
import Darcs.Patch.Named
    ( Named(..), namepatch, infopatch
    , mergerIdNamed
    , adddeps, getdeps
    , patch2patchinfo, patchcontents
    )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.Patchy ( Invert(..), Commute(..), Patchy, Apply(..),
                            ShowPatch(..), ReadPatch(..),
                            PatchInspect(..)
                          )
import Darcs.Patch.Prim
    ( PrimPatch, PrimPatchBase, PrimOf, FromPrim(..), FromPrims(..)
    )
import Darcs.Patch.Rebase
    ( Rebasing(..), RebaseItem(..)
    )
import Darcs.Patch.Rebase.Fixup
    ( RebaseFixup(..)
    , commuteFixupNamed, commuteNamedFixups
    , flToNamesPrims
    )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Patch.Rebase.NameHack ( NameHack(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Summary ( plainSummary )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..), ShowDict(ShowDictClass)
    , showsPrec2
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.Printer ( ($$), redText, empty, vcat )
import Darcs.Util.Show ( appPrec )

import Prelude hiding ( pi )
import Control.Applicative ( (<$>) )
import Data.List ( nub, (\\) )
import Data.Maybe ( fromMaybe )

#include "impossible.h"

-- |Encapsulate a single patch in the rebase state together with its fixups.
-- Used during interactive selection to make sure that each item presented
-- to the user corresponds to a patch.
data RebaseSelect p wX wY where
   -- The normal case for a RebaseSelect - a patch that points forwards.
   RSFwd :: FL (RebaseFixup p) wX wY -> Named p wY wZ -> RebaseSelect p wX wZ
   -- We need an 'Invert' instance. We just represent inverses
   -- with a different constructor instead of trying to come up with some logical
   -- inversion of the individual components. Typically they get uninverted
   -- before anything significant is done with them, so a lot of code that
   -- processes 'RebaseSelect' patches just uses 'impossible' for 'RSRev'.
   RSRev :: FL (RebaseFixup p) wX wY -> Named p wY wZ -> RebaseSelect p wZ wX


instance (Show2 p, Show2 (PrimOf p)) => Show (RebaseSelect p wX wY) where
    showsPrec d (RSFwd fixups toedit) =
        showParen (d > appPrec) $
            showString "RSFwd " . showsPrec2 (appPrec + 1) fixups .
            showString " " . showsPrec2 (appPrec + 1) toedit
    showsPrec d (RSRev fixups toedit) =
        showParen (d > appPrec) $
            showString "RSRev " . showsPrec2 (appPrec + 1) fixups .
            showString " " . showsPrec2 (appPrec + 1) toedit

instance (Show2 p, Show2 (PrimOf p)) => Show1 (RebaseSelect p wX) where
    showDict1 = ShowDictClass

instance (Show2 p, Show2 (PrimOf p)) => Show2 (RebaseSelect p) where
    showDict2 = ShowDictClass

-- TODO: merge with RebaseSelect.
-- |Used for displaying during 'rebase changes'.
-- 'Named (RebaseChange p)' is very similar to 'RebaseSelect p' but slight
-- mismatches ('Named' embeds an 'FL') makes it not completely trivial to merge
-- them.
data RebaseChange p wX wY where
    RCFwd :: FL (RebaseFixup p) wX wY -> FL p wY wZ -> RebaseChange p wX wZ
    RCRev :: FL (RebaseFixup p) wX wY -> FL p wY wZ -> RebaseChange p wZ wX

instance (Show2 p, Show2 (PrimOf p)) => Show1 (RebaseChange p wX) where
    showDict1 = ShowDictClass

instance (Show2 p, Show2 (PrimOf p)) => Show2 (RebaseChange p) where
    showDict2 = ShowDictClass

instance (Show2 p, Show2 (PrimOf p)) => Show (RebaseChange p wX wY) where
    showsPrec d (RCFwd fixups changes) =
        showParen (d > appPrec) $
            showString "RCFwd " . showsPrec2 (appPrec + 1) fixups .
            showString " " . showsPrec2 (appPrec + 1) changes
    showsPrec d (RCRev fixups changes) =
        showParen (d > appPrec) $
            showString "RCRev " . showsPrec2 (appPrec + 1) fixups .
            showString " " . showsPrec2 (appPrec + 1) changes

-- |Get hold of the 'PatchInfoAnd' patch inside a 'RebaseSelect'.
rsToPia :: RebaseSelect p wX wY -> Sealed2 (PatchInfoAnd p)
rsToPia (RSFwd _ toEdit) = Sealed2 (n2pia toEdit)
rsToPia (RSRev _ toEdit) = Sealed2 (n2pia toEdit)

instance PrimPatchBase p => PrimPatchBase (RebaseSelect p) where
   type PrimOf (RebaseSelect p) = PrimOf p

instance (PrimPatchBase p, PatchListFormat p, Conflict p, FromPrim p, Effect p, CommuteNoConflicts p, IsHunk p, Patchy p, ApplyState p ~ ApplyState (PrimOf p), NameHack p)
    => Patchy (RebaseSelect p)

instance ( PrimPatchBase p, Apply p, ApplyState p ~ ApplyState (PrimOf p)
         , Invert p
         )
        => Patchy (RebaseChange p)

instance PatchDebug p => PatchDebug (RebaseSelect p)

instance PatchDebug p => PatchDebug (RebaseChange p)

instance (PrimPatchBase p, Invert p, Apply p, ApplyState p ~ ApplyState (PrimOf p)) => Apply (RebaseSelect p) where
   type ApplyState (RebaseSelect p) = ApplyState p
   apply (RSFwd fixups toedit) = apply fixups >> apply toedit
   apply (RSRev fixups toedit) = apply (invert toedit) >> apply (invertFL fixups)

instance ( PrimPatchBase p, Invert p, Apply p
         , ApplyState p ~ ApplyState (PrimOf p)
         )
        => Apply (RebaseChange p) where

    type ApplyState (RebaseChange p) = ApplyState p
    apply (RCFwd fixups contents) = apply fixups >> apply contents
    apply (RCRev fixups contents) = apply (invert contents) >> apply (invertFL fixups)

instance (PrimPatchBase p, FromPrim p, Conflict p, CommuteNoConflicts p, Invert p) => Conflict (RebaseSelect p) where
   resolveConflicts (RSFwd _ toedit) = resolveConflicts toedit
   resolveConflicts (RSRev{}) = impossible

-- newtypes to help the type-checker with the 'changeAsMerge' abstraction
newtype ResolveConflictsResult p wY =
    ResolveConflictsResult {
        getResolveConflictsResult :: [[Sealed (FL (PrimOf p) wY)]]
    }

newtype ListConflictedFilesResult (p :: * -> * -> *) wY =
    ListConflictedFilesResult {
        getListConflictedFilesResult :: [FilePath]
    }

newtype ConflictedEffectResult p wY =
    ConflictedEffectResult {
        getConflictedEffectResult :: [IsConflictedPrim (PrimOf p)]
    }

changeAsMerge
    :: (PrimPatchBase p, Invert p, FromPrim p, Merge p)
    => (forall wX' . FL p wX' wY -> result p wY)
    -> RebaseChange p wX wY
    -> result p wY
changeAsMerge f (RCFwd fixups changes) =
    case flToNamesPrims fixups of
        _names :> prims ->
            case merge (invert (fromPrims prims) :\/: changes) of
                changes' :/\: _ifixups' ->
                    -- it might make sense to pass
                    -- (changes' +>+ invert _ifixups') to resolveConflicts,
                    -- but this isn't actually treated as a conflict by
                    -- either V1 or V2 patches (not quite sure why)
                    f (unsafeCoercePEnd changes')
changeAsMerge _ (RCRev _ _) = impossible

instance
    ( PrimPatchBase p, Invert p, Effect p
    , FromPrim p, Merge p, Conflict p, CommuteNoConflicts p
    )
   => Conflict (RebaseChange p) where
    resolveConflicts =
        getResolveConflictsResult . changeAsMerge (ResolveConflictsResult . resolveConflicts)

    listConflictedFiles =
        getListConflictedFilesResult . changeAsMerge (ListConflictedFilesResult . listConflictedFiles)

    conflictedEffect =
        getConflictedEffectResult . changeAsMerge (ConflictedEffectResult . conflictedEffect)

instance (PrimPatchBase p, Invert p, Effect p) => Effect (RebaseSelect p) where
   effect (RSFwd fixups toedit) =
        concatFL (mapFL_FL effect fixups) +>+ effect toedit
   effect (RSRev fixups toedit) = invertRL . reverseFL . effect $ RSFwd fixups toedit

instance (PrimPatchBase p, Invert p, Effect p) => Effect (RebaseChange p) where
    effect (RCFwd fixups changes) =
        concatFL (mapFL_FL effect fixups) +>+ effect changes
    effect (RCRev fixups changes) =
        invertRL . reverseFL . effect $ RCFwd fixups changes

instance (PrimPatchBase p, PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (RebaseSelect p) where
   showPatch (RSFwd fixups toedit) =
         showPatch (Suspended (mapFL_FL Fixup fixups +>+ ToEdit toedit :>: NilFL))
   showPatch (RSRev {}) = impossible

instance (PrimPatchBase p, PatchListFormat p, ShowPatchBasic p)
      => ShowPatchBasic (RebaseChange p) where
    showPatch (RCFwd fixups contents) =
        vcat (mapFL showPatch contents) $$
        (if nullFL fixups
            then empty
            else
                redText "" $$
                redText "conflicts:" $$
                redText "" $$
                vcat (mapRL showFixup (invertFL fixups))
        )
        where
            showFixup (PrimFixup p) = showPatch p
            showFixup (NameFixup n) = showPatch n
    showPatch (RCRev {}) = impossible

instance (PrimPatchBase p, PatchListFormat p, Apply p, CommuteNoConflicts p, Conflict p, IsHunk p, ShowPatch p)
    => ShowPatch (RebaseSelect p) where

   description (RSFwd _ toedit) = description toedit
   description (RSRev _  _toedit) = impossible

   summary = summaryFL . fromRebaseSelect . (:>: NilFL)
   summaryFL = summaryFL . fromRebaseSelect

instance
    ( PrimPatchBase p, PatchListFormat p, ShowPatchBasic p
    , Invert p, Effect p, Merge p, FromPrim p
    , Conflict p, CommuteNoConflicts p
    )
   => ShowPatch (RebaseChange p) where

    summary = plainSummary
    summaryFL = plainSummary


instance ReadPatch p => ReadPatch (RebaseSelect p) where
   readPatch' = error "can't read RebaseSelect patches"

instance ReadPatch (RebaseChange p) where
   readPatch' = error "can't read RebaseChange patches"

-- |Turn a list of rebase items being rebased into a list suitable for use
-- by interactive selection. Each actual patch being rebased is grouped
-- together with any fixups needed.
toRebaseSelect :: PrimPatchBase p => FL (RebaseItem p) wX wY -> FL (RebaseSelect p) wX wY

-- |Turn a list of items back from the format used for interactive selection
-- into a normal list
fromRebaseSelect :: FL (RebaseSelect p) wX wY -> FL (RebaseItem p) wX wY

fromRebaseSelect NilFL = NilFL
fromRebaseSelect (RSFwd fixups toedit :>: ps)
    = mapFL_FL Fixup fixups +>+ ToEdit toedit :>: fromRebaseSelect ps
fromRebaseSelect (RSRev {} :>: _) = impossible

toRebaseSelect NilFL = NilFL
toRebaseSelect (Fixup f :>: ps) =
    case toRebaseSelect ps of
      RSFwd fixups toedit :>: rest -> RSFwd (f :>: fixups) toedit :>: rest
      NilFL -> bug "rebase chain with Fixup at end"
      _ -> impossible
toRebaseSelect (ToEdit te :>: ps) = RSFwd NilFL te :>: toRebaseSelect ps

toRebaseChanges
    :: PrimPatchBase p
    => FL (RebaseItem p) wX wY
    -> FL (PatchInfoAnd (RebaseChange p)) wX wY
toRebaseChanges = mapFL_FL toChange . toRebaseSelect

toChange :: RebaseSelect p wX wY -> PatchInfoAnd (RebaseChange p) wX wY
toChange (RSFwd fixups named) =
    n2pia $
    flip adddeps (getdeps named) $
    infopatch (patch2patchinfo named) $
    (:>: NilFL) $
    RCFwd fixups (patchcontents named)
toChange (RSRev fixups named) =
    n2pia $
    flip adddeps (getdeps named) $
    infopatch (patch2patchinfo named) $
    (:>: NilFL) $
    RCRev fixups (patchcontents named)

instance PrimPatch (PrimOf p) => PrimPatchBase (RebaseChange p) where
    type PrimOf (RebaseChange p) = PrimOf p


instance Invert p => Invert (RebaseSelect p) where
   invert (RSFwd fixups edit) = RSRev fixups edit
   invert (RSRev fixups edit) = RSFwd fixups edit

instance Invert (RebaseChange p) where
   invert (RCFwd fixups contents) = RCRev fixups contents
   invert (RCRev fixups contents) = RCFwd fixups contents

instance (PrimPatchBase p, Commute p, MyEq p) => MyEq (RebaseSelect p) where
   RSFwd fixups1 edit1 =\/= RSFwd fixups2 edit2
      | IsEq <- fixups1 =\/= fixups2, IsEq <- edit1 =\/= edit2 = IsEq
   RSRev fixups1 edit1 =\/= RSRev fixups2 edit2
      | IsEq <- edit1 =/\= edit2, IsEq <- fixups1 =/\= fixups2 = IsEq

   _ =\/= _ = impossible

instance (PrimPatchBase p, FromPrim p, Effect p, Commute p, Invert p, NameHack p) => Commute (RebaseSelect p) where
   commute (RSFwd {} :> RSRev {}) = impossible
   commute (RSRev {} :> RSFwd {}) = impossible
   commute (RSRev fixups1 edit1 :> RSRev fixups2 edit2) =
      do RSFwd fixups1' edit1' :> RSFwd fixups2' edit2'
                   <- commute (RSFwd fixups2 edit2 :> RSFwd fixups1 edit1)
         return (RSRev fixups2' edit2' :> RSRev fixups1' edit1')

   commute (RSFwd fixups1 edit1 :> RSFwd fixups2 edit2)
    = do
         fixups2' :> edit1' <- commuteNamedFixups (edit1 :> fixups2)
         edit2' :> edit1'' <- commute (edit1' :> edit2)
         fixupsS :> (fixups2'' :> edit2'') :> fixups1' <- return $ pushThrough (fixups1 :> (fixups2' :> edit2'))
         return (RSFwd (fixupsS +>+ fixups2'') edit2'' :> RSFwd fixups1' edit1'')

instance Commute (RebaseChange p) where
    commute _ = impossible

instance (PrimPatchBase p, PatchInspect p) => PatchInspect (RebaseSelect p) where
   listTouchedFiles (RSFwd fixup toedit) = nub (listTouchedFiles fixup ++ listTouchedFiles toedit)
   listTouchedFiles (RSRev fixup toedit) = nub (listTouchedFiles fixup ++ listTouchedFiles toedit)

   hunkMatches f (RSFwd fixup toedit) = hunkMatches f fixup || hunkMatches f toedit
   hunkMatches f (RSRev fixup toedit) = hunkMatches f fixup || hunkMatches f toedit

instance (PrimPatchBase p, PatchInspect p) => PatchInspect (RebaseChange p) where
   listTouchedFiles (RCFwd fixup contents) = nub (listTouchedFiles fixup ++ listTouchedFiles contents)
   listTouchedFiles (RCRev fixup contents) = nub (listTouchedFiles fixup ++ listTouchedFiles contents)

   hunkMatches f (RCFwd fixup contents) = hunkMatches f fixup || hunkMatches f contents
   hunkMatches f (RCRev fixup contents) = hunkMatches f fixup || hunkMatches f contents

-- |Split a list of rebase patches into those that will
-- have conflicts if unsuspended and those that won't.
partitionUnconflicted
    :: (PrimPatchBase p, FromPrim p, Effect p, Commute p, Invert p, NameHack p)
    => FL (RebaseSelect p) wX wY
    -> (FL (RebaseSelect p) :> RL (RebaseSelect p)) wX wY
partitionUnconflicted = partitionUnconflictedAcc NilRL

partitionUnconflictedAcc :: (PrimPatchBase p, FromPrim p, Effect p, Commute p, Invert p, NameHack p)
                         => RL (RebaseSelect p) wX wY -> FL (RebaseSelect p) wY wZ
                         -> (FL (RebaseSelect p) :> RL (RebaseSelect p)) wX wZ
partitionUnconflictedAcc right NilFL = NilFL :> right
partitionUnconflictedAcc right (p :>: ps) =
   case commuterRLId selfCommuter (right :> p) of
     Just (p'@(RSFwd NilFL _) :> right')
       -> case partitionUnconflictedAcc right' ps of
            left' :> right'' -> (p' :>: left') :> right''
     _ -> partitionUnconflictedAcc (p :<: right) ps

-- | A patch, together with a list of patch names that it used to depend on,
-- but were lost during the rebasing process. The UI can use this information
-- to report them to the user.
data WithDroppedDeps p wX wY =
    WithDroppedDeps {
        wddPatch :: p wX wY,
        wddDependedOn :: [PatchInfo]
    }

noDroppedDeps :: p wX wY -> WithDroppedDeps p wX wY
noDroppedDeps p = WithDroppedDeps p []

instance PrimPatchBase p => PrimPatchBase (WithDroppedDeps p) where
   type PrimOf (WithDroppedDeps p) = PrimOf p

instance Effect p => Effect (WithDroppedDeps p) where
   effect = effect . wddPatch

-- Note, this could probably be rewritten using a generalised commuteWhatWeCanFL from
-- Darcs.Patch.Permutations.
-- |@pushThrough (ps :> (qs :> te))@ tries to commute as much of @ps@ as possible through
-- both @qs@ and @te@, giving @psStuck :> (qs' :> te') :> psCommuted@.
-- Anything that can be commuted ends up in @psCommuted@ and anything that can't goes in
-- @psStuck@.
pushThrough :: (PrimPatchBase p, FromPrim p, Effect p, Commute p, Invert p)
            => (FL (RebaseFixup p) :> (FL (RebaseFixup p) :> Named p)) wX wY
            -> (FL (RebaseFixup p) :> (FL (RebaseFixup p) :> Named p) :> FL (RebaseFixup p)) wX wY
pushThrough (NilFL :> v) = NilFL :> v :> NilFL
pushThrough ((p :>: ps) :> v) =
  case pushThrough (ps :> v) of
   psS :> v'@(qs:>te) :> ps' ->
     fromMaybe ((p :>: psS) :> v' :> ps') $ do
       psS' :> p' <- commuterIdFL selfCommuter (p :> psS)
       qs' :> p'' <- commuterIdFL selfCommuter (p' :> qs)
       te' :> p''' <- commuteFixupNamed (p'' :> te)
       return (psS' :> (qs' :> te') :> (p''' +>+ ps'))

type WDDNamed p = WithDroppedDeps (Named p)

mergerIdWDD :: MergeFn p1 p2 -> MergeFn p1 (WithDroppedDeps p2)
mergerIdWDD merger (p1 :\/: WithDroppedDeps p2 deps) =
   case merger (p1 :\/: p2) of
     p2' :/\: p1' -> WithDroppedDeps p2' deps :/\: p1'


commuterIdWDD :: CommuteFn p q -> CommuteFn p (WithDroppedDeps q)
commuterIdWDD commuter (p :> WithDroppedDeps q deps)
  = do -- no need to worry about names, because by definition a dropped dep
       -- is a name we no longer have
       -- TODO consistency checking?
       -- TODO consider inverse commutes, e.g. what happens if we wanted to
       -- commute (WithDroppedDeps ... [n] :> AddName n)?
       q' :> p' <- commuter (p :> q)
       return (WithDroppedDeps q' deps :> p')

-- |Forcibly commute a 'RebaseName' with a patch, dropping any dependencies
-- if necessary and recording them in the patch
forceCommuteName :: (RebaseName p :> WDDNamed p) wX wY -> (WDDNamed p :> RebaseName p) wX wY
forceCommuteName (AddName an :> WithDroppedDeps (NamedP pn deps body) ddeps)
  | an == pn = impossible
  | otherwise = WithDroppedDeps (NamedP pn (deps \\ [an]) (unsafeCoerceP body)) (if an `elem` deps then an:ddeps else ddeps) :> AddName an
forceCommuteName (DelName dn :> p@(WithDroppedDeps (NamedP pn deps _body) _ddeps))
  | dn == pn = impossible
  | dn `elem` deps = impossible
  | otherwise = unsafeCoerceP p :> DelName dn
forceCommuteName (Rename old new :> WithDroppedDeps (NamedP pn deps body) ddeps)
  | old == pn = impossible
  | new == pn = impossible
  | old `elem` deps = impossible
  | otherwise =
      let newdeps = map (\dep -> if new == dep then old else dep) deps
      in WithDroppedDeps (NamedP pn newdeps (unsafeCoerceP body)) ddeps :> Rename old new

forceCommutePrim :: (Commute p, Merge p, Invert p, Effect p, FromPrim p)
                 => (PrimOf p :> WDDNamed p) wX wY
                 -> (WDDNamed p :> FL (PrimOf p)) wX wY
forceCommutePrim (p :> q) =
    case mergerIdWDD (mergerIdNamed selfMerger) (invert (fromPrim p) :\/: q) of
        q' :/\: invp' -> q' :> effect (invert invp')


forceCommutesPrim :: (Commute p, Merge p, Invert p, Effect p, FromPrim p)
                  => (PrimOf p :> FL (WDDNamed p)) wX wY
                  -> (FL (WDDNamed p) :> FL (PrimOf p)) wX wY
forceCommutesPrim (p :> NilFL) = NilFL :> (p :>: NilFL)
forceCommutesPrim (p :> (q :>: qs)) =
    case forceCommutePrim (p :> q) of
        q' :> p' -> case forceCommutessPrim ( p' :> qs) of
            qs' :> p'' -> (q' :>: qs') :> p''

forceCommutessPrim :: (Commute p, Merge p, Invert p, Effect p, FromPrim p)
                   => (FL (PrimOf p) :> FL (WDDNamed p)) wX wY
                   -> (FL (WDDNamed p) :> FL (PrimOf p)) wX wY
forceCommutessPrim (NilFL :> qs) = qs :> NilFL
forceCommutessPrim ((p :>: ps) :> qs) =
    case forceCommutessPrim (ps :> qs) of
        qs' :> ps' ->
            case forceCommutesPrim (p :> qs') of
                qs'' :> p' -> qs'' :> (p' +>+ ps')

forceCommutess :: (Commute p, Merge p, Invert p, Effect p, FromPrim p)
               => (FL (RebaseFixup p) :> FL (WDDNamed p)) wX wY
               -> (FL (WDDNamed p) :> FL (RebaseFixup p)) wX wY
forceCommutess (NilFL :> qs) = qs :> NilFL
forceCommutess ((NameFixup n :>: ps) :> qs) =
    case forceCommutess (ps :> qs) of
        qs' :> ps' ->
            case totalCommuterIdFL forceCommuteName (n :> qs') of
                qs'' :> n' -> qs'' :> (NameFixup n' :>: ps')
forceCommutess ((PrimFixup p :>: ps) :> qs) =
    case forceCommutess (ps :> qs) of
        qs' :> ps' ->
            case forceCommutesPrim (p :> qs') of
                qs'' :> p' -> qs'' :> (mapFL_FL PrimFixup p' +>+ ps')

-- |Turn a selected rebase patch back into a patch we can apply to
-- the main repository, together with residual fixups that need
-- to go back into the rebase state (unless the rebase is now finished).
-- Any fixups associated with the patch will turn into conflicts.
extractRebaseSelect :: (Commute p, Merge p, Invert p, Effect p, FromPrim p, PrimPatchBase p)
                    => FL (RebaseSelect p) wX wY
                    -> (FL (WDDNamed p) :> FL (RebaseFixup p)) wX wY
extractRebaseSelect NilFL = NilFL :> NilFL
extractRebaseSelect (RSFwd fixups toedit :>: rest)
  = case extractRebaseSelect rest of
     toedits2 :> fixups2 ->
        case forceCommutess (fixups :> (WithDroppedDeps toedit [] :>: toedits2)) of
          toedits' :> fixups' ->
            toedits' :> (fixups' +>+ fixups2)

extractRebaseSelect (RSRev{} :>: _) = impossible

-- signature to be compatible with extractRebaseSelect
-- | Like 'extractRebaseSelect', but any fixups are "reified" into a separate patch.
reifyRebaseSelect :: forall p wX wY
                   . (PrimPatchBase p, Commute p, Merge p, Invert p, Effect p, FromPrim p)
                  => FL (RebaseSelect p) wX wY
                  -> IO ((FL (WDDNamed p) :> FL (RebaseFixup p)) wX wY)
reifyRebaseSelect rs = do res <- concatFL <$> mapFL_FL_M reifyOne rs
                          return (res :> NilFL)
  where reifyOne :: RebaseSelect p wA wB -> IO (FL (WDDNamed p) wA wB)
        reifyOne (RSFwd fixups toedit) =
            case flToNamesPrims fixups of
                names :> NilFL ->
                    return (mapFL_FL (noDroppedDeps . mkDummy) names +>+ noDroppedDeps toedit :>: NilFL)
                names :> prims -> do
                    n <- mkReified prims
                    return (mapFL_FL (noDroppedDeps . mkDummy) names +>+ noDroppedDeps n :>: noDroppedDeps toedit :>: NilFL)
        reifyOne (RSRev{}) = impossible

mkReified :: FromPrim p => FL (PrimOf p) wX wY -> IO (Named p wX wY)
mkReified ps = do
     let name = "Reified fixup patch"
     let desc = []
     date <- getIsoDateTime
     let author = "Invalid <invalid@invalid>"
     namepatch date name author desc (mapFL_FL fromPrim ps)

mkDummy :: RebaseName p wX wY -> Named p wX wY
mkDummy (AddName pi) = infopatch pi (unsafeCoerceP NilFL)
mkDummy (DelName _) = error "internal error: can't make a dummy patch from a delete"
mkDummy (Rename _ _) = error "internal error: can't make a dummy patch from a rename"

instance CommuteNoConflicts (RebaseChange p) where
    commuteNoConflicts _ = impossible

instance MaybeInternal (RebaseChange p)

instance IsHunk p => IsHunk (RebaseChange p) where
    -- RebaseChange is a compound patch, so it doesn't really make sense to
    -- ask whether it's a hunk. TODO: get rid of the need for this.
    isHunk _ = Nothing

instance NameHack (RebaseChange p)
instance PatchListFormat (RebaseChange p)

instance ( PrimPatchBase p, Apply p, Invert p
         , PatchInspect p
         , ApplyState p ~ ApplyState (PrimOf p)
         )
        => Matchable (RebaseChange p)
