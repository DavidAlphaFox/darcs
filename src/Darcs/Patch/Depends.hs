-- Copyright (C) 2003-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP , ScopedTypeVariables #-}

module Darcs.Patch.Depends
    ( getUncovered
    , areUnrelatedRepos
    , findCommonAndUncommon
    , mergeThem
    , findCommonWithThem
    , countUsThem
    , removeFromPatchSet
    , slightlyOptimizePatchset
    , getPatchesBeyondTag
    , splitOnTag
    , newsetUnion
    , newsetIntersection
    , commuteToEnd
    , findUncommon
    , merge2FL
    ) where

#include "impossible.h"

import Prelude hiding ( pi )
import Data.List ( delete, intersect, (\\) )
import Data.Maybe ( fromMaybe )

import Darcs.Patch ( Patchy, getdeps, commute, commuteFLorComplain,
                     commuteRL )
import Darcs.Patch.Info ( PatchInfo, isTag, showPatchInfoUI )
import Darcs.Patch.Merge ( Merge, mergeFL )
import Darcs.Patch.Permutations ( partitionFL, partitionRL,
                                  removeSubsequenceRL )
import Darcs.Patch.PatchInfoAnd( PatchInfoAnd, hopefully, hopefullyM, info )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, newset2RL,
                         appendPSFL )
import Darcs.Patch.Progress ( progressRL )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), (=\/=), (=/\=) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..), (:/\:)(..), (:>)(..), Fork(..),
    (+>+), mapFL, RL(..), FL(..), isShorterThanRL,
    (+<+), reverseFL, reverseRL, mapRL )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), FlippedSeal(..), flipSeal, seal )

import Darcs.Util.Printer ( renderString, vcat, RenderMode(..) )

{-
 - This module uses the following definitions:
 -
 - Explicit dependencies: the set of patches that a patch depends on "by name",
 - i.e. irrespective of (non-)commutation (non commuting patches are implicit
 - dependencies, or conflicts). In other words, the set of patch names in a tag
 - or patch recorded with --ask-deps.
 -
 - Covered: a patch p covers another, q, if p's explicit dependencies include
 - q. E.g. in a repo [a,b,t] where t is a tag and a,b have no explicit
 - dependencies, then t will cover a and b.
 -
 - "Clean" tag: a tag in a repository is clean if all patches prior to the tag
 - are (transitively-)covered by the tag. An obvious example of obtaining an
 - unclean tag is by pulling from one repo into another - the tag could have
 - been commuted past other patches. When patches are created, they are clean,
 - since they explicitly depend on all uncovered patches.
 -}

{-|
taggedIntersection takes two 'PatchSet's and splits them into a /common/
intersection portion and two sets of patches.  The intersection, however,
is only lazily determined, so there is no guarantee that all intersecting
patches will be included in the intersection 'PatchSet'.  This is a pretty
efficient function, because it makes use of the already-broken-up nature of
'PatchSet's.

Note that the first argument to taggedIntersection should be
the repository that is more cheaply accessed (i.e. local), as
taggedIntersection does its best to reduce the number of
inventories that are accessed from its rightmost argument.
-}
taggedIntersection :: forall p wStart wX wY . (Patchy p, NameHack p) =>
                      PatchSet p wStart wX -> PatchSet p wStart wY ->
                      Fork (RL (Tagged p))
                           (RL (PatchInfoAnd p))
                           (RL (PatchInfoAnd p)) wStart wX wY
taggedIntersection (PatchSet ps1 NilRL) s2 = Fork NilRL ps1 (newset2RL s2)
taggedIntersection s1 (PatchSet ps2 NilRL) = Fork NilRL (newset2RL s1) ps2
taggedIntersection s1 (PatchSet ps2 (Tagged t _ _ :<: _))
    | Just (PatchSet ps1 ts1) <- maybeSplitSetOnTag (info t) s1 =
    Fork ts1 ps1 (unsafeCoercePStart ps2)
taggedIntersection s1 s2@(PatchSet ps2 (Tagged t _ p :<: ts2)) =
    case hopefullyM t of
        Just _ -> taggedIntersection s1 (PatchSet (ps2 +<+ t :<: p) ts2)
        Nothing -> case splitOnTag (info t) s1 of
                       Just (PatchSet NilRL com :> us) ->
                           Fork com us (unsafeCoercePStart ps2)
                       Just _ -> impossible
                       Nothing -> Fork NilRL (newset2RL s1) (newset2RL s2)

-- |'maybeSplitSetOnTag' takes a tag's 'PatchInfo', @t0@, and a 'PatchSet' and
-- attempts to find @t0@ in one of the 'Tagged's in the PatchSet. If the tag is
-- found, the PatchSet is split up, on that tag, such that all later patches
-- are in the "since last tag" patch list. If the tag is not found, 'Nothing'
-- is returned.
maybeSplitSetOnTag :: PatchInfo -> PatchSet p wStart wX
                   -> Maybe (PatchSet p wStart wX)
maybeSplitSetOnTag t0 origSet@(PatchSet ps (Tagged t _ pst :<: ts))
    | t0 == info t = Just origSet
    | otherwise = do
        PatchSet ps' ts' <- maybeSplitSetOnTag t0 (PatchSet (t :<: pst) ts)
        Just $ PatchSet (ps +<+ ps') ts'
maybeSplitSetOnTag _ _ = Nothing

getPatchesBeyondTag :: (Patchy p, NameHack p) => PatchInfo -> PatchSet p wStart wX
                    -> FlippedSeal (RL (PatchInfoAnd p)) wX
getPatchesBeyondTag t (PatchSet ps (Tagged hp _ _ :<: _)) | info hp == t =
    flipSeal ps
getPatchesBeyondTag t patchset@(PatchSet (hp :<: ps) ts) =
    if info hp == t
        then if getUncovered patchset == [info hp]
                 -- special case to avoid looking at redundant patches
                 then flipSeal NilRL
                 else case splitOnTag t patchset of
                     Just (_ :> e) -> flipSeal e
                     _ -> impossible
        else case getPatchesBeyondTag t (PatchSet ps ts) of
                 FlippedSeal xxs -> FlippedSeal (hp :<: xxs)
getPatchesBeyondTag t (PatchSet NilRL NilRL) =
    bug $ "tag\n" ++ renderString Encode (showPatchInfoUI t)
          ++ "\nis not in the patchset in getPatchesBeyondTag."
getPatchesBeyondTag t0 (PatchSet NilRL (Tagged t _ ps :<: ts)) =
    getPatchesBeyondTag t0 (PatchSet (t :<: ps) ts)

-- |splitOnTag takes a tag's 'PatchInfo', and a 'PatchSet', and attempts to
-- find the tag in the PatchSet, returning a pair: the clean PatchSet "up to"
-- the tag, and a RL of patches after the tag; If the tag is not in the
-- PatchSet, we return Nothing.
splitOnTag :: (Patchy p, NameHack p) => PatchInfo -> PatchSet p wStart wX
           -> Maybe ((PatchSet p :> RL (PatchInfoAnd p)) wStart wX)
-- If the tag we are looking for is the first Tagged tag of the patchset, just
-- separate out the patchset's patches.
splitOnTag t (PatchSet ps ts@(Tagged hp _ _ :<: _)) | info hp == t =
    Just $ PatchSet NilRL ts :> ps
-- If the tag is the most recent patch in the set, we check if the patch is the
-- only non-depended-on patch in the set (i.e. it is a clean tag); creating a
-- new Tagged out of the patches and tag, and adding it to the patchset, if
-- this is the case. Otherwise, we try to make the tag clean.
splitOnTag t patchset@(PatchSet hps@(hp :<: ps) ts) | info hp == t =
    if getUncovered patchset == [t]
        then Just $ PatchSet NilRL (Tagged hp Nothing ps :<: ts) :> NilRL
        else case partitionRL ((`notElem` (t : ds)) . info) hps of
            -- Partition hps by those that are the tag and its explicit deps.
            tagAndDeps@(hp' :<: ds') :> nonDeps ->
                -- If @ds@ doesn't contain the tag of the first Tagged, that
                -- tag will also be returned by the call to getUncovered - so
                -- we need to unwrap the next Tagged in order to expose it to
                -- being partitioned out in the recursive call to splitOnTag.
                if getUncovered (PatchSet tagAndDeps ts) == [t]
                    then let tagged = Tagged hp' Nothing ds' in
                         return $ PatchSet NilRL (tagged :<: ts) :> nonDeps
                    else do
                        unfolded <- unwrapOneTagged $ PatchSet tagAndDeps ts
                        xx :> yy <- splitOnTag t unfolded
                        return $ xx :> (nonDeps +<+ yy)
            _ -> impossible
  where
    ds = getdeps (hopefully hp)
-- We drop the leading patch, to try and find a non-Tagged tag.
splitOnTag t (PatchSet (p :<: ps) ts) = do
    ns :> x <- splitOnTag t (PatchSet ps ts)
    return $ ns :> (p :<: x)
-- If there are no patches left, we "unfold" the next Tagged, and try again.
splitOnTag t0 patchset@(PatchSet NilRL (Tagged _ _ _s :<: _)) =
    unwrapOneTagged patchset >>= splitOnTag t0
-- If we've checked all the patches, but haven't found the tag, return Nothing.
splitOnTag _ (PatchSet NilRL NilRL) = Nothing

-- |'unwrapOneTagged' unfolds a single Tagged object in a PatchSet, adding the
-- tag and patches to the PatchSet's patch list.
unwrapOneTagged :: (Monad m) => PatchSet p wX wY -> m (PatchSet p wX wY)
unwrapOneTagged (PatchSet ps (Tagged t _ tps :<: ts)) =
    return $ PatchSet (ps +<+ t :<: tps) ts
unwrapOneTagged _ = fail "called unwrapOneTagged with no Tagged's in the set"

-- | @getUncovered ps@ returns the 'PatchInfo' for all the patches in
--   @ps@ that are not depended on by anything else *through explicit
--   dependencies*. Tags are a likely candidate, although we may also
--   find some non-tag patches in this list.
--
--   Keep in mind that in a typical repository with a lot of tags, only a small
--   fraction of tags would be returned as they would be at least indirectly
--   depended on by the topmost ones.
getUncovered :: PatchSet p wStart wX -> [PatchInfo]
getUncovered patchset = case patchset of
    (PatchSet ps NilRL) -> findUncovered (mapRL infoAndExplicitDeps ps)
    (PatchSet ps (Tagged t _ _ :<: _)) ->
        findUncovered (mapRL infoAndExplicitDeps (ps +<+ t :<: NilRL))
  where
    findUncovered :: [(PatchInfo, Maybe [PatchInfo])] -> [PatchInfo]
    findUncovered [] = []
    findUncovered ((pi, Nothing) : rest) = pi : findUncovered rest
    findUncovered ((pi, Just deps) : rest) =
        pi : findUncovered (dropDepsIn deps rest)

    -- |dropDepsIn traverses the list of patches, dropping any patches that
    -- occur in the dependency list; when a patch is dropped, its dependencies
    -- are added to the dependency list used for later patches.
    dropDepsIn :: [PatchInfo] -> [(PatchInfo, Maybe [PatchInfo])]
               -> [(PatchInfo, Maybe [PatchInfo])]
    dropDepsIn [] pps = pps
    dropDepsIn _  []  = []
    dropDepsIn ds (hp : pps)
        | fst hp `elem` ds =
            let extraDeps = fromMaybe [] $ snd hp in
            dropDepsIn (extraDeps ++ delete (fst hp) ds) pps
        | otherwise = hp : dropDepsIn ds pps

    -- |infoAndExplicitDeps returns the patch info and (for tags only) the list
    -- of explicit dependencies of a patch.
    infoAndExplicitDeps :: PatchInfoAnd p wX wY
                        -> (PatchInfo, Maybe [PatchInfo])
    infoAndExplicitDeps p
        | isTag (info p) = (info p, getdeps `fmap` hopefullyM p)
        | otherwise = (info p, Nothing)

-- | @slightlyOptimizePatchset@ only works on the surface inventory
--   (see 'optimizePatchset') and only optimises at most one tag in
--   there, going for the most recent tag which has no non-depended
--   patch after it. Older tags won't be 'clean', which means the
--   PatchSet will not be in 'unclean :< clean' state.
slightlyOptimizePatchset :: PatchSet p wStart wX -> PatchSet p wStart wX
slightlyOptimizePatchset (PatchSet ps0 ts0) =
    sops $ PatchSet (prog ps0) ts0
  where
    prog = progressRL "Optimizing inventory"
    sops :: PatchSet p wStart wY -> PatchSet p wStart wY
    sops patchset@(PatchSet NilRL _) = patchset
    sops patchset@(PatchSet (hp :<: ps) ts)
        | isTag (info hp) =
            if getUncovered patchset == [info hp]
                -- exactly one tag and it depends on everything not already
                -- archived
                then PatchSet NilRL (Tagged hp Nothing ps :<: ts)
                -- other tags or other top-level patches too (so move past hp)
                else let ps' = sops $ PatchSet (prog ps) ts in
                     appendPSFL ps' (hp :>: NilFL)
        | otherwise = appendPSFL (sops $ PatchSet ps ts) (hp :>: NilFL)

commuteToEnd :: forall p wStart wX wY
              . (Patchy p, NameHack p)
             => RL (PatchInfoAnd p) wX wY
               -> PatchSet p wStart wY
               -> (PatchSet p :> RL (PatchInfoAnd p)) wStart wX
commuteToEnd NilRL (PatchSet ps ts) = PatchSet NilRL ts :> ps
commuteToEnd (p :<: ps) (PatchSet xs ts) | info p `elem` mapRL info xs =
    case fastRemoveRL p xs of
        Just xs' -> commuteToEnd ps (PatchSet xs' ts)
        Nothing -> impossible -- "Nothing is impossible"
commuteToEnd ps (PatchSet xs (Tagged t _ ys :<: ts)) =
    commuteToEnd ps (PatchSet (xs +<+ t :<: ys) ts)
commuteToEnd _ _ = impossible

removeFromPatchSet :: (Patchy p, NameHack p) => FL (PatchInfoAnd p) wX wY
                   -> PatchSet p wStart wY -> Maybe (PatchSet p wStart wX)
removeFromPatchSet bad0 = rfns (reverseFL bad0)
  where
    rfns :: (Patchy p, NameHack p)
         => RL (PatchInfoAnd p) wX wY -> PatchSet p wStart wY
         -> Maybe (PatchSet p wStart wX)
    rfns bad (PatchSet ps ts)
        | all (`elem` mapRL info ps) (mapRL info bad) = do
            ps' <- removeSubsequenceRL bad ps
            Just $ PatchSet ps' ts
    rfns _ (PatchSet _ NilRL) = Nothing
    rfns bad (PatchSet ps (Tagged t _ tps :<: ts)) =
        rfns bad (PatchSet (ps +<+ t :<: tps) ts)

findCommonAndUncommon :: forall p wStart wX wY . (Patchy p, NameHack p)
                      => PatchSet p wStart wX -> PatchSet p wStart wY
                      -> Fork (PatchSet p)
                              (FL (PatchInfoAnd p))
                              (FL (PatchInfoAnd p)) wStart wX wY
findCommonAndUncommon us them = case taggedIntersection us them of
    Fork common us' them' ->
        case partitionFL (infoIn them') $ reverseRL us' of
            _ :> bad@(_ :>: _) :> _ ->
                bug $ "Failed to commute common patches:\n"
                      ++ renderString Encode
                          (vcat $ mapRL (showPatchInfoUI . info) $ reverseFL bad)
            (common2 :> NilFL :> only_ours) ->
                case partitionFL (infoIn us') $ reverseRL them' of
                    _ :> bad@(_ :>: _) :> _ ->
                        bug $ "Failed to commute common patches:\n"
                            ++ renderString Encode (vcat $
                                mapRL (showPatchInfoUI . info) $ reverseFL bad)
                    _ :> NilFL :> only_theirs ->
                        Fork (PatchSet (reverseFL common2) common)
                            only_ours (unsafeCoercePStart only_theirs)
  where
    infoIn inWhat = (`elem` mapRL info inWhat) . info

findCommonWithThem :: (Patchy p, NameHack p)
                   => PatchSet p wStart wX
                   -> PatchSet p wStart wY
                   -> (PatchSet p :> FL (PatchInfoAnd p)) wStart wX
findCommonWithThem us them = case taggedIntersection us them of
    Fork common us' them' ->
        case partitionFL ((`elem` mapRL info them') . info) $ reverseRL us' of
            _ :> bad@(_ :>: _) :> _ ->
                bug $ "Failed to commute common patches:\n"
                      ++ renderString Encode
                          (vcat $ mapRL (showPatchInfoUI . info) $ reverseFL bad)
            common2 :> _nilfl :> only_ours ->
                PatchSet (reverseFL common2) common :> unsafeCoerceP only_ours

findUncommon :: (Patchy p, NameHack p)
             => PatchSet p wStart wX -> PatchSet p wStart wY
             -> (FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wX wY
findUncommon us them =
    case findCommonWithThem us them of
        _common :> us' -> case findCommonWithThem them us of
            _ :> them' -> unsafeCoercePStart us' :\/: them'

countUsThem :: (Patchy p, NameHack p)
            => PatchSet p wStart wX
            -> PatchSet p wStart wY
            -> (Int, Int)
countUsThem us them =
    case taggedIntersection us them of
        Fork _ us' them' -> let uu = mapRL info us'
                                tt = mapRL info them' in
                            (length $ uu \\ tt, length $ tt \\ uu)

mergeThem :: (Patchy p, Merge p, NameHack p)
          => PatchSet p wStart wX -> PatchSet p wStart wY
          -> Sealed (FL (PatchInfoAnd p) wX)
mergeThem us them =
    case taggedIntersection us them of
        Fork _ us' them' ->
            case merge2FL (reverseRL us') (reverseRL them') of
               them'' :/\: _ -> Sealed them''

newsetIntersection :: (Patchy p, NameHack p)
                   => [SealedPatchSet p wStart]
                   -> SealedPatchSet p wStart
newsetIntersection [] = seal $ PatchSet NilRL NilRL
newsetIntersection [x] = x
newsetIntersection (Sealed y : ys) =
    case newsetIntersection ys of
        Sealed z -> case taggedIntersection y z of
            Fork common a b -> case mapRL info a `intersect` mapRL info b of
                morecommon ->
                    case partitionRL (\e -> info e `notElem` morecommon) a of
                        commonps :> _ -> seal $ PatchSet commonps common

newsetUnion :: (Patchy p, Merge p, NameHack p)
            => [SealedPatchSet p wStart]
            -> SealedPatchSet p wStart
newsetUnion [] = seal $ PatchSet NilRL NilRL
newsetUnion [x] = x
newsetUnion (Sealed y@(PatchSet psy tsy) : Sealed y2 : ys) =
    case mergeThem y y2 of
        Sealed p2 ->
            newsetUnion $ seal (PatchSet (reverseFL p2 +<+ psy) tsy) : ys

-- | Merge two FLs (say L and R), starting in a common context. The result is a
-- FL starting in the original end context of L, going to a new context that is
-- the result of applying all patches from R on top of patches from L.
--
-- While this function is similar to 'mergeFL', there are three important
-- differences to keep in mind:
--
-- * 'mergeFL' does not correctly deal with duplicate patches whereas this one
--   does
--   (Question from Eric Kow: in what sense? Why not fix the mergeFL instance?)
--
-- * The conventional order we use in this function is reversed from
--   'mergeFL' (so @mergeFL r l@ vs. @merge2FL l r@. This does not
--   matter so much for the former since you get both paths.
--   (Question from Eric Kow: should we flip merge2FL for more uniformity in
--    the code?)
merge2FL :: (Patchy p, Merge p, NameHack p)
         => FL (PatchInfoAnd p) wX wY
         -> FL (PatchInfoAnd p) wX wZ
         -> (FL (PatchInfoAnd p) :/\: FL (PatchInfoAnd p)) wY wZ
merge2FL xs NilFL = NilFL :/\: xs
merge2FL NilFL ys = ys :/\: NilFL
merge2FL xs (y :>: ys) | Just xs' <- fastRemoveFL y xs = merge2FL xs' ys
merge2FL (x :>: xs) ys | Just ys' <- fastRemoveFL x ys = merge2FL xs ys'
                       | otherwise = case mergeFL (x :\/: ys) of
                                         ys' :/\: x' ->
                                            case merge2FL xs ys' of
                                                ys'' :/\: xs' ->
                                                   ys'' :/\: (x' :>: xs')

areUnrelatedRepos :: (Patchy p, NameHack p)
                  => PatchSet p wStart wX
                  -> PatchSet p wStart wY -> Bool
areUnrelatedRepos us them =
    case taggedIntersection us them of
        Fork c u t -> checkit c u t
  where
    checkit (Tagged{} :<: _) _ _ = False
    checkit _ u t | t `isShorterThanRL` 5 = False
                  | u `isShorterThanRL` 5 = False
                  | otherwise = null $ intersect (mapRL info u) (mapRL info t)

-- | Remove a patch from FL, using PatchInfo equality. The result is Just
-- whenever the patch has been found and removed. If the patch is not present
-- in the sequence at all or any commutation fails, we get Nothing. First two
-- cases are optimisations for the common cases where the head of the list is
-- the patch to remove, or the patch is not there at all.
fastRemoveFL :: (Patchy p, NameHack p)
             => PatchInfoAnd p wX wY
             -> FL (PatchInfoAnd p) wX wZ -> Maybe (FL (PatchInfoAnd p) wY wZ)
fastRemoveFL _ NilFL = Nothing
fastRemoveFL a (b :>: bs) | IsEq <- a =\/= b = Just bs
                          | info a `notElem` mapFL info bs = Nothing
fastRemoveFL a (b :>: bs) = do
    a' :> bs' <- pullout NilRL bs
    a'' :> b' <- commute (b :> a')
    IsEq <- return (a'' =\/= a)
    Just (b' :>: bs')
  where
    i = info a
    pullout :: (Patchy p, NameHack p)
            => RL (PatchInfoAnd p) wA0 wA
            -> FL (PatchInfoAnd p) wA wB
            -> Maybe ((PatchInfoAnd p :> FL (PatchInfoAnd p)) wA0 wB)
    pullout _ NilFL = Nothing
    pullout acc (x :>: xs)
        | info x == i = do x' :> acc' <- commuteRL (acc :> x)
                           Just (x' :> reverseRL acc' +>+ xs)
        | otherwise = pullout (x :<: acc) xs

fastRemoveRL :: (Patchy p, NameHack p)
             => PatchInfoAnd p wY wZ
             -> RL (PatchInfoAnd p) wX wZ -> Maybe (RL (PatchInfoAnd p) wX wY)
fastRemoveRL _ NilRL = Nothing
fastRemoveRL a (b :<: bs) | IsEq <- a =/\= b = Just bs
                          | info a `notElem` mapRL info bs = Nothing
fastRemoveRL a (b :<: bs) = do
    bs' :> a' <- pullout NilFL bs
    b' :> a'' <- commute (a' :> b)
    IsEq <- return (a'' =/\= a)
    Just (b' :<: bs')
  where
    i = info a
    pullout :: (Patchy p, NameHack p)
            => FL (PatchInfoAnd p) wB wC
            -> RL (PatchInfoAnd p) wA wB
            -> Maybe ((RL (PatchInfoAnd p) :> PatchInfoAnd p) wA wC)
    pullout _ NilRL = Nothing
    pullout acc (x :<: xs)
        | info x == i = do
            acc' :> x' <- either (const Nothing)
                                 Just
                                 (commuteFLorComplain (x :> acc))
            Just (reverseFL acc' +<+ xs :> x')
        | otherwise = pullout (x :>: acc) xs
