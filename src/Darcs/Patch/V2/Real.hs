-- Copyright (C) 2007 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP #-}


module Darcs.Patch.V2.Real
    ( RealPatch(..)
    , prim2real
    , isConsistent
    , isForward
    , isDuplicate
    , mergeUnravelled
    ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ( (*>) )
#endif

import Control.Monad ( mplus, liftM )
import qualified Data.ByteString.Char8 as BC ( ByteString, pack )
import Data.Maybe ( fromMaybe )
import Data.List ( partition, nub )

import Darcs.Patch.Commute ( commuteFL, commuteFLorComplain, commuteRL
                           , commuteRLFL )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts(..)
                            , IsConflictedPrim(..), ConflictState(..) )
import Darcs.Patch.ConflictMarking ( mangleUnravelled )
import Darcs.Patch.Debug
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(..)
                          , FileNameFormat(NewFormat) )
import Darcs.Patch.Invert ( invertFL, invertRL )
import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Prim ( FromPrim(..), ToFromPrim(..), showPrim, showPrimFL
                        , readPrim, PrimOf, PrimPatchBase, PrimPatch )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.ReadMonads ( skipSpace, string, choice )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase )
import Darcs.Patch.Repair ( mapMaybeSnd, RepairToFL(..), Check(..) )
import Darcs.Patch.Patchy ( Patchy, Apply(..), Commute(..), PatchInspect(..)
                          , ReadPatch(..), ShowPatch(..), Invert(..) )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL, commuteWhatWeCanRL
                                , genCommuteWhatWeCanRL, removeRL, removeFL
                                , removeSubsequenceFL )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Summary ( plainSummary )
import Darcs.Patch.V2.Non ( Non(..), Nonable(..), unNon, showNons, showNon
                          , readNons, readNon, commutePrimsOrAddToCtx
                          , commuteOrAddToCtx, commuteOrAddToCtxRL
                          , commuteOrRemFromCtx, commuteOrRemFromCtxFL
                          , remNons, (*>), (>*), (*>>), (>>*) )
import Data.List.Ordered ( nubSort )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..), (+>+), (+<+)
    , mapFL_FL, reverseFL, (:\/:)(..), (:/\:)(..)
    , reverseRL, lengthFL, lengthRL, nullFL )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..), Sealed(Sealed), mapSeal
    , unseal )
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..), ShowDict(..)
    , showsPrec2, appPrec
    )
import Darcs.Util.Printer.Color ( errorDoc, assertDoc )

import Darcs.Util.Printer ( Doc, blueText, redText, (<+>), ($$) )

#include "impossible.h"

-- |'RealPatch' is used to represents prim patches that are duplicates of, or
-- conflict with, another prim patch in the repository.
--
-- @Normal prim@: A primitive patch
--
-- @Duplicate x@: This patch has no effect since @x@ is already present in the
-- repository.
--
-- @Etacilpud x: invert (Duplicate x)@
--
-- @Conflictor ix xx x@:
-- @ix@ is the set of patches:
--   * that conflict with @x@ and also conflict with another patch in the
--     repository.
--   * that conflict with a patch that conflict with @x@
--
-- @xx@ is the sequence of patches that conflict *only* with @x@
--
-- @x@ is the original, conflicting patch.
--
-- @ix@ and @x@ are stored as @Non@ objects, which include any necessary
--  context to uniquely define the patch that is referred to.
--
-- The intuition is that a Conflictor should have the effect of inverting any
-- patches that 'x' conflicts with, that haven't already been undone by another
-- Conflictor in the repository.
-- Therefore, the effect of a Conflictor is @invert xx@.
--
-- @InvConflictor ix xx x@: like @invert (Conflictor ix xx x)@
data RealPatch prim wX wY where
    Duplicate :: Non (RealPatch prim) wX -> RealPatch prim wX wX
    Etacilpud :: Non (RealPatch prim) wX -> RealPatch prim wX wX
    Normal :: prim wX wY -> RealPatch prim wX wY
    Conflictor :: [Non (RealPatch prim) wX] -> FL prim wX wY
               -> Non (RealPatch prim) wX -> RealPatch prim wY wX
    InvConflictor :: [Non (RealPatch prim) wX] -> FL prim wX wY
                  -> Non (RealPatch prim) wX -> RealPatch prim wX wY

instance PrimPatch prim => PrimPatchBase (RealPatch prim) where
   type PrimOf (RealPatch prim) = prim

-- | 'isDuplicate' @p@ is @True@ if @p@ is either a 'Duplicate' or 'Etacilpud'
-- patch.
isDuplicate :: RealPatch prim wS wY -> Bool
isDuplicate (Duplicate _) = True
isDuplicate (Etacilpud _) = True
isDuplicate _ = False

-- | 'isForward' @p@ is @True@ if @p@ is either an 'InvConflictor' or
-- 'Etacilpud'.
isForward :: PrimPatch prim => RealPatch prim wS wY -> Maybe Doc
isForward p = case p of
    p@(InvConflictor{}) -> justRedP "An inverse conflictor" p
    p@(Etacilpud _) -> justRedP "An inverse duplicate" p
    _ -> Nothing
  where
    justRedP msg p = Just $ redText msg $$ showPatch p

-- |'mergeUnravelled' is used when converting from Darcs V1 patches (Mergers)
-- to Darcs V2 patches (Conflictors).
mergeUnravelled :: PrimPatch prim => [Sealed ((FL prim) wX)]
                -> Maybe (FlippedSeal (RealPatch prim) wX)
mergeUnravelled [] = Nothing
mergeUnravelled [_] = Nothing
mergeUnravelled ws =
    case mergeUnravelled_private ws of
        Nothing -> Nothing
        Just NilRL -> bug "found no patches in mergeUnravelled"
        Just (z :<: _) -> Just $ FlippedSeal z
  where
    notNullS :: PrimPatch prim => Sealed ((FL prim) wX) -> Bool
    notNullS (Sealed NilFL) = False
    notNullS _ = True

    mergeUnravelled_private :: PrimPatch prim => [Sealed (FL prim wX)]
                            -> Maybe (RL (RealPatch prim) wX wX)
    mergeUnravelled_private xs = let nonNullXs = filter notNullS xs in
        reverseFL `fmap` mergeConflictingNons (map sealed2non nonNullXs)

    -- | 'sealed2non' @(Sealed xs)@ converts @xs@ to a 'Non'.
    -- @xs@ must be non-empty since we split this list at the last patch,
    -- taking @init xs@ as the context of @last xs@.
    sealed2non :: Sealed ((FL prim) wX) -> Non (RealPatch prim) wX
    sealed2non (Sealed xs) =
        case reverseFL xs of
            y :<: ys -> Non (mapFL_FL fromPrim $ reverseRL ys) y
            NilRL -> bug "NilFL encountered in sealed2non"

mergeConflictingNons :: PrimPatch prim => [Non (RealPatch prim) wX]
                     -> Maybe (FL (RealPatch prim) wX wX)
mergeConflictingNons ns = mcn $ map unNon ns
    where mcn :: PrimPatch prim => [Sealed (FL (RealPatch prim) wX)]
              -> Maybe (FL (RealPatch prim) wX wX)
          mcn [] = Just NilFL
          -- Apparently, the joinEffects call is a safety check "and could be
          -- removed when we're sure of the code"!
          mcn [Sealed p] = case joinEffects p of
                               NilFL -> Just p
                               _ -> Nothing
          mcn (Sealed p1:Sealed p2:zs) =
            case pullCommon p1 p2 of
                Common c ps qs ->
                    case merge (ps :\/: qs) of
                        qs' :/\: _ -> mcn (Sealed (c +>+ ps +>+ qs'):zs)

joinEffects :: forall p wX wY . (Effect p, Invert (PrimOf p),
            Commute (PrimOf p), MyEq (PrimOf p)) => p wX wY
            -> FL (PrimOf p) wX wY
joinEffects = joinInverses . effect
    where joinInverses :: FL (PrimOf p) wA wB -> FL (PrimOf p) wA wB
          joinInverses NilFL = NilFL
          joinInverses (p :>: ps) =
              let ps' = joinInverses ps in
              fromMaybe (p :>: ps') $ removeFL (invert p) ps'

assertConsistent :: PrimPatch prim => RealPatch prim wX wY
                 -> RealPatch prim wX wY
assertConsistent x = flip assertDoc x $ do
    e <- isConsistent x
    Just (redText "Inconsistent patch:" $$ showPatch x $$ e)

-- | @mergeAfterConflicting@ takes as input a sequence of conflicting patches
-- @xxx@ (which therefore have no effect) and a sequence of primitive patches
-- @yyy@ that follow said sequence of conflicting patches, and may depend upon
-- some of the conflicting patches (as a resolution).

-- The output is two sequences of patches the first consisting of a set of
-- mutually-conflicting patches, and the second having the same effect as the
-- original primitive patch sequence in the input.

-- So far as I can tell, the second output is always identical to @mapFL Normal
-- yyy@

-- The first output is the set of patches from @xxx@ that are depended upon by
-- @yyy@.
mergeAfterConflicting :: PrimPatch prim => FL (RealPatch prim) wX wX
                      -> FL prim wX wY -> Maybe ( FL (RealPatch prim) wX wX
                                                 , FL (RealPatch prim) wX wY)
mergeAfterConflicting xxx yyy = mac (reverseFL xxx) yyy NilFL
  where
    mac :: PrimPatch prim
        => RL (RealPatch prim) wX wY -> FL prim wY wZ
        -> FL (RealPatch prim) wZ wA
        -> Maybe (FL (RealPatch prim) wX wX, FL (RealPatch prim) wX wA)
    mac NilRL xs goneby = case joinEffects goneby of
                              NilFL -> Just (NilFL, mapFL_FL Normal xs)
                              _ -> Nothing
    mac (p :<: ps) xs goneby =
        case commuteFLorComplain (p :> mapFL_FL Normal xs) of
            Left _ ->
                case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                    a :> p' :> b ->
                        do (b', xs') <- mac b xs goneby
                           let pa = joinEffects $ p' :<: a
                           NilFL <- return pa
                           return (reverseRL (p' :<: a) +>+ b', xs')
                        `mplus`
                        do NilFL <- return goneby
                           NilFL <- return $ joinEffects (p :<: ps)
                           return (reverseRL (p :<: ps), mapFL_FL Normal xs)
            Right (l :> p'') ->
                case allNormal l of
                    Just xs'' -> mac ps xs'' (p'' :>: goneby)
                    Nothing ->
                        case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                            a :> p' :> b ->
                                do (b', xs') <- mac b xs goneby
                                   let pa = joinEffects $ p' :<: a
                                   NilFL <- return pa
                                   return (reverseRL (p' :<: a) +>+ b', xs')

geteff :: PrimPatch prim => [Non (RealPatch prim) wX] -> FL prim wX wY
       -> ([Non (RealPatch prim) wX], FL (RealPatch prim) wX wY)
geteff _ NilFL = ([], NilFL)
geteff ix (x :>: xs) | Just ix' <- mapM (commuteOrRemFromCtx (Normal x)) ix =
    case geteff ix' xs of
        (ns, xs') -> ( non (Normal x) : map (commuteOrAddToCtx (Normal x)) ns
                     , Normal x :>: xs')
geteff ix xx =
    case mergeConflictingNons ix of
        Nothing -> errorDoc $
            redText "mergeConflictingNons failed in geteff: ix" $$
            showNons ix $$ redText "xx" $$ showPatch xx
        Just rix ->
            case mergeAfterConflicting rix xx of
                Just (a, x) ->
                    ( map (commuteOrAddToCtxRL (reverseFL a)) $ toNons x
                    , a +>+ x)
                Nothing ->
                    errorDoc $
                        redText "mergeAfterConflicting failed in geteff" $$
                        redText "where ix" $$ showNons ix $$
                        redText "and xx" $$ showPatch xx $$
                        redText "and rix" $$ showPatch rix

xx2nons :: PrimPatch prim => [Non (RealPatch prim) wX] -> FL prim wX wY
        -> [Non (RealPatch prim) wX]
xx2nons ix xx = fst $ geteff ix xx

xx2patches :: PrimPatch prim => [Non (RealPatch prim) wX] -> FL prim wX wY
           -> FL (RealPatch prim) wX wY
xx2patches ix xx = snd $ geteff ix xx

-- | If @xs@ consists only of 'Normal' patches, 'allNormal' @xs@ returns
--   @Just pxs@ those patches (so @lengthFL pxs == lengthFL xs@).
--   Otherwise, it returns 'Nothing'.
allNormal :: FL (RealPatch prim) wX wY -> Maybe (FL prim wX wY)
allNormal (Normal x :>: xs) = (x  :>: ) `fmap` allNormal xs
allNormal NilFL = Just NilFL
allNormal _ = Nothing

-- | This is used for unit-testing and for internal sanity checks
isConsistent :: PrimPatch prim => RealPatch prim wX wY -> Maybe Doc
isConsistent (Normal _) = Nothing
isConsistent (Duplicate _) = Nothing
isConsistent (Etacilpud _) = Nothing
isConsistent c@(InvConflictor{}) = isConsistent (invert c)
isConsistent (Conflictor im mm m@(Non deps _))
    | not $ everyoneConflicts im =
        Just $ redText "Someone doesn't conflict in im in isConsistent"
    | Just _ <- commuteOrRemFromCtxFL rmm m, _ :>: _ <- mm =
        Just $ redText "m doesn't conflict with mm in isConsistent"
    | any (\x -> any (x `conflictsWith`) nmm) im =
        Just $ redText "mm conflicts with im in isConsistent where nmm is" $$
               showNons nmm
    | Nothing <- (nmm ++ im) `minus` toNons deps =
        Just $ redText "dependencies not in conflict:" $$
               showNons (toNons deps) $$
               redText "compared with deps itself:" $$
               showPatch deps
    | otherwise =
        case allConflictsWith m im of
            (im1, []) | im1 `eqSet` im -> Nothing
            (_, imnc) -> Just $ redText ("m doesn't conflict with im in "
                                         ++ "isConsistent. unconflicting:") $$
                                showNons imnc
    where (nmm, rmm) = geteff im mm

everyoneConflicts :: PrimPatch prim => [Non (RealPatch prim) wX] -> Bool
everyoneConflicts [] = True
everyoneConflicts (x : xs) = case allConflictsWith x xs of
                                 ([], _) -> False
                                 (_, xs') -> everyoneConflicts xs'

prim2real :: prim wX wY -> RealPatch prim wX wY
prim2real = Normal

instance MaybeInternal (RealPatch prim)
instance NameHack (RealPatch prim)
instance RecontextRebase (RealPatch prim)
instance PrimPatch prim => Patchy (RealPatch prim)
instance PatchDebug prim => PatchDebug (RealPatch prim)

mergeWith :: PrimPatch prim => Non (RealPatch prim) wX
          -> [Non (RealPatch prim) wX] -> Sealed (FL prim wX)
mergeWith p [] = effect `mapSeal` unNon p
mergeWith p xs =
    mergeall . map unNon . (p :) . unconflicting_of $ nonDependsOrConflictsP xs
  where
    nonDependsOrConflictsP =
        filter (\x -> not ((p `dependsUpon` x) || (p `conflictsWith` x)))
    mergeall :: PrimPatch prim => [Sealed (FL (RealPatch prim) wX)]
             -> Sealed (FL prim wX)
    mergeall [Sealed x] = Sealed $ effect x
    mergeall [] = Sealed NilFL
    mergeall (Sealed x : Sealed y : rest) =
        case merge (x :\/: y) of
            y' :/\: _ -> mergeall (Sealed (x +>+ y') : rest)
    unconflicting_of [] = []
    unconflicting_of (q : qs) = case allConflictsWith q qs of
                                    ([], _) -> q : qs
                                    (_, nc) -> unconflicting_of nc

instance PrimPatch prim => Conflict (RealPatch prim) where
    conflictedEffect (Duplicate (Non _ x)) = [IsC Duplicated x]
    conflictedEffect (Etacilpud _) = impossible
    conflictedEffect (Conflictor _ _ (Non _ x)) = [IsC Conflicted x]
    conflictedEffect (InvConflictor{}) = impossible
    conflictedEffect (Normal x) = [IsC Okay x]
    resolveConflicts (Conflictor ix xx x) = [mangledUnravelled : unravelled]
      where
        mangledUnravelled = mangleUnravelled unravelled
        unravelled = nub $ filter isCons $ map (`mergeWith` xIxNonXX) xIxNonXX
        xIxNonXX = x : ix ++ nonxx
        nonxx = nonxx_ (reverseFL $ xx2patches ix xx)
        -- |nonxx_ takes an RL of patches, and returns a singleton list
        -- containing a Non, in the case where we have a Normal patch at the
        -- end of the list (using the rest of the RL as context), and an empty
        -- list otherwise.
        nonxx_ :: RL (RealPatch prim) wX wY -> [Non (RealPatch prim) wX]
        nonxx_ (Normal q :<: qs) = [Non (reverseRL qs) q]
        nonxx_ _ = []
        isCons = unseal (not . nullFL)
    resolveConflicts _ = []

instance PrimPatch prim => CommuteNoConflicts (RealPatch prim) where
    commuteNoConflicts (d1@(Duplicate _) :> d2@(Duplicate _)) = Just (d2 :> d1)
    commuteNoConflicts (e@(Etacilpud _) :> d@(Duplicate _)) = Just (d :> e)
    commuteNoConflicts (d@(Duplicate _) :> e@(Etacilpud _)) = Just (e :> d)
    commuteNoConflicts (e1@(Etacilpud _) :> e2@(Etacilpud _)) = Just (e2 :> e1)

    -- If the duplicate is @x@, as a 'Non', with @invert x@ as the context,
    -- then it is the patch the duplicate @d@ represents, so commuting results
    -- in the same two patches (since we'd make one a duplicate, and the other
    -- would become @x@ as it would no longer be duplicated).
    -- Otherwise, we commute past, or remove @invert x@ from the context of @d@
    -- to obtain a new Duplicate.
    commuteNoConflicts orig@(x :> Duplicate d) =
        if d == commuteOrAddToCtx (invert x) (non x)
            then Just orig
            else do d' <- commuteOrRemFromCtx (invert x) d
                    return (Duplicate d' :> x)

    -- Commuting a Duplicate and any other patch simply places @invert x@ into
    -- the context of the non @d@, by commuting past, or adding to the context.
    commuteNoConflicts (Duplicate d :> x) =
        Just (x :> Duplicate (commuteOrAddToCtx (invert x) d))

    -- handle Etacilpud cases by first inverting, then using the previous
    -- definitions.
    commuteNoConflicts c@(Etacilpud _ :> _) = invertCommuteNC c
    commuteNoConflicts c@(_ :> Etacilpud _) = invertCommuteNC c

    -- Two normal patches should be simply commuted (assuming the can).
    commuteNoConflicts (Normal x :> Normal y) = do
        y' :> x' <- commute (x :> y)
        return (Normal y' :> Normal x')

    -- Commuting a Normal patch past a Conflictor first commutes @x@ past the
    -- effect of the Conflictor, then commutes the resulting @x'@ past the
    -- conflicting patch and the already-undone patches. The commuting must be
    -- done in this order to make the contexts match up (@iy@ and @y@ are made
    -- in the context before @yy@ have their effect, so we need to commute past
    -- the effect of @yy@ first).
    commuteNoConflicts (Normal x :> Conflictor iy yy y) = do
        iyy' :> x' <- commuteFL (x :> invert yy)
        y' : iy' <- mapM (Normal x' >*) (y : iy)
        return (Conflictor iy' (invert iyy') y' :> Normal x')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(InvConflictor{} :> Normal _) = invertCommuteNC c

    -- Commuting a Conflictor past a Normal patch is the dual operation to
    -- commuting a Normal patch past a Conflictor.
    commuteNoConflicts (Conflictor iy yy y :> Normal x) = do
        y' : iy' <- mapM (*> Normal x) (y : iy)
        x' :> iyy' <- commuteRL (invertFL yy :> x)
        return (Normal x' :> Conflictor iy' (invertRL iyy') y')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(Normal _ :> InvConflictor{}) = invertCommuteNC c

    -- Commuting two Conflictors, c1 and c2, first commutes the Conflictors'
    -- effects, then commutes the effect of c1 and c2 and the other's
    -- already-undone, and conflicting patch, to bring the already-undone and
    -- conflicting patch into the context of the commuted effects.
    commuteNoConflicts (Conflictor ix xx x :> Conflictor iy yy y) = do
        xx' :> yy' <- commute (yy :> xx)
        x':ix' <- mapM (yy >>*) (x:ix)
        y':iy' <- mapM (*>> xx') (y:iy)
        False <- return $ any (conflictsWith y) (x':ix')
        False <- return $ any (conflictsWith x') iy
        return (Conflictor iy' yy' y' :> Conflictor ix' xx' x')

    -- Handle via the previous case, using the inverting commuter.
    commuteNoConflicts c@(InvConflictor{} :> InvConflictor{}) =
        invertCommuteNC c

    commuteNoConflicts (InvConflictor ix xx x :> Conflictor iy yy y) = do
        iyy' :> xx' <- commute (xx :> invert yy)
        y':iy' <- mapM (xx' >>*) (y:iy)
        x':ix' <- mapM (invertFL iyy' >>*) (x:ix)
        False <- return $ any (conflictsWith y') (x':ix')
        False <- return $ any (conflictsWith x') iy'
        return (Conflictor iy' (invert iyy') y' :> InvConflictor ix' xx' x')

    commuteNoConflicts (Conflictor iy' yy' y' :> InvConflictor ix' xx' x') = do
        xx :> iyy <- commute (invert yy' :> xx')
        y:iy <- mapM (*>> xx') (y':iy')
        x:ix <- mapM (*>> yy') (x':ix')
        False <- return $ any (conflictsWith y') (x':ix')
        False <- return $ any (conflictsWith x') iy'
        return (InvConflictor ix xx x :> Conflictor iy (invert iyy) y)

instance PrimPatch prim => Check (RealPatch prim) where
    isInconsistent = isConsistent

instance FromPrim (RealPatch prim) where
    fromPrim = prim2real

instance ToFromPrim (RealPatch prim) where
    toPrim (Normal p) = Just p
    toPrim _ = Nothing

instance PrimPatch prim => MyEq (RealPatch prim) where
    (Duplicate x) =\/= (Duplicate y) | x == y = IsEq
    (Etacilpud x) =\/= (Etacilpud y) | x == y = IsEq
    (Normal x) =\/= (Normal y) = x =\/= y
    (Conflictor cx xx x) =\/= (Conflictor cy yy y)
        | map commuteOrAddIXX cx `eqSet` map commuteOrAddIYY cy
          && commuteOrAddIXX x == commuteOrAddIYY y = xx =/\= yy
      where
          commuteOrAddIXX = commutePrimsOrAddToCtx (invertFL xx)
          commuteOrAddIYY = commutePrimsOrAddToCtx (invertFL yy)
    (InvConflictor cx xx x) =\/= (InvConflictor cy yy y)
        | cx `eqSet` cy && x == y = xx =\/= yy
    _ =\/= _ = NotEq

eqSet :: Eq a => [a] -> [a] -> Bool
eqSet [] [] = True
eqSet (x:xs) xys | Just ys <- remove1 x xys = eqSet xs ys
eqSet _ _ = False

remove1 :: Eq a => a -> [a] -> Maybe [a]
remove1 x (y : ys) = if x == y then Just ys else (y :) `fmap` remove1 x ys
remove1 _ [] = Nothing

minus :: Eq a => [a] -> [a] -> Maybe [a]
minus xs [] = Just xs
minus xs (y:ys) = do xs' <- remove1 y xs
                     xs' `minus` ys

invertNon :: PrimPatch prim => Non (RealPatch prim) wX
          -> Non (RealPatch prim) wX
invertNon (Non c x)
    | Just rc' <- removeRL nix (reverseFL c) = Non (reverseRL rc') (invert x)
    | otherwise = commuteOrAddToCtxRL (Normal x :<: reverseFL c) $ non nix
  where
    nix = Normal $ invert x

nonTouches :: PatchInspect prim => Non (RealPatch prim) wX -> [FilePath]
nonTouches (Non c x) = listTouchedFiles (c +>+ fromPrim x :>: NilFL)

nonHunkMatches :: PatchInspect prim => (BC.ByteString -> Bool)
               -> Non (RealPatch prim) wX -> Bool
nonHunkMatches f (Non c x) = hunkMatches f c || hunkMatches f x

toNons :: forall p wX wY . (Conflict p, Patchy p, PatchListFormat p,
       ToFromPrim p, Nonable p, ShowPatchBasic (PrimOf p), ShowPatchBasic p)
       => FL p wX wY -> [Non p wX]
toNons xs = map lastNon $ initsFL xs
    where lastNon :: Sealed ((p :> FL p) wX) -> Non p wX
          lastNon (Sealed xxx) =
              case lastNon_aux xxx of
                   deps :> p :> _ ->
                       case non p of
                           Non NilFL pp -> Non (reverseRL deps) pp
                           Non ds pp ->
                               errorDoc $ redText "Weird case in toNons" $$
                                          redText "please report this bug!" $$
                                          (case xxx of
                                           z :> zs -> showPatch (z :>: zs)) $$
                                          redText "ds are" $$ showPatch ds $$
                                          redText "pp is" $$ showPatch pp

          reverseFoo :: (p :> FL p) wX wZ -> (RL p :> p) wX wZ
          reverseFoo (p :> ps) = rf NilRL p ps
            where
              rf :: RL p wA wB -> p wB wC -> FL p wC wD
                 -> (RL p :> p) wA wD
              rf rs l NilFL = rs :> l
              rf rs x (y :>: ys) = rf (x :<: rs) y ys

          lastNon_aux :: (p :> FL p) wX wZ -> (RL p :> p :> RL p) wX wZ
          lastNon_aux = commuteWhatWeCanRL . reverseFoo

initsFL :: Patchy p => FL p wX wY -> [Sealed ((p :> FL p) wX)]
initsFL NilFL = []
initsFL (x :>: xs) =
    Sealed (x :> NilFL) :
    map (\(Sealed (y :> xs')) -> Sealed (x :> y :>: xs')) (initsFL xs)

filterConflictsFL :: PrimPatch prim => Non (RealPatch prim) wX
                  -> FL prim wX wY -> (FL prim :> FL prim) wX wY
filterConflictsFL _ NilFL = NilFL :> NilFL
filterConflictsFL n (p :>: ps)
    | Just n' <- commuteOrRemFromCtx (fromPrim p) n =
        case filterConflictsFL n' ps of
            p1 :> p2 -> p :>: p1 :> p2
    | otherwise = case commuteWhatWeCanFL (p :> ps) of
                      p1 :> p' :> p2 ->
                          case filterConflictsFL n p1 of
                              p1a :> p1b -> p1a :> p1b +>+ p' :>: p2

instance Invert prim => Invert (RealPatch prim) where
    invert (Duplicate d) = Etacilpud d
    invert (Etacilpud d) = Duplicate d
    invert (Normal p) = Normal (invert p)
    invert (Conflictor x c p) = InvConflictor x c p
    invert (InvConflictor x c p) = Conflictor x c p

instance PrimPatch prim => Commute (RealPatch prim) where
    commute (x :> y) | Just (y' :> x') <-
        commuteNoConflicts (assertConsistent x :> assertConsistent y) =
        Just (y' :> x')

    -- These patches conflicted, since we failed to commuteNoConflicts in the
    -- case above.
    commute (Normal x :> Conflictor a1'nop2 n1'x p1')
        | Just rn1' <- removeRL x (reverseFL n1'x) = do
            let p2 : n1nons = reverse $ xx2nons a1'nop2 $ reverseRL (x :<: rn1')
                a2 = p1' : a1'nop2 ++ n1nons
            case (a1'nop2, reverseRL rn1', p1') of
                ([], NilFL, Non c y) | NilFL <- joinEffects c ->
                    Just (Normal y :> Conflictor a1'nop2 (y :>: NilFL) p2)
                (a1, n1, _) ->
                    Just (Conflictor a1 n1 p1' :> Conflictor a2 NilFL p2)

    -- Handle using the inverting commuter, and the previous case.  N.b. this
    -- is innefficient, since we'll have to also try commuteNoConflicts again
    -- (which we know will fail, since we got here).
    commute c@(InvConflictor{} :> Normal _) = invertCommute c

    commute (Conflictor a1 n1 p1 :> Conflictor a2 n2 p2)
        | Just a2_minus_p1 <- remove1 p1' a2
        , not (p2 `dependsUpon` p1') = do
            let n1nons = map (commutePrimsOrAddToCtx n2) $ xx2nons a1 n1
                n2nons = xx2nons a2 n2
                Just a2_minus_p1n1 = a2_minus_p1 `minus` n1nons
                n2n1 = n2 +>+ n1
                a1' = map (commutePrimsOrAddToCtx n2) a1
                p2ooo = remNons a1' p2
            n1' :> n2' <- return $ filterConflictsFL p2ooo n2n1
            let n1'n2'nons = xx2nons a2_minus_p1n1 (n1' +>+ n2')
                n1'nons = take (lengthFL n1') n1'n2'nons
                n2'nons = drop (lengthFL n1') n1'n2'nons
                Just a1'nop2 = (a2 ++ n2nons) `minus` (p1' : n1'nons)
                Just a2'o =
                    fst (allConflictsWith p2 $ a2_minus_p1 ++ n2nons)
                    `minus` n2'nons
                Just a2' =
                    mapM (commuteOrRemFromCtxFL (xx2patches a1'nop2 n1')) a2'o
                Just p2' = commuteOrRemFromCtxFL (xx2patches a1'nop2 n1') p2
            case (a2', n2', p2') of
                ([], NilFL, Non c x) ->
                    case joinEffects c of
                        NilFL -> let n1'x = n1' +>+ x :>: NilFL in
                                 Just (Normal x :> Conflictor a1'nop2 n1'x p1')
                        _ -> impossible
                _ -> Just (c1 :> c2)
                  where
                    c1 = Conflictor a2' n2' p2'
                    c2 = Conflictor (p2 : a1'nop2) n1' p1'

        where (_, rpn2) = geteff a2 n2
              p1' = commuteOrAddToCtxRL (reverseFL rpn2) p1

    -- Handle using the inverting commuter, and the previous case. This is also
    -- innefficient, since we'll have to also try commuteNoConflicts again
    -- (which we know will fail, since we got here).
    commute c@(InvConflictor{} :> InvConflictor{}) = invertCommute c

    commute _ = Nothing

instance PrimPatch prim => Merge (RealPatch prim) where
    merge (InvConflictor{} :\/: _) = impossible
    merge (_ :\/: InvConflictor{}) = impossible
    merge (Etacilpud _ :\/: _) = impossible
    merge (_ :\/: Etacilpud _) = impossible


    merge (Duplicate a :\/: Duplicate b) = Duplicate b :/\: Duplicate a
    -- We had a FIXME comment on this case, why?
    merge (Duplicate a :\/: b) =
        b :/\: Duplicate (commuteOrAddToCtx (invert b) a)

    -- Handle using the swap merge and the previous case.
    merge m@(_ :\/: Duplicate _) = swapMerge m

    -- When merging x and y, we do a bunch of what look like "consistency"
    -- check merges. If the resulting y'' and y are equal, then we succeed.
    -- If the first case fails, we check for equal patches (which wouldn't
    -- commute) and return a Duplicate on both sides of the merge, in that
    -- case.
    merge (x :\/: y)
        | Just (y' :> ix') <-
            commute (invert (assertConsistent x) :> assertConsistent y)
        , Just (y'' :> _) <- commute (x :> y')
        , IsEq <- y'' =\/= y =
            assertConsistent y' :/\: invert (assertConsistent ix')
        -- If we detect equal patches, we have a duplicate.
        | IsEq <- x =\/= y
        , n <- commuteOrAddToCtx (invert x) $ non x =
            Duplicate n :/\: Duplicate n

    -- We know that these two patches conflict, and aren't Duplicates, since we
    -- failed the previous case. We therefore create basic Conflictors, which
    -- undo the other patch.
    merge (nx@(Normal x) :\/: ny@(Normal y)) = cy :/\: cx
      where
        cy = Conflictor [] (x :>: NilFL) (non ny)
        cx = Conflictor [] (y :>: NilFL) (non nx)

    -- If a Normal patch @x@ and a Conflictor @cy@ conflict, we add @x@ to the
    -- effect of @cy@ on one side, and create a Conflictor that has no effect,
    -- but has the already-undone and conflicted patch of @cy@ and some foos as
    -- the already-undone on the other side.
    --
    -- TODO: what is foo?
    -- Why do we need nyy? I think @x'@ is @x@ in the context of @yy@.
    merge (Normal x :\/: Conflictor iy yy y) =
          Conflictor iy yyx y :/\: Conflictor (y : iy ++ nyy) NilFL x'
              where yyx = yy +>+ x :>: NilFL
                    (x' : nyy) = reverse $ xx2nons iy yyx

    -- Handle using the swap merge and the previous case.
    merge m@(Conflictor{} :\/: Normal _) = swapMerge m

    -- mH see also cH
    merge (Conflictor ix xx x :\/: Conflictor iy yy y) =
        case pullCommonRL (reverseFL xx) (reverseFL yy) of
            CommonRL rxx1 ryy1 c ->
                case commuteRLFL (ryy1 :> invertRL rxx1) of
                    Just (ixx' :> ryy') ->
                        let xx' = invert ixx'
                            yy' = reverseRL ryy'
                            y' : iy' =
                                map (commutePrimsOrAddToCtx xx') (y : iy)
                            x' : ix' =
                                map (commutePrimsOrAddToCtx ryy') (x : ix)
                            nyy' = xx2nons iy' yy'
                            nxx' = xx2nons ix' xx'
                            icx = drop (lengthRL rxx1) $
                                xx2nons ix (reverseRL $ c +<+ rxx1)
                            ic' = map (commutePrimsOrAddToCtx ryy') icx
                            -- +++ is a more efficient version of nub (iy' ++
                            -- ix') given that we know each element shows up
                            -- only once in either list.
                            ixy' = ic' ++ (iy' +++ ix')
                            c1 = Conflictor (x' : ixy' ++ nxx') yy' y'
                            c2 = Conflictor (y' : ixy' ++ nyy') xx' x' in
                            c1 :/\: c2
                    Nothing -> impossible

instance PatchInspect prim => PatchInspect (RealPatch prim) where
    listTouchedFiles (Duplicate p) = nonTouches p
    listTouchedFiles (Etacilpud p) = nonTouches p
    listTouchedFiles (Normal p) = listTouchedFiles p
    listTouchedFiles (Conflictor x c p) =
        nubSort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p
    listTouchedFiles (InvConflictor x c p) =
        nubSort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p

    hunkMatches f (Duplicate p) = nonHunkMatches f p
    hunkMatches f (Etacilpud p) = nonHunkMatches f p
    hunkMatches f (Normal p) = hunkMatches f p
    hunkMatches f (Conflictor x c p) =
        any (nonHunkMatches f) x || hunkMatches f c || nonHunkMatches f p
    hunkMatches f (InvConflictor x c p) =
        any (nonHunkMatches f) x || hunkMatches f c || nonHunkMatches f p

allConflictsWith :: PrimPatch prim => Non (RealPatch prim) wX
                 -> [Non (RealPatch prim) wX]
                 -> ([Non (RealPatch prim) wX], [Non (RealPatch prim) wX])
allConflictsWith x ys = acw $ partition (conflictsWith x) ys
  where
    acw ([], nc) = ([], nc)
    acw (c:cs, nc) = case allConflictsWith c nc of
                         (c1, nc1) -> case acw (cs, nc1) of
                                          (xs', nc') -> (c : c1 ++ xs', nc')

conflictsWith :: PrimPatch prim => Non (RealPatch prim) wX
              -> Non (RealPatch prim) wX -> Bool
conflictsWith x y | x `dependsUpon` y || y `dependsUpon` x = False
conflictsWith x (Non cy y) =
    case commuteOrRemFromCtxFL cy x of
        Just (Non cx' x') ->
            let iy = fromPrim $ invert y in
            case commuteFLorComplain (iy :> cx' +>+ fromPrim x' :>: NilFL) of
                Right _ -> False
                Left _ -> True
        Nothing -> True

dependsUpon :: PrimPatch prim => Non (RealPatch prim) wX
            -> Non (RealPatch prim) wX -> Bool
dependsUpon (Non xs _) (Non ys y) =
    case removeSubsequenceFL (ys +>+ fromPrim y :>: NilFL) xs of
        Just _ -> True
        Nothing -> False

(+++) :: Eq a => [a] -> [a] -> [a]
[] +++ x = x
x +++ [] = x
(x:xs) +++ xys | Just ys <- remove1 x xys = x : (xs +++ ys)
               | otherwise = x : (xs +++ xys)

swapMerge :: PrimPatch prim => (RealPatch prim :\/: RealPatch prim) wX wY
          -> (RealPatch prim :/\: RealPatch prim) wX wY
swapMerge (x :\/: y) = case merge (y :\/: x) of x' :/\: y' -> y' :/\: x'

invertCommute :: PrimPatch prim => (RealPatch prim :> RealPatch prim) wX wY
              -> Maybe ((RealPatch prim :> RealPatch prim) wX wY)
invertCommute (x :> y) = do ix' :> iy' <- commute (invert y :> invert x)
                            return (invert iy' :> invert ix')

invertCommuteNC :: PrimPatch prim => (RealPatch prim :> RealPatch prim) wX wY
                -> Maybe ((RealPatch prim :> RealPatch prim) wX wY)
invertCommuteNC (x :> y) = do
    ix' :> iy' <- commuteNoConflicts (invert y :> invert x)
    return (invert iy' :> invert ix')

-- | 'pullCommon' @xs ys@ returns the set of patches that can be commuted out
-- of both @xs@ and @ys@ along with the remnants of both lists
pullCommon :: (Patchy p, MyEq p) => FL p wO wX -> FL p wO wY -> Common p wO wX wY
pullCommon NilFL ys = Common NilFL NilFL ys
pullCommon xs NilFL = Common NilFL xs NilFL
pullCommon (x :>: xs) xys | Just ys <- removeFL x xys =
    case pullCommon xs ys of
        Common c xs' ys' -> Common (x :>: c) xs' ys'
pullCommon (x :>: xs) ys =
    case commuteWhatWeCanFL (x :> xs) of
        xs1 :> x' :> xs2 -> case pullCommon xs1 ys of
            Common c xs1' ys' -> Common c (xs1' +>+ x' :>: xs2) ys'

-- | 'Common' @cs xs ys@ represents two sequences of patches that have @cs@ in
-- common, in other words @cs +>+ xs@ and @cs +>+ ys@
data Common p wO wX wY where
    Common :: FL p wO wI -> FL p wI wX -> FL p wI wY -> Common p wO wX wY

-- | 'pullCommonRL' @xs ys@ returns the set of patches that can be commuted
--   out of both @xs@ and @ys@ along with the remnants of both lists
pullCommonRL :: (Patchy p, MyEq p) => RL p wX wO -> RL p wY wO -> CommonRL p wX wY wO
pullCommonRL NilRL ys = CommonRL NilRL ys NilRL
pullCommonRL xs NilRL = CommonRL xs NilRL NilRL
pullCommonRL (x :<: xs) xys | Just ys <- removeRL x xys =
    case pullCommonRL xs ys of
        CommonRL xs' ys' c -> CommonRL xs' ys' (x :<: c)
pullCommonRL (x :<: xs) ys =
    case commuteWhatWeCanRL (xs :> x) of
        xs1 :> x' :> xs2 ->
            case pullCommonRL xs2 ys of
                CommonRL xs2' ys' c -> CommonRL (xs2' +<+ x' :<: xs1) ys' c

-- | 'CommonRL' @xs ys cs@' represents two sequences of patches that have @cs@
-- in common, in other words @xs +<+ cs@ and @ys +<+ cs@
data CommonRL p wX wY wF where
    CommonRL :: RL p wX wI -> RL p wY wI -> RL p wI wF -> CommonRL p wX wY wF

instance PrimPatch prim => Apply (RealPatch prim) where
    type ApplyState (RealPatch prim) = ApplyState prim
    apply p = apply (effect p)

instance PrimPatch prim => RepairToFL (RealPatch prim) where
    applyAndTryToFixFL (Normal p) =
        mapMaybeSnd (mapFL_FL Normal) `liftM` applyAndTryToFixFL p
    applyAndTryToFixFL x = do apply x; return Nothing

instance PatchListFormat (RealPatch prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect RealPatch to be used with any other
   -- argument anyway, so it doesn't matter.
    patchListFormat = ListFormatV2

duplicate, etacilpud, conflictor, rotcilfnoc :: String
duplicate = "duplicate"
etacilpud = "etacilpud"
conflictor = "conflictor"
rotcilfnoc = "rotcilfnoc"

instance PrimPatch prim => ShowPatchBasic (RealPatch prim) where
    showPatch (Duplicate d) = blueText duplicate $$ showNon d
    showPatch (Etacilpud d) = blueText etacilpud $$ showNon d
    showPatch (Normal p) = showPrim NewFormat p
    showPatch (Conflictor i NilFL p) =
        blueText conflictor <+> showNons i <+> blueText "[]" $$ showNon p
    showPatch (Conflictor i cs p) =
        blueText conflictor <+> showNons i <+> blueText "[" $$
        showPrimFL NewFormat cs $$
        blueText "]" $$
        showNon p
    showPatch (InvConflictor i NilFL p) =
        blueText rotcilfnoc <+> showNons i <+> blueText "[]" $$ showNon p
    showPatch (InvConflictor i cs p) =
        blueText rotcilfnoc <+> showNons i <+> blueText "[" $$
        showPrimFL NewFormat cs $$
        blueText "]" $$
        showNon p

instance PrimPatch prim => ShowPatch (RealPatch prim) where
    showContextPatch (Normal p) = showContextPatch p
    showContextPatch c = return $ showPatch c
    summary = plainSummary
    summaryFL = plainSummary
    thing _ = "change"

instance PrimPatch prim => ReadPatch (RealPatch prim) where
    readPatch' = do
        skipSpace
        let str = string . BC.pack
            readConflictorPs = do
               i <- readNons
               ps <- bracketedFL (readPrim NewFormat) '[' ']'
               p <- readNon
               return (i, ps, p)
        choice [ do str duplicate
                    p <- readNon
                    return $ Sealed $ Duplicate p
               , do str etacilpud
                    p <- readNon
                    return $ Sealed $ Etacilpud p
               , do str conflictor
                    (i, Sealed ps, p) <- readConflictorPs
                    return $ Sealed $ Conflictor i (unsafeCoerceP ps) p
               , do str rotcilfnoc
                    (i, Sealed ps, p) <- readConflictorPs
                    return $ Sealed $ InvConflictor i ps p
               , do Sealed p <- readPrim NewFormat
                    return $ Sealed $ Normal p
               ]

instance Show2 prim => Show (RealPatch prim wX wY) where
    showsPrec d (Normal prim) =
        showParen (d > appPrec) $ showString "Darcs.Patch.V2.Real.Normal " . showsPrec2 (appPrec + 1) prim

    showsPrec d (Duplicate x) =
        showParen (d > appPrec) $ showString "Darcs.Patch.V2.Real.Duplicate " . showsPrec (appPrec + 1) x

    showsPrec d (Etacilpud x) =
        showParen (d > appPrec) $ showString "Darcs.Patch.V2.Etacilpud " . showsPrec (appPrec + 1) x

    showsPrec d (Conflictor ix xx x) =
        showParen (d > appPrec) $
            showString "Darcs.Patch.V2.Real.Conflictor " . showsPrec (appPrec + 1) ix .
            showString " " . showsPrec (appPrec + 1) xx .
            showString " " . showsPrec (appPrec + 1) x

    showsPrec d (InvConflictor ix xx x) =
        showParen (d > appPrec) $
            showString "Darcs.Patch.V2.Real.InvConflictor " . showsPrec (appPrec + 1) ix .
            showString " " . showsPrec (appPrec + 1) xx .
            showString " " . showsPrec (appPrec + 1) x

instance Show2 prim => Show1 (RealPatch prim wX) where
    showDict1 = ShowDictClass

instance Show2 prim => Show2 (RealPatch prim) where
    showDict2 = ShowDictClass

instance PrimPatch prim => Nonable (RealPatch prim) where
    non (Duplicate d) = d
    non (Etacilpud d) = invertNon d -- FIXME !!! ???
    non (Normal p) = Non NilFL p
    non (Conflictor _ xx x) = commutePrimsOrAddToCtx (invertFL xx) x
    non (InvConflictor _ _ n) = invertNon n

instance PrimPatch prim => Effect (RealPatch prim) where
    effect (Duplicate _) = NilFL
    effect (Etacilpud _) = NilFL
    effect (Normal p) = p :>: NilFL
    effect (Conflictor _ e _) = invert e
    effect (InvConflictor _ e _) = e
    effectRL (Duplicate _) = NilRL
    effectRL (Etacilpud _) = NilRL
    effectRL (Normal p) = p :<: NilRL
    effectRL (Conflictor _ e _) = invertFL e
    effectRL (InvConflictor _ e _) = reverseFL e

instance IsHunk prim => IsHunk (RealPatch prim) where
    isHunk rp = do Normal p <- return rp
                   isHunk p
