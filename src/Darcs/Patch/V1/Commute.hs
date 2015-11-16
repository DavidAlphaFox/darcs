--  Copyright (C) 2002-2003 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}


module Darcs.Patch.V1.Commute
    (
      merge,
      merger, unravel,
      publicUnravel,
    )
       where

import Control.Monad ( MonadPlus, mplus, msum, mzero, guard )
import Control.Applicative ( Applicative(..), Alternative(..) )
import Data.Maybe ( isJust )

import Darcs.Patch.Commute ( toFwdCommute, selfCommuter )
import Darcs.Patch.CommuteFn ( commuterIdFL, commuterFLId )
import Darcs.Patch.ConflictMarking ( mangleUnravelled )
import Darcs.Util.Path ( FileName )
import Darcs.Patch.Invert ( invertRL )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Commute(..), PatchInspect(..), Invert(..) )
import Darcs.Patch.V1.Core ( Patch(..),
                             isMerger,
                             mergerUndo )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Prim ( FromPrim(..), PrimPatch,
                          is_filepatch, sortCoalesceFL,
                        )
import Darcs.Patch.Permutations ( headPermutationsRL, simpleHeadPermutationsFL )
import Darcs.Util.Printer ( text, vcat, ($$) )
import Darcs.Patch.V1.Show ( showPatch_ )
import Data.List ( nub, nubBy )
import Data.List.Ordered ( nubSort )
#include "impossible.h"
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..) , mapSeal, unseal, FlippedSeal(..), mapFlipped
    , unsafeUnseal, unsafeUnsealFlipped )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), MyEq(..) )
import Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP, unsafeCoercePStart
    , unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Ordered
    ( mapFL_FL,
    FL(..), RL(..),
    (:/\:)(..), (:<)(..), (:\/:)(..), (:>)(..),
    lengthFL, mapRL,
    reverseFL, reverseRL, concatFL
    )

data Perhaps a = Unknown | Failed | Succeeded a

instance Functor Perhaps where
    fmap _ Unknown = Unknown
    fmap _ Failed = Failed
    fmap f (Succeeded x) = Succeeded (f x)

instance Applicative Perhaps where
    pure = Succeeded
    _ <*> Failed = Failed
    _ <*> Unknown = Unknown
    Failed <*> _ = Failed
    Unknown <*> _ = Unknown
    Succeeded f <*> Succeeded x = Succeeded (f x)

instance  Monad Perhaps where
    (Succeeded x) >>= k =  k x
    Failed   >>= _      =  Failed
    Unknown  >>= _      =  Unknown
    return              =  Succeeded
    fail _              =  Unknown

instance Alternative Perhaps where
    empty = Unknown
    Unknown <|> ys    = ys
    Failed  <|> _     = Failed
    (Succeeded x) <|> _ = Succeeded x

instance  MonadPlus Perhaps where
    mzero = Unknown
    mplus = (<|>)

toMaybe :: Perhaps a -> Maybe a
toMaybe (Succeeded x) = Just x
toMaybe _ = Nothing

toPerhaps :: Maybe a -> Perhaps a
toPerhaps (Just x) = Succeeded x
toPerhaps Nothing = Failed

-- | 'cleverCommute' attempts to commute two patches @p1@ and @p2@, in their
-- original order, with the given commute function. If the commute function
-- doesn't know how to handle the patches (i.e. it returns Unknown as a
-- result), then we try again with @invert p2@ and @invert p1@ (inverting the
-- results, if succesful).
--
-- TODO: when can the first attempt fail, but the second not? What's so clever
-- in this function?
cleverCommute :: Invert prim => CommuteFunction prim -> CommuteFunction prim
cleverCommute c (p1:<p2) =
    case c (p1 :< p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed
    Unknown -> case c (invert p2 :< invert p1) of
               Succeeded (p1' :< p2') -> Succeeded (invert p2' :< invert p1')
               Failed -> Failed
               Unknown -> Unknown

-- | If we have two Filepatches which modify different files, we can return a
-- result early, since the patches trivially commute.
speedyCommute :: PrimPatch prim => CommuteFunction prim
speedyCommute (p1 :< p2)
    | isJust p1_modifies && isJust p2_modifies &&
      p1_modifies /= p2_modifies = Succeeded (unsafeCoerceP p2 :< unsafeCoerceP p1)
    | otherwise = Unknown
    where p1_modifies = isFilepatchMerger p1
          p2_modifies = isFilepatchMerger p2

everythingElseCommute :: forall prim . PrimPatch prim => CommuteFunction prim
everythingElseCommute (PP px :< PP py) = toPerhaps $ do
    x' :> y' <- commute (py :> px)
    return (PP y' :< PP x')
everythingElseCommute _xx =
        msum [
              cleverCommute commuteRecursiveMerger       _xx
             ,cleverCommute otherCommuteRecursiveMerger _xx
             ]

{-
Note that it must be true that

commutex (A^-1 A, P) = Just (P, A'^-1 A')

and

if commutex (A, B) == Just (B', A')
then commutex (B^-1, A^-1) == Just (A'^-1, B'^-1)
-}

unsafeMerger :: PrimPatch prim => String -> Patch prim wX wY -> Patch prim wX wZ -> Patch prim wA wB
unsafeMerger x p1 p2 = unsafeCoercePStart $ unsafeUnseal $ merger x p1 p2

-- | Attempt to commute two patches, the first of which is a Merger patch.
mergerCommute :: PrimPatch prim => (Patch prim :< Patch prim) wX wY -> Perhaps ((Patch prim :< Patch prim) wX wY)
mergerCommute (Merger _ _ p1 p2 :< pA)
    | unsafeCompare pA p1 = Succeeded (unsafeMerger "0.0" p2 p1 :< unsafeCoercePStart p2)
    | unsafeCompare pA (invert (unsafeMerger "0.0" p2 p1)) = Failed
mergerCommute (Merger _ _
                (Merger _ _ c b)
                (Merger _ _ c' a) :<
                Merger _ _ b' c'')
    | unsafeCompare b' b && unsafeCompare c c' && unsafeCompare c c'' =
        Succeeded (unsafeMerger "0.0" (unsafeMerger "0.0" b (unsafeCoercePStart a)) (unsafeMerger "0.0" b c) :<
                   unsafeMerger "0.0" b (unsafeCoercePStart a))
mergerCommute _ = Unknown

instance PrimPatch prim => Merge (Patch prim) where
    merge (y :\/: z) =
        case actualMerge (y:\/:z) of
        -- actualMerge returns one "arm" of a merge result (@y'@, which applies
        -- "after" @z@), but we need to return both arms. We therefore commute
        -- @z@ and @y'@, to obtain a @z'@, which applies "after" @y'' == y@.
        Sealed y' -> case commute (z :> y') of
                         Nothing -> bugDoc $ text "merge_patches bug"
                                    $$ showPatch_ y
                                   $$ showPatch_ z
                                   $$ showPatch_ y'
                         Just (_ :> z') -> unsafeCoercePStart z' :/\: y'

instance PrimPatch prim => Commute (Patch prim) where
    commute x = toMaybe $ msum
                  [toFwdCommute speedyCommute x,
                   toFwdCommute (cleverCommute mergerCommute) x,
                   toFwdCommute everythingElseCommute x
                  ]

instance PrimPatch prim => PatchInspect (Patch prim) where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (Merger _ _ p1 p2) = nubSort $ listTouchedFiles p1
                                            ++ listTouchedFiles p2
    listTouchedFiles c@(Regrem{}) = listTouchedFiles $ invert c
    listTouchedFiles (PP p) = listTouchedFiles p

    hunkMatches f (Merger _ _ p1 p2) = hunkMatches f p1 || hunkMatches f p2
    hunkMatches f c@(Regrem{}) = hunkMatches f $ invert c
    hunkMatches f (PP p) = hunkMatches f p

commuteNoMerger :: PrimPatch prim => MaybeCommute prim
commuteNoMerger x =
    toMaybe $ msum [speedyCommute x,
                    everythingElseCommute x]

isFilepatchMerger :: PrimPatch prim => Patch prim wX wY -> Maybe FileName
isFilepatchMerger (PP p) = is_filepatch p
isFilepatchMerger (Merger _ _ p1 p2) = do
     f1 <- isFilepatchMerger p1
     f2 <- isFilepatchMerger p2
     if f1 == f2 then return f1 else Nothing
isFilepatchMerger (Regrem und unw p1 p2)
    = isFilepatchMerger (Merger und unw p1 p2)

commuteRecursiveMerger :: PrimPatch prim => (Patch prim :< Patch prim) wX wY -> Perhaps ((Patch prim :< Patch prim) wX wY)
commuteRecursiveMerger (p@(Merger _ _ p1 p2) :< pA) = toPerhaps $
  do (_ :> pA') <- commuterIdFL selfCommuter (pA :> undo)
     _ <- commuterIdFL selfCommuter (pA' :> invert undo)
     (_ :> pAmid) <- commute (pA :> unsafeCoercePStart (invert p1))
     (p1' :> pAx) <- commute (pAmid :> p1)
     guard (pAx `unsafeCompare` pA)
     (p2' :> _) <- commute (pAmid :> p2)
     (p2o :> _) <- commute (invert pAmid :> p2')
     guard (p2o `unsafeCompare` p2)
     let p' = if unsafeCompare p1' p1 && unsafeCompare p2' p2
              then unsafeCoerceP p
              else unsafeMerger "0.0" p1' p2'
         undo' = mergerUndo p'
     (pAo :> _) <- commuterFLId selfCommuter (undo' :> pA')
     guard (pAo `unsafeCompare` pA)
     return (pA' :< p')
    where undo = mergerUndo p
commuteRecursiveMerger _ = Unknown

otherCommuteRecursiveMerger :: PrimPatch prim => (Patch prim :< Patch prim) wX wY -> Perhaps ((Patch prim :< Patch prim) wX wY)
otherCommuteRecursiveMerger (pA':< p_old@(Merger _ _ p1' p2')) =
  toPerhaps $
  do (pA :> _) <- commuterFLId selfCommuter (mergerUndo p_old :> pA')
     (pAmid :> p1) <- commute (unsafeCoercePEnd p1' :> pA)
     (_ :> pAmido) <- commute (pA :> invert p1)
     guard (pAmido `unsafeCompare` pAmid)
     (p2 :> _) <- commute (invert pAmid :> p2')
     (p2o' :> _) <- commute (pAmid :> p2)
     guard (p2o' `unsafeCompare` p2')
     let p = if p1 `unsafeCompare` p1' && p2 `unsafeCompare` p2'
             then unsafeCoerceP p_old
             else unsafeMerger "0.0" p1 p2
         undo = mergerUndo p
     guard (not $ pA `unsafeCompare` p1) -- special case here...
     (_ :> pAo') <- commuterIdFL selfCommuter (pA :> undo)
     guard (pAo' `unsafeCompare` pA')
     return (p :< pA)
otherCommuteRecursiveMerger _ = Unknown

type CommuteFunction prim = forall wX wY . (Patch prim :< Patch prim) wX wY -> Perhaps ((Patch prim :< Patch prim) wX wY)
type MaybeCommute prim = forall wX wY . (Patch prim :< Patch prim) wX wY -> Maybe ((Patch prim :< Patch prim) wX wY)

revCommuteFLId :: MaybeCommute prim -> (FL (Patch prim) :< Patch prim) wX wY -> Maybe ((Patch prim :< FL (Patch prim)) wX wY)
revCommuteFLId _        (NilFL :< p) = return (p :< NilFL)
revCommuteFLId commuter ((q :>: qs) :< p) = do
   p' :< q' <- commuter (q :< p)
   p'' :< qs' <- revCommuteFLId commuter (qs :< p')
   return (p'' :< (q' :>: qs'))

-- | elegantMerge attempts to perform the "intuitive" merge of two patches,
-- from a common starting context @wX@.
elegantMerge :: PrimPatch prim
              => (Patch prim :\/: Patch prim) wX wY
              -> Maybe ((Patch prim :/\: Patch prim) wX wY)
elegantMerge (p1 :\/: p2) = do
  p1' :> ip2' <- commute (invert p2 :> p1)
  p1o :> _    <- commute (p2 :> p1')
  guard $ unsafeCompare p1o p1 -- should be a redundant check
  return $ invert ip2' :/\: p1'

{-
A note about mergers and type witnesses
---------------------------------------

The merger code predates the introduction of type witnesses, and because
of its complexity has proved the hardest part of the codebase to retrofit.
Attempting to do this has exposed various places where the code behaves
oddly (e.g. 'putBefore' below); these are likely to be bugs but fixing
them would be potentially disruptive and dangerous as it might change
the existing merge behaviour and thus break existing repositories.

As a result the addition of witnesses to this code has required the
liberal use of unsafe operators. In effect, witnesses bring no safety
in this area; the sole purpose of adding them here was to allow this
code to run as part of a codebase that uses witnesses everywhere else.

A key problem point is the type of the 'Merger' and 'Regrem' constructors
of Patch, where the witnesses seem odd. It is likely that some or many
of the unsafe operations could be removed by finding a better type for
these constructors.
-}


-- |actualMerge attempts to perform a merge; if successful, it returns the
-- "right" branch of the merge
--
-- TODO: why does this code throw away the other branch, only for merge to
-- rebuild it?
actualMerge :: PrimPatch prim => (Patch prim :\/: Patch prim) wX wY -> Sealed (Patch prim wY)
actualMerge (p1 :\/: p2) = case elegantMerge (p1:\/:p2) of
                             Just (_ :/\: p1') -> Sealed p1'
                             Nothing -> merger "0.0" p2 p1

-- Recreates a patch history in reverse.
unwind :: Patch prim wX wY -> Sealed (RL (Patch prim) wX)
unwind (Merger _ unwindings _ _) = Sealed unwindings
unwind p = Sealed (p :<: NilRL)

-- Recreates a patch history in reverse. The patch being unwound is always at
-- the start of the list of patches.
trueUnwind :: PrimPatch prim => Patch prim wX wY -> Sealed (RL (Patch prim) wX)
trueUnwind p@(Merger _ _ p1 p2) =
    case (unwind p1, unwind p2) of
    (Sealed (_:<:p1s),Sealed (_:<:p2s)) ->
         Sealed (p :<: unsafeCoerceP p1 :<: unsafeUnsealFlipped (reconcileUnwindings p p1s (unsafeCoercePEnd p2s)))
    _ -> impossible
trueUnwind _ = impossible

reconcileUnwindings :: PrimPatch prim
                    => Patch prim wA wB -> RL (Patch prim) wX wZ -> RL (Patch prim) wY wZ -> FlippedSeal (RL (Patch prim)) wZ
reconcileUnwindings _ NilRL p2s = FlippedSeal p2s
reconcileUnwindings _ p1s NilRL = FlippedSeal p1s
reconcileUnwindings p (p1:<:p1s) p2s@(p2:<:tp2s) =
    case [(p1s', p2s')|
          p1s'@(hp1s':<:_) <- headPermutationsRL (p1:<:p1s),
          p2s'@(hp2s':<:_) <- headPermutationsRL p2s,
          hp1s' `unsafeCompare` hp2s'] of
    ((p1':<:p1s', _:<:p2s'):_) ->
        mapFlipped (p1' :<:) $ reconcileUnwindings p p1s' (unsafeCoercePEnd p2s')
    [] -> case reverseFL `fmap` putBefore p1 (reverseRL p2s) of
          Just p2s' -> mapFlipped (p1 :<:) $ reconcileUnwindings p p1s p2s'
          Nothing ->
              case fmap reverseFL $ putBefore p2 $
                   reverseRL (p1:<:p1s) of
              Just p1s' -> mapFlipped (p2 :<:) $
                           reconcileUnwindings p p1s' tp2s
              Nothing ->
                  bugDoc $ text "in function reconcileUnwindings"
                        $$ text "Original patch:"
                        $$ showPatch_ p
    _ -> bug "in reconcileUnwindings"

-- This code seems wrong, shouldn't the commute be invert p1 :> p2 ? And why isn't p1' re-inverted?
-- it seems to have been this way forever:
-- Fri May 23 10:27:04 BST 2003  droundy@abridgegame.org
--    * fix bug in unwind and add docs on unwind algorithm.
putBefore :: PrimPatch prim => Patch prim wY wZ -> FL (Patch prim) wX wZ -> Maybe (FL (Patch prim) wY wW)
putBefore p1 (p2:>:p2s) =
    do p1' :> p2' <- commute (unsafeCoerceP p2 :> invert p1)
       _ <- commute (p2' :> p1)
       (unsafeCoerceP p2' :>:) `fmap` putBefore p1' (unsafeCoerceP p2s)
putBefore _ NilFL = Just (unsafeCoerceP NilFL)

instance PrimPatch prim => CommuteNoConflicts (Patch prim) where
  commuteNoConflicts (x:>y) =   do x' :< y' <- commuteNoMerger (y :< x)
                                   return (y':>x')

instance PrimPatch prim => Conflict (Patch prim) where
  resolveConflicts patch = rcs NilFL (patch :<: NilRL)
    where rcs :: FL (Patch prim) wY wW -> RL (Patch prim) wX wY -> [[Sealed (FL prim wW)]]
          rcs _ NilRL = []
          rcs passedby (p@(Merger{}):<:ps) =
              case revCommuteFLId commuteNoMerger (passedby:<p) of
              Just (p'@(Merger _ _ p1 p2):<_) ->
                  map Sealed (nubBy unsafeCompare $
                        effect (unsafeCoercePStart $ unsafeUnseal (glump09 p1 p2)) : map (unsafeCoercePStart . unsafeUnseal) (unravel p'))
                  : rcs (p :>: passedby) ps
              Nothing -> rcs (p :>: passedby) ps
              _ -> impossible
          rcs passedby (p:<:ps) = seq passedby $
                                  rcs (p :>: passedby) ps

-- This type seems wrong - the most natural type for the result would seem to be
-- [Sealed (FL Prim wX)], given the type of unwind.
-- However downstream code in darcs convert assumes the wY type, and I was unable
-- to figure out whether this could/should reasonably be changed -- Ganesh 13/4/10
publicUnravel :: PrimPatch prim => Patch prim wX wY -> [Sealed (FL prim wY)]
publicUnravel = map (mapSeal unsafeCoercePStart) . unravel

unravel :: PrimPatch prim => Patch prim wX wY -> [Sealed (FL prim wX)]
unravel p = nub $ map (mapSeal (sortCoalesceFL . concatFL . mapFL_FL effect)) $
            getSupers $ map (mapSeal reverseRL) $ unseal (newUr p) $ unwind p

getSupers :: PrimPatch prim => [Sealed (FL (Patch prim) wX)] -> [Sealed (FL (Patch prim) wX)]
getSupers (x:xs) =
    case filter (not.(x `isSuperpatchOf`)) xs of
    xs' -> if any (`isSuperpatchOf` x) xs'
           then getSupers xs'
           else x : getSupers xs'
getSupers [] = []

isSuperpatchOf :: PrimPatch prim => Sealed (FL (Patch prim) wX) -> Sealed (FL (Patch prim) wX) -> Bool
Sealed x `isSuperpatchOf` Sealed y | lengthFL y > lengthFL x = False -- should be just an optimisation
Sealed x `isSuperpatchOf` Sealed y = x `iso` y
    where iso :: PrimPatch prim => FL (Patch prim) wX wY -> FL (Patch prim) wX wZ -> Bool
          _ `iso` NilFL = True
          NilFL `iso` _ = False
          a `iso` (b:>:bs) =
              head $ ([as `iso` bs | (ah :>: as) <- simpleHeadPermutationsFL a, IsEq <- [ah =\/= b]] :: [Bool]) ++ [False]

-- | merger takes two patches, (which have been determined to conflict) and
-- constructs a Merger patch to represent the conflict. @p1@ is considered to
-- be conflicting with @p2@ (@p1@ is the "first" patch in the repo ordering),
-- the resulting Merger is therefore a representation of @p2@.
merger :: PrimPatch prim => String -> Patch prim wX wY -> Patch prim wX wZ -> Sealed (Patch prim wY)
merger "0.0" p1 p2 = Sealed $ Merger undoit unwindings p1 p2
    where fake_p = Merger NilFL NilRL p1 p2
          unwindings = unsafeUnseal (trueUnwind fake_p)
          p = Merger NilFL unwindings p1 p2
          undoit =
              case (isMerger p1, isMerger p2) of
              (True ,True ) -> case unwind p of
                                 Sealed (_:<:t) -> unsafeCoerceP $ invertRL t
                                 _ -> impossible
              (False,False) -> unsafeCoerceP $ invert p1 :>: NilFL
              (True ,False) -> unsafeCoerceP NilFL
              (False,True ) -> unsafeCoerceP $ invert p1 :>: mergerUndo p2
merger g _ _ =
    error $ "Cannot handle mergers other than version 0.0\n"++g
    ++ "\nPlease use darcs optimize --modernize with an older darcs."

glump09 :: PrimPatch prim => Patch prim wX wY -> Patch prim wX wZ -> Sealed (FL (Patch prim) wY)
glump09 p1 p2 = mapSeal (mapFL_FL fromPrim) $ mangleUnravelled $ unseal unravel $ merger "0.0" p1 p2

instance PrimPatch prim => Effect (Patch prim) where
    effect p@(Merger{}) = sortCoalesceFL $ effect $ mergerUndo p
    effect p@(Regrem{}) = invert $ effect $ invert p
    effect (PP p) = p :>: NilFL

instance IsHunk prim => IsHunk (Patch prim) where
    isHunk p = do PP p' <- return p
                  isHunk p'

newUr :: PrimPatch prim => Patch prim wA wB -> RL (Patch prim) wX wY -> [Sealed (RL (Patch prim) wX)]
newUr p (Merger _ _ p1 p2 :<: ps) =
   case filter (\(pp:<:_) -> pp `unsafeCompare` p1) $ headPermutationsRL ps of
   ((_:<:ps'):_) -> newUr p (unsafeCoercePStart p1:<:ps') ++ newUr p (unsafeCoercePStart p2:<:ps')
   _ -> bugDoc $ text "in function newUr"
              $$ text "Original patch:"
              $$ showPatch_ p
              $$ text "Unwound:"
              $$ vcat (unseal (mapRL showPatch_) $ unwind p)

newUr op ps =
    case filter (\(p:<:_) -> isMerger p) $ headPermutationsRL ps of
    [] -> [Sealed ps]
    (ps':_) -> newUr op ps'

instance Invert prim => Invert (Patch prim) where
    invert (Merger undo unwindings p1 p2)
        = Regrem undo unwindings p1 p2
    invert (Regrem undo unwindings p1 p2)
        = Merger undo unwindings p1 p2
    invert (PP p) = PP (invert p)

instance MyEq prim => MyEq (Patch prim) where
    unsafeCompare = eqPatches

instance MyEq prim => Eq (Patch prim wX wY) where
    (==) = unsafeCompare

eqPatches :: MyEq prim => Patch prim wX wY -> Patch prim wW wZ -> Bool
eqPatches (PP p1) (PP p2) = unsafeCompare p1 p2
eqPatches (Merger _ _ p1a p1b) (Merger _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches (Regrem _ _ p1a p1b) (Regrem _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches _ _ = False
