-- Copyright (C) 2002-2003 David Roundy
-- Copyright (C) 2009 Ganesh Sittampalam
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}


module Darcs.Patch.Permutations ( removeFL, removeRL, removeCommon,
                                  commuteWhatWeCanFL, commuteWhatWeCanRL,
                                  genCommuteWhatWeCanRL, genCommuteWhatWeCanFL,
                                  partitionFL, partitionRL,
                                  simpleHeadPermutationsFL, headPermutationsRL,
                                  headPermutationsFL,
                                  removeSubsequenceFL, removeSubsequenceRL,
                                  partitionConflictingFL,
                                  inverseCommuter
                                ) where

import Data.Maybe ( mapMaybe )
import Darcs.Patch.Commute ( Commute, commute, commuteFLorComplain, commuteRL )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..), (+<+)
    , reverseFL, (+>+), (:\/:)(..), lengthFL
    , lengthRL, reverseRL )
#include "impossible.h"

-- |split an 'FL' into "left" and "right" lists according to a predicate @p@, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "middle" list; to sum up, we have: @all p left@ and @all (not.p) right@, while
-- midddle is mixed.
-- Note that @p@ should be invariant under commutation (i.e. if 'x1' can commute to 'x2' then 'p x1 <=> p x2').
partitionFL :: Commute p
            => (forall wU wV . p wU wV -> Bool)  -- ^predicate; if true we would like the patch in the "left" list
            -> FL p wX wY                        -- ^input 'FL'
            -> (FL p :> FL p :> FL p) wX wY      -- ^"left", "middle" and "right"

-- optimise by using an accumulating parameter to track all the "right" patches that we've found so far
partitionFL' :: Commute p
             => (forall wU wV . p wU wV -> Bool)
             -> RL p wA wB  -- the "middle" patches found so far
             -> RL p wB wC  -- the "right" patches found so far
             -> FL p wC wD
             -> (FL p :> FL p :> FL p) wA wD

partitionFL keepleft = partitionFL' keepleft NilRL NilRL

partitionFL' _ middle right NilFL = NilFL :> reverseRL middle :> reverseRL right
partitionFL' keepleft middle right (p :>: ps)
   | keepleft p = case commuteRL (right :> p) of
     Just (p' :> right') -> case commuteRL (middle :> p') of
       Just (p'' :> middle') -> case partitionFL' keepleft middle' right' ps of
         (a :> b :> c) -> p'' :>: a :> b :> c
       Nothing -> partitionFL' keepleft (p' :<: middle) right' ps
     Nothing -> case commuteWhatWeCanRL (right :> p) of
       (tomiddle :> p' :> right') -> partitionFL' keepleft (p' :<: tomiddle +<+ middle) right' ps
   | otherwise = partitionFL' keepleft middle (p :<: right) ps


-- |split an 'RL' into "left" and "right" lists according to a predicate, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "left" list.
partitionRL :: Commute p
            => (forall wU wV . p wU wV -> Bool)    -- ^predicate; if true we would like the patch in the "right" list
            -> RL p wX wY                       -- ^input 'RL'
            -> (RL p :> RL p) wX wY             -- ^"left" and "right" results

-- optimise by using an accumulating parameter to track all the "left" patches that we've found so far
partitionRL' :: Commute p
             => (forall wU wV . p wU wV -> Bool)
             -> RL p wX wZ
             -> FL p wZ wY   -- the "left" patches found so far
             -> (RL p :> RL p) wX wY

partitionRL keepright ps = partitionRL' keepright ps NilFL

partitionRL' _ NilRL qs = reverseFL qs :> NilRL

partitionRL' keepright (p :<: ps) qs
   | keepright p,
     Right (qs' :> p') <- commuteFLorComplain (p :> qs)
       = case partitionRL' keepright ps qs' of
         a :> b -> a :> p' :<: b
   | otherwise = partitionRL' keepright ps (p :>: qs)

commuteWhatWeCanFL :: Commute p => (p :> FL p) wX wY -> (FL p :> p :> FL p) wX wY
commuteWhatWeCanFL = genCommuteWhatWeCanFL commute

genCommuteWhatWeCanFL :: Commute q =>
                         (forall wA wB . ((p:>q) wA wB -> Maybe ((q:>p)wA wB)))
                          -> (p :> FL q) wX wY -> (FL q :> p :> FL q) wX wY
genCommuteWhatWeCanFL com (p :> x :>: xs) =
    case com (p :> x) of
    Nothing -> case commuteWhatWeCanFL (x :> xs) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanFL com (p :> xs1) of
                              xs1' :> p' :> xs2' -> xs1' :> p' :> xs2' +>+ x' :>: xs2
    Just (x' :> p') -> case genCommuteWhatWeCanFL com (p' :> xs) of
                       a :> p'' :> c -> x' :>: a :> p'' :> c
genCommuteWhatWeCanFL _ (y :> NilFL) = NilFL :> y :> NilFL

commuteWhatWeCanRL :: Commute p => (RL p :> p) wX wY -> (RL p :> p :> RL p) wX wY
commuteWhatWeCanRL = genCommuteWhatWeCanRL commute

genCommuteWhatWeCanRL :: Commute p =>
                         (forall wA wB . ((p :> q) wA wB -> Maybe ((q :> p) wA wB)))
                         -> (RL p :> q) wX wY -> (RL p :> q :> RL p) wX wY
genCommuteWhatWeCanRL com (x :<: xs :> p) =
    case com (x :> p) of
    Nothing -> case commuteWhatWeCanRL (xs :> x) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanRL com (xs2 :> p) of
                              xs1' :> p' :> xs2' -> xs1' +<+ x' :<: xs1 :> p' :> xs2'
    Just (p' :> x') -> case genCommuteWhatWeCanRL com (xs :> p') of
                       a :> p'' :> c -> a :> p'' :> x' :<: c
genCommuteWhatWeCanRL _ (NilRL :> y) = NilRL :> y :> NilRL


removeCommon :: (MyEq p, Commute p) => (FL p :\/: FL p) wX wY -> (FL p :\/: FL p) wX wY
removeCommon (xs :\/: NilFL) = xs :\/: NilFL
removeCommon (NilFL :\/: xs) = NilFL :\/: xs
removeCommon (xs :\/: ys) = rc xs (headPermutationsFL ys)
    where rc :: (MyEq p, Commute p) => FL p wX wY -> [(p:>FL p) wX wZ] -> (FL p :\/: FL p) wY wZ
          rc nms ((n:>ns):_) | Just ms <- removeFL n nms = removeCommon (ms :\/: ns)
          rc ms [n:>ns] = ms :\/: n:>:ns
          rc ms (_:nss) = rc ms nss
          rc _ [] = impossible -- because we already checked for NilFL case

-- | 'removeFL' @x xs@ removes @x@ from @xs@ if @x@ can be commuted to its head.
--   Otherwise it returns 'Nothing'
removeFL :: (MyEq p, Commute p) => p wX wY -> FL p wX wZ -> Maybe (FL p wY wZ)
removeFL x xs = r x $ headPermutationsFL xs
    where r :: (MyEq p, Commute p) => p wX wY -> [(p:>FL p) wX wZ] -> Maybe (FL p wY wZ)
          r _ [] = Nothing
          r z ((z':>zs):zss) | IsEq <- z =\/= z' = Just zs
                             | otherwise = r z zss

-- | 'removeRL' is like 'removeFL' except with 'RL'
removeRL :: (MyEq p, Commute p) => p wY wZ -> RL p wX wZ -> Maybe (RL p wX wY)
removeRL x xs = r x $ headPermutationsRL xs
    where r :: (MyEq p, Commute p) => p wY wZ -> [RL p wX wZ] -> Maybe (RL p wX wY)
          r z ((z':<:zs):zss) | IsEq <- z =/\= z' = Just zs
                              | otherwise = r z zss
          r _ _ = Nothing

-- | 'removeSubsequenceFL' @ab abc@ returns @Just c'@ where all the patches in
--   @ab@ have been commuted out of it, if possible.  If this is not possible
--   for any reason (the set of patches @ab@ is not actually a subset of @abc@,
--   or they can't be commuted out) we return 'Nothing'.
removeSubsequenceFL :: (MyEq p, Commute p) => FL p wA wB
                     -> FL p wA wC -> Maybe (FL p wB wC)
removeSubsequenceFL a b | lengthFL a > lengthFL b = Nothing
                         | otherwise = rsFL a b
    where rsFL :: (MyEq p, Commute p) => FL p wA wB -> FL p wA wC -> Maybe (FL p wB wC)
          rsFL NilFL ys = Just ys
          rsFL (x:>:xs) yys = removeFL x yys >>= removeSubsequenceFL xs

-- | 'removeSubsequenceRL' is like @removeSubsequenceFL@ except that it works
--   on 'RL'
removeSubsequenceRL :: (MyEq p, Commute p) => RL p wAb wAbc
                     -> RL p wA wAbc -> Maybe (RL p wA wAb)
removeSubsequenceRL a b | lengthRL a > lengthRL b = Nothing
                         | otherwise = rsRL a b
    where rsRL :: (MyEq p, Commute p) => RL p wAb wAbc -> RL p wA wAbc -> Maybe (RL p wA wAb)
          rsRL NilRL ys = Just ys
          rsRL (x:<:xs) yys = removeRL x yys >>= removeSubsequenceRL xs

-- | This is a minor variant of 'headPermutationsFL' with each permutation
--   is simply returned as a 'FL'
simpleHeadPermutationsFL :: Commute p => FL p wX wY -> [FL p wX wY]
simpleHeadPermutationsFL ps = map (\ (x:>xs) -> x:>:xs) $ headPermutationsFL ps

-- | 'headPermutationsFL' @p:>:ps@ returns all the permutations of the list
--   in which one element of @ps@ is commuted past @p@
--
--   Suppose we have a sequence of patches
--
--   >  X h a y s-t-c k
--
--   Suppose furthermore that the patch @c@ depends on @t@, which in turn
--   depends on @s@.  This function will return
--
--   > X :> h a y s t c k
--   > h :> X a y s t c k
--   > a :> X h y s t c k
--   > y :> X h a s t c k
--   > s :> X h a y t c k
--   > k :> X h a y s t c
headPermutationsFL :: Commute p => FL p wX wY -> [(p :> FL p) wX wY]
headPermutationsFL NilFL = []
headPermutationsFL (p:>:ps) =
    (p:>ps) : mapMaybe (swapfirstFL.(p:>)) (headPermutationsFL ps)
        where swapfirstFL (p1:>p2:>xs) = do p2':>p1' <- commute (p1:>p2)
                                            Just $ p2':>p1':>:xs

-- | 'headPermutationsRL' is like 'headPermutationsFL', except that we
--   operate on an 'RL' (in other words, we are pushing things to the end of a
--   patch sequence instead of to the beginning).
headPermutationsRL :: Commute p => RL p wX wY -> [RL p wX wY]
headPermutationsRL NilRL = []
headPermutationsRL (p:<:ps) =
    (p:<:ps) : mapMaybe (swapfirstRL.(p:<:)) (headPermutationsRL ps)
        where swapfirstRL (p1:<:p2:<:xs) = do p1':>p2' <- commute (p2:>p1)
                                              Just $ p2':<:p1':<:xs
              swapfirstRL _ = Nothing

instance (MyEq p, Commute p) => MyEq (FL p) where
    a =\/= b | lengthFL a /= lengthFL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: FL p wX wY -> FL p wX wZ -> EqCheck wY wZ
                   cmpSameLength (x:>:xs) xys | Just ys <- removeFL x xys = cmpSameLength xs ys
                   cmpSameLength NilFL NilFL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =/\= ys = reverseFL xs =/\= reverseFL ys

instance (MyEq p, Commute p) => MyEq (RL p) where
    unsafeCompare = bug "Buggy use of unsafeCompare on RL"
    a =/\= b | lengthRL a /= lengthRL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: RL p wX wY -> RL p wW wY -> EqCheck wX wW
                   cmpSameLength (x:<:xs) xys | Just ys <- removeRL x xys = cmpSameLength xs ys
                   cmpSameLength NilRL NilRL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =\/= ys = reverseRL xs =\/= reverseRL ys

-- |Partition a list into the patches that merge with the given patch and those that don't (including dependencies)
partitionConflictingFL :: (Commute p1, Invert p1) => CommuteFn p1 p2 -> FL p1 wX wY -> p2 wX wZ -> (FL p1 :> FL p1) wX wY
partitionConflictingFL _ NilFL _ = NilFL :> NilFL
partitionConflictingFL commuter (x :>: xs) y =
   case commuter (invert x :> y) of
     Nothing -> case commuteWhatWeCanFL (x :> xs) of
                 xs_ok :> x' :> xs_deps ->
                   case partitionConflictingFL commuter xs_ok y of
                     xs_clean :> xs_conflicts -> xs_clean :> (xs_conflicts +>+ (x' :>: xs_deps))
     Just (y' :> _) ->
       case partitionConflictingFL commuter xs y' of
          xs_clean :> xs_conflicts -> (x :>: xs_clean) :> xs_conflicts

inverseCommuter :: (Invert p, Invert q) => CommuteFn p q -> CommuteFn q p
inverseCommuter commuter (p :> q) =
   do invp' :> invq' <- commuter (invert q :> invert p)
      return (invert invq' :> invert invp')

