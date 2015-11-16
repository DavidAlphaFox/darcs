-- Copyright (C) 2002-2003,2007 David Roundy
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

{-# LANGUAGE CPP #-}


module Darcs.Test.Patch.Properties.V1Set2
    ( propCommuteInverse, propPatchAndInverseIsIdentity
    , propSimpleSmartMergeGoodEnough, propCommuteEquivalency
    , propMergeValid, propInverseValid, propOtherInverseValid
    , propCommuteEitherOrder
    , propCommuteEitherWay, propCommuteTwice
    , propMergeIsCommutableAndCorrect, propMergeIsSwapable

    , checkSubcommutes
    , subcommutesInverse, subcommutesNontrivialInverse, subcommutesFailure

    , propReadShow
    -- TODO: these are exported temporarily to mark them as used
    -- Figure out whether to enable or remove the tests.
    , propUnravelThreeMerge, propUnravelSeqMerge
    , propUnravelOrderIndependent, propResolveConflictsValid
    ) where

import Prelude hiding ( pi )
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework ( Test )
import Data.Maybe ( isJust )

import Darcs.Test.Patch.Properties.Check ( Check, checkAPatch )

import Darcs.Patch ( invert, commute, merge,
                     readPatch, resolveConflicts,
                     fromPrim, showPatch )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Invert ( Invert )
import qualified Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Patch.V1.Commute ( unravel, merger )
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( Prim(..) )
import Darcs.Patch.Prim.V1.Commute ( WrappedCommuteFunction(..), Perhaps(..),
                                     subcommutes )
import Darcs.Util.Printer ( renderPS, RenderMode(..) )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), unsafeUnseal, unseal, mapSeal, Sealed2(..) )
import Darcs.Patch.Witnesses.Unsafe

#include "impossible.h"

type Patch = V1.Patch Prim


-- | Groups a set of tests by giving them the same prefix in their description.
--   When this is called as @checkSubcommutes subcoms expl@, the prefix for a
--   test becomes @"Checking " ++ expl ++ " for subcommute "@.
checkSubcommutes :: Testable a => [(String, a)] -> String
                                                 -> [Test]
checkSubcommutes subcoms expl = map check_subcommute subcoms
  where check_subcommute (name, test) =
            let testName = expl ++ " for subcommute " ++ name
            in testProperty testName test

propInverseValid :: Sealed2 (FL Patch) -> Bool
propInverseValid (Sealed2 p1) = checkAPatch (invert p1:>:p1:>:NilFL)
propOtherInverseValid :: Sealed2 (FL Patch) -> Bool
propOtherInverseValid (Sealed2 p1) = checkAPatch (p1:>:invert p1:>:NilFL)

propCommuteTwice :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteTwice (Sealed2 (p1:>p2)) =
    (doesCommute p1 p2) ==> (Just (p1:>p2) == (commute (p1:>p2) >>= commute))
doesCommute :: (MyEq p, Invert p, Commute p, Check p) => p wX wY -> p wY wZ -> Bool
doesCommute p1 p2 =
    commute (p1:>p2) /= Nothing && checkAPatch (p1:>:p2:>:NilFL)
propCommuteEquivalency :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteEquivalency (Sealed2 (p1:>p2)) =
    (doesCommute p1 p2) ==>
    case commute (p1:>p2) of
    Just (p2':>p1') -> checkAPatch (p1:>:p2:>:invert p1':>:invert p2':>:NilFL)
    _ -> impossible

propCommuteEitherWay :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteEitherWay (Sealed2 (p1:>p2)) =
    doesCommute p1 p2 ==> doesCommute (invert p2) (invert p1)

propCommuteEitherOrder :: Sealed2 (FL Patch :> FL Patch :> FL Patch) -> Property
propCommuteEitherOrder (Sealed2 (p1:>p2:>p3)) =
    checkAPatch (p1:>:p2:>:p3:>:NilFL) &&
    doesCommute p1 (p2+>+p3) &&
    doesCommute p2 p3 ==>
    case commute (p1:>p2) of
    Nothing -> False
    Just (p2':>p1') ->
        case commute (p1':>p3) of
        Nothing -> False
        Just (p3':>_) ->
            case commute (p2':>p3') of
            Nothing -> False
            Just (p3'' :> _) ->
                case commute (p2:>p3) of
                Nothing -> False
                Just (p3'a:>_) ->
                    case commute (p1:>p3'a) of
                    Just (p3''a:>_) -> isIsEq (p3''a =\/= p3'')
                    Nothing -> False

propPatchAndInverseIsIdentity :: Sealed2 (FL Patch :> FL Patch) -> Property
propPatchAndInverseIsIdentity (Sealed2 (p1:>p2)) =
    checkAPatch (p1:>:p2:>:NilFL) && (commute (p1:>p2) /= Nothing) ==>
    case commute (p1:>p2) of
    Just (p2':>_) -> case commute (invert p1:>p2') of
                    Nothing -> True -- This is a subtle distinction.
                    Just (p2'':>_) -> isIsEq (p2'' =\/= p2)
    Nothing -> impossible

propMergeIsCommutableAndCorrect :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeIsCommutableAndCorrect (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    p1' :/\: p2' ->
        case commute (p1:>p2') of
        Nothing -> False
        Just (p2'':>p1'') -> isIsEq (p2'' =\/= p2) && isIsEq (p1' =/\= p1'')
propMergeIsSwapable :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeIsSwapable (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    p1' :/\: p2' ->
           case merge (p1:\/:p2) of
           p2''' :/\: p1''' -> isIsEq (p1' =\/= p1''') && isIsEq (p2' =\/= p2''')

propMergeValid :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propMergeValid (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case merge (p2:\/:p1) of
    _ :/\: p2' ->
        checkAPatch (invert p1:>:p2:>:invert p2:>:p1:>:p2':>:NilFL)

propSimpleSmartMergeGoodEnough :: Sealed2 (FL Patch :\/: FL Patch) -> Property
propSimpleSmartMergeGoodEnough (Sealed2 (p1:\/:p2)) =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    case simpleSmartMerge (p1 :\/: p2) of
      Nothing -> True
      Just (Sealed p1'a)
       -> isJust ((do
                    p1o :> _ <- commute (p2 :> p1'a)
                    IsEq <- return $ p1o =\/= p1
                    Sealed p2'a <- simpleSmartMerge (p2 :\/: p1)
                    p2b :> p1'b <- commute (p1 :> p2'a)
                    IsEq <- return $ p2 =\/= p2b
                    IsEq <- return $ p1'a =\/= p1'b
                    return ()) :: Maybe ())

simpleSmartMerge :: (Commute p, Invert p) => (p :\/: p) wX wY -> Maybe (Sealed (p wY))
simpleSmartMerge (p1 :\/: p2) =
  case commute (invert p2 :> p1) of
  Just (p1':>_) -> Just (Sealed p1')
  Nothing -> Nothing

-- | The conflict resolution code (glump) begins by "unravelling" the merger
-- into a set of sequences of patches.  Each sequence of patches corresponds
-- to one non-conflicted patch that got merged together with the others.  The
-- result of the unravelling of a series of merges must obviously be
-- independent of the order in which those merges are performed.  This
-- unravelling code (which uses the unwind code mentioned above) uses probably
-- the second most complicated algorithm.  Fortunately, if we can successfully
-- unravel the merger, almost any function of the unravelled merger satisfies
-- the two constraints mentioned above that the conflict resolution code must
-- satisfy.
propUnravelThreeMerge :: Patch wX wY -> Patch wX wZ -> Patch wX wW -> Property
propUnravelThreeMerge p1 p2 p3 =
    checkAPatch (invert p1:>:p2:>:invert p2:>:p3:>:NilFL) ==>
    (unravel $ unsafeUnseal $ merger "0.0" (unsafeUnseal (merger "0.0" p2 p3)) (unsafeUnseal (merger "0.0" p2 p1))) ==
    (unravel $ unsafeUnseal $ merger "0.0" (unsafeUnseal (merger "0.0" p1 p3)) (unsafeUnseal (merger "0.0" p1 p2)))

propUnravelSeqMerge :: Patch wX wY -> Patch wX wZ -> Patch wZ wW -> Property
propUnravelSeqMerge p1 p2 p3 =
    checkAPatch (invert p1:>:p2:>:p3:>:NilFL) ==>
    (unravel $ unsafeUnseal $ merger "0.0" p3 $ unsafeUnseal $ merger "0.0" p2 p1) ==
    (unravel $ unsafeUnseal $ merger "0.0" (unsafeUnseal $ merger "0.0" p2 p1) p3)

propUnravelOrderIndependent :: Patch wX wY -> Patch wX wZ -> Property
propUnravelOrderIndependent p1 p2 =
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    (unravel $ unsafeCoercePStart $ unsafeUnseal $ merger "0.0" p2 p1) == (unravel $ unsafeUnseal $ merger "0.0" p1 p2)

propResolveConflictsValid :: Patch wX wY -> Patch wX wZ -> Property
propResolveConflictsValid p1 p2 =
 case merge (p1:\/:p2) of
 _ :/\: p1' ->
   let p = p2:>:p1':>:NilFL in
    checkAPatch (invert p1:>:p2:>:NilFL) ==>
    and $ map (\l -> (\ml -> checkAPatch (p+>+ml)) `unseal` mergeList l)
            $ resolveConflicts p

mergeList :: [Sealed (FL Prim wX)] -> Sealed (FL Patch wX)
mergeList patches = mapFL_FL fromPrim `mapSeal` doml NilFL patches
    where doml :: FL Prim wX wY -> [Sealed (FL Prim wX)] -> Sealed (FL Prim wX)
          doml mp (Sealed p:ps) =
              case commute (invert p :> mp) of
              Just (mp' :> _) -> doml (p +>+ mp') ps
              Nothing -> doml mp ps -- This shouldn't happen for "good" resolutions.
          doml mp [] = Sealed mp

propReadShow :: FL Patch wX wY -> Bool
propReadShow p = case readPatch $ renderPS Standard $ showPatch p of
                   Just (Sealed p') -> isIsEq (p' =\/= p)
                   Nothing -> False

-- |In order for merges to work right with commuted patches, inverting a patch
-- past a patch and its inverse had golly well better give you the same patch
-- back again.
propCommuteInverse :: Sealed2 (FL Patch :> FL Patch) -> Property
propCommuteInverse (Sealed2 (p1 :> p2)) =
    doesCommute p1 p2 ==> case commute (p1 :> p2) of
                           Nothing -> impossible
                           Just (_ :> p1') ->
                               case commute (p1' :> invert p2) of
                               Nothing -> False
                               Just (_ :> p1'') -> isIsEq (p1'' =/\= p1)

type CommuteProperty = Sealed2 (Prim :> Prim) -> Property

subcommutesInverse :: [(String, CommuteProperty)]
subcommutesInverse = zip names (map prop_subcommute cs)
    where (names, cs) = unzip subcommutes
          prop_subcommute c (Sealed2 (p1:>p2)) =
              does c p1 p2 ==>
              case runWrappedCommuteFunction c (p2:< p1) of
              Succeeded (p1':<p2') ->
                  case runWrappedCommuteFunction c (invert p2:< p1') of
                  Succeeded (p1'':<ip2x') -> isIsEq (p1'' =/\= p1) &&
                      case runWrappedCommuteFunction c (invert p1:< invert p2) of
                      Succeeded (ip2':< ip1') ->
                          case runWrappedCommuteFunction c (p2':< invert p1) of
                          Succeeded (ip1o':< p2o) -> isJust ((do
                                 IsEq <- return $ invert ip1' =/\= p1'
                                 IsEq <- return $ invert ip2' =/\= p2'
                                 IsEq <- return $ ip1o' =/\= ip1'
                                 IsEq <- return $ p2o =\/= p2
                                 IsEq <- return $ p1'' =/\= p1
                                 IsEq <- return $ ip2x' =\/= ip2'
                                 return ()) :: Maybe ())
                          _ -> False
                      _ -> False
                  _ -> False
              _ -> False

subcommutesNontrivialInverse :: [(String, CommuteProperty)]
subcommutesNontrivialInverse = zip names (map prop_subcommute cs)
    where -- speedyCommute will never be "nontrivial"
          (names, cs) = unzip . filter ((/= "speedyCommute") . fst) $ subcommutes
          prop_subcommute c (Sealed2 (p1 :> p2)) =
              nontrivial c p1 p2 ==>
              case runWrappedCommuteFunction c (p2:< p1) of
              Succeeded (p1':<p2') ->
                  case runWrappedCommuteFunction c (invert p2:< p1') of
                  Succeeded (p1'':<ip2x') -> isIsEq (p1'' =/\= p1) &&
                      case runWrappedCommuteFunction c (invert p1:< invert p2) of
                      Succeeded (ip2':< ip1') ->
                          case runWrappedCommuteFunction c (p2':< invert p1) of
                          Succeeded (ip1o':< p2o) -> isJust ((do
                              IsEq <- return $ invert ip1' =/\= p1'
                              IsEq <- return $ invert ip2' =/\= p2'
                              IsEq <- return $ ip1o' =/\= ip1'
                              IsEq <- return $ p2o =\/= p2
                              IsEq <- return $ p1'' =/\= p1
                              IsEq <- return $ ip2x' =\/= ip2'
                              return ()) :: Maybe ())
                          _ -> False
                      _ -> False
                  _ -> False
              _ -> False

subcommutesFailure :: [(String, CommuteProperty)]
subcommutesFailure = zip names (map prop cs)
    where -- speedyCommute will never fail (it just returns "Unknown")
          (names, cs) = unzip . filter ((/= "speedyCommute") . fst) $ subcommutes
          prop c (Sealed2 (p1 :> p2)) =
              doesFail c p1 p2 ==>
                case runWrappedCommuteFunction c (invert p1 :< invert p2) of
                 Failed -> True
                 _ -> False

doesFail :: WrappedCommuteFunction -> Prim wX wY -> Prim wY wZ -> Bool
doesFail c p1 p2 =
    fails (runWrappedCommuteFunction c (p2:<p1)) && checkAPatch (p1 :>: p2 :>: NilFL)
        where fails Failed = True
              fails _ = False

does :: WrappedCommuteFunction -> Prim wX wY -> Prim wY wZ -> Bool
does c p1 p2 =
    succeeds (runWrappedCommuteFunction c (p2:<p1)) && checkAPatch (p1 :>: p2 :>: NilFL)
        where succeeds (Succeeded _) = True
              succeeds _ = False

nontrivial :: WrappedCommuteFunction -> Prim wX wY -> Prim wY wZ -> Bool
nontrivial c p1 p2 =
    succeeds (runWrappedCommuteFunction c (p2:<p1)) && checkAPatch (p1 :>: p2 :>: NilFL)
        where succeeds (Succeeded (p1' :< p2')) = not (p1' `unsafeCompare` p1 && p2' `unsafeCompare` p2)
              succeeds _ = False
