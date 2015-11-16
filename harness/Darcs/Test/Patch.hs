{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, ImpredicativeTypes #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif
--  Copyright (C) 2002-2005,2007 David Roundy
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

module Darcs.Test.Patch ( testSuite ) where

import Data.Maybe( isNothing )
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck.Arbitrary( Arbitrary )
import Test.QuickCheck( Testable )
import Test.HUnit ( assertBool )

import Darcs.Test.Util.TestResult ( TestResult, isOk, fromMaybe )
import Darcs.Test.Patch.Utils ( testConditional )

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq ( MyEq, unsafeCompare )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Prim( PrimPatch, coalesce, FromPrim, PrimOf, PrimPatchBase )
import qualified Darcs.Patch.Prim.V1 as V1 ( Prim )
import qualified Darcs.Patch.Prim.V3 as V3 ( Prim )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Type ( PatchType(..) )
import Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Patch.V2.Real ( isConsistent, isForward, RealPatch )
import Darcs.Patch.Patchy ( Commute(..), Patchy )
import Darcs.Patch.Merge( Merge )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Apply( ApplyState )

import Darcs.Test.Patch.Arbitrary.Generic
import qualified Darcs.Test.Patch.Arbitrary.PrimV1 as P1
import Darcs.Test.Patch.Arbitrary.PrimV3()
import Darcs.Test.Patch.Arbitrary.Real
import Darcs.Test.Patch.Arbitrary.PatchV1 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState( WithState, WithStartState )

import qualified Darcs.Test.Patch.Info

import qualified Darcs.Test.Patch.Examples.Set1 as Ex
import qualified Darcs.Test.Patch.Examples.Set2Unwitnessed as ExU

import Darcs.Test.Patch.Properties.Check( Check(..) )
import qualified Darcs.Test.Patch.Properties.V1Set1 as Prop1
import qualified Darcs.Test.Patch.Properties.V1Set2 as Prop2
import qualified Darcs.Test.Patch.Properties.Generic as PropG
import qualified Darcs.Test.Patch.Properties.Real as PropR
import qualified Darcs.Test.Patch.Properties.GenericUnwitnessed as PropU

import qualified Darcs.Test.Patch.Rebase as Rebase

import qualified Darcs.Test.Patch.WSub as WSub


type instance ModelOf (FL prim) = ModelOf prim

type TestGenerator thing gen = (forall t ctx . ((forall wXx wYy . thing wXx wYy -> t) -> (gen ctx -> t)))
type TestCondition thing = (forall wYy wZz . thing wYy wZz -> Bool)
type TestCheck thing t = (forall wYy wZz . thing wYy wZz -> t)

-- arbitraryThing :: (forall wXx wYy . thing wXx wYy -> t) -> (thing wA wB -> t)
arbitraryThing :: x -> TestGenerator thing (thing x)
arbitraryThing _ f p = f p

-- | Run a test function on a set of data, using HUnit. The test function should
--   return @Nothing@ upon success and a @Just x@ upon failure.
testCases :: Show a => String               -- ^ The test name
             -> (a -> TestResult)  -- ^ The test function
             -> [a]                -- ^ The test data
             -> Test
testCases name test datas = testCase name (assertBool assertName res)
    where assertName = "Boolean assertion for \"" ++ name ++ "\""
          res        = and $ map (isOk . test) datas

unit_V1P1:: [Test]
unit_V1P1 =
  [ testCases "known commutes" Prop1.checkCommute Ex.knownCommutes
  , testCases "known non-commutes" Prop1.checkCantCommute Ex.knownCantCommutes
  , testCases "known merges" Prop1.checkMerge Ex.knownMerges
  , testCases "known merges (equiv)" Prop1.checkMergeEquiv Ex.knownMergeEquivs
  , testCases "known canons" Prop1.checkCanon Ex.knownCanons
  , testCases "merge swaps" Prop1.checkMergeSwap Ex.mergePairs2
  , testCases "the patch validation works" Prop1.tTestCheck Ex.validPatches
  , testCases "commute/recommute" (PropG.recommute commute) Ex.commutePairs
  , testCases "merge properties: merge either way valid" Prop1.tMergeEitherWayValid Ex.mergePairs
  , testCases "merge properties: merge swap" PropG.mergeEitherWay Ex.mergePairs
  , testCases "primitive patch IO functions" (Prop1.tShowRead eqFLUnsafe) Ex.primitiveTestPatches
  , testCases "IO functions (test patches)" (Prop1.tShowRead eqFLUnsafe) Ex.testPatches
  , testCases "IO functions (named test patches)" (Prop1.tShowRead unsafeCompare) Ex.testPatchesNamed
  , testCases "primitive commute/recommute" (PropG.recommute commute) Ex.primitiveCommutePairs
  ]

unit_V2P1 :: [Test]
unit_V2P1 =
  [ testCases "coalesce commute" (PropU.coalesceCommute WSub.coalesce) ExU.primPermutables
  , testCases "prim recommute" (PropU.recommute WSub.commute) ExU.commutables
  , testCases "prim patch and inverse commute" (PropU.patchAndInverseCommute WSub.commute) ExU.commutables
  , testCases "prim inverses commute" (PropU.commuteInverses WSub.commute) ExU.commutables
  , testCases "FL prim recommute" (PropU.recommute WSub.commute) ExU.commutablesFL
  , testCases "FL prim patch and inverse commute" (PropU.patchAndInverseCommute WSub.commute) ExU.commutablesFL
  , testCases "FL prim inverses commute" (PropU.commuteInverses WSub.commute) $ ExU.commutablesFL
  , testCases "fails" (PropU.commuteFails WSub.commute) ([] :: [(V1.Prim WSub.:> V1.Prim) wX wY])
  , testCases "read and show work on Prim" PropU.show_read ExU.primPatches
  , testCases "read and show work on RealPatch" PropU.show_read ExU.realPatches
  , testCases "example flattenings work" PropU.consistentTreeFlattenings ExU.realPatchLoopExamples
  , testCases "real merge input consistent" (PropU.mergeArgumentsConsistent isConsistent) ExU.realMergeables
  , testCases "real merge input is forward" (PropU.mergeArgumentsConsistent isForward) ExU.realMergeables
  , testCases "real merge output is forward" (PropU.mergeConsistent isForward) ExU.realMergeables
  , testCases "real merge output consistent" (PropU.mergeConsistent isConsistent) ExU.realMergeables
  , testCases "real merge either way" PropU.mergeEitherWay ExU.realMergeables
  , testCases "real merge and commute" PropU.mergeCommute ExU.realMergeables

  , testCases "real recommute" (PropU.recommute WSub.commute) ExU.realCommutables
  , testCases "real inverses commute" (PropU.commuteInverses WSub.commute) ExU.realCommutables
  , testCases "real permutivity" (PropU.permutivity WSub.commute) ExU.realNonduplicateTriples
  , testCases "real partial permutivity" (PropU.partialPermutivity WSub.commute) ExU.realNonduplicateTriples
  ]

instance PrimPatch prim => Check (RealPatch prim) where
  checkPatch p = return $ isNothing $ isConsistent p

instance Check V3.Prim where
  checkPatch _ = return True -- XXX

commuteReals :: PrimPatch prim => (RealPatch prim :> RealPatch prim) wX wY -> Maybe ((RealPatch prim :> RealPatch prim) wX wY)
commuteReals = commute

qc_prim :: forall prim wX wY wA model.
           (PrimPatch prim, ArbitraryPrim prim, Show2 prim
           , model ~ ModelOf prim, RepoModel model
           , RepoState model ~ ApplyState (PrimOf prim)
           , Show1 (ModelOf prim)
           , Check prim, PrimPatchBase prim, PrimOf prim ~ prim
           , FromPrim prim
           , MightBeEmptyHunk prim
           , MightHaveDuplicate prim
           , Show1 (prim wA)
           , Show1 ((prim :> prim) wA)
           , Show1 (WithState model prim wA)
           , Arbitrary (Sealed ((prim :> prim) wA))
           , Arbitrary (Sealed ((prim :> prim :> prim) wA))
           , Arbitrary (Sealed (prim wA))
           , Arbitrary (Sealed (FL prim wA))
           , Arbitrary (Sealed ((FL prim :> FL prim) wA))
           , Arbitrary (Sealed (WithState model prim wA))
           , Arbitrary (Sealed (WithState model (FL prim) wA))
           , Arbitrary (Sealed2 (WithState model (prim :> prim)))
           , Arbitrary (Sealed ((WithState model (prim :> prim)) wA))
           , Arbitrary (Sealed ((WithState model (FL prim :> FL prim)) wA))
           ) => prim wX wY -> [Test]
qc_prim p =
  -- The following fails because of setpref patches...
  -- testProperty "prim inverse doesn't commute" (inverseDoesntCommute :: Prim -> Maybe Doc)
  (if runCoalesceTests p then
    [ testProperty "prim coalesce effect preserving... "
      (unseal2 $ PropG.coalesceEffectPreserving coalesce :: Sealed2 (WithState model (prim :> prim)) -> TestResult)
    ]
   else [])
    ++ concat
  [ pair_properties            (undefined :: prim wX wY)    "arbitrary"    arbitraryThing'
  , pair_properties            (undefined :: FL prim wX wY) "arbitrary FL" arbitraryThing'
  , coalesce_properties        (undefined :: prim wX wY)    "arbitrary"    arbitraryThing'
  , nonreal_commute_properties (undefined :: prim wX wY)    "arbitrary"    arbitraryThing'
  , nonreal_commute_properties (undefined :: FL prim wX wY) "arbitrary FL" arbitraryThing'
  , patch_properties           (undefined :: prim wX wA)    "arbitrary"    arbitraryThing'
  , patch_properties           (undefined :: FL prim wX wA) "arbitrary FL" arbitraryThing'
  , patch_repo_properties      (undefined :: prim wX wA)    "arbitrary"    arbitraryThing'
  , patch_repo_properties      (undefined :: FL prim wX wA) "arbitrary FL" arbitraryThing'
  , pair_repo_properties       (undefined :: prim wX wA)    "arbitrary"    arbitraryThing'
  , pair_repo_properties       (undefined :: FL prim wX wA) "arbitrary FL" arbitraryThing'
  ]
      where arbitraryThing' = arbitraryThing (undefined :: wA) -- bind the witness for generator

qc_V2P1 :: [Test]
qc_V2P1 =
  [ testProperty "tree flattenings are consistent... "
    PropR.propConsistentTreeFlattenings
  , testProperty "with quickcheck that real patches are consistent... "
    (unseal $ P1.patchFromTree $ fromMaybe . isConsistent)
  -- permutivity ----------------------------------------------------------------------------
  , testConditional "permutivity"
    (unseal $ P1.commuteTripleFromTree notDuplicatestriple)
    (unseal $ P1.commuteTripleFromTree $ PropG.permutivity commuteReals)
  , testConditional "partial permutivity"
    (unseal $ P1.commuteTripleFromTree notDuplicatestriple)
    (unseal $ P1.commuteTripleFromTree $ PropG.partialPermutivity commuteReals)
  , testConditional "nontrivial permutivity"
    (unseal $ P1.commuteTripleFromTree (\t -> nontrivialTriple t && notDuplicatestriple t))
    (unseal $ P1.commuteTripleFromTree $ (PropG.permutivity commuteReals))
  ]

qc_V2 :: forall prim wXx wYy . (PrimPatch prim, Show1 (ModelOf prim), RepoModel (ModelOf prim),
                                  Check (RealPatch prim), ArbitraryPrim prim, Show2 prim,
                                  RepoState (ModelOf prim) ~ ApplyState prim)
      => prim wXx wYy -> [Test]
qc_V2 _ =
  [ testProperty "readPatch and showPatch work on RealPatch... "
    (unseal $ patchFromTree $ (PropG.show_read :: RealPatch prim wX wY -> TestResult))
  , testProperty "readPatch and showPatch work on FL RealPatch... "
    (unseal2 $ (PropG.show_read :: FL (RealPatch prim) wX wY -> TestResult))
  , testProperty "we can do merges using QuickCheck"
    (isNothing . (PropG.propIsMergeable ::
                     Sealed (WithStartState (ModelOf prim) (Tree prim))
                     -> Maybe (Tree (RealPatch prim) wX)))
  ]
  ++ concat
  [ merge_properties   (undefined :: RealPatch prim wX wY) "tree" mergePairFromTree
  , merge_properties   (undefined :: RealPatch prim wX wY) "twfp" mergePairFromTWFP
  , pair_properties    (undefined :: RealPatch prim wX wY) "tree" commutePairFromTree
  , pair_properties    (undefined :: RealPatch prim wX wY) "twfp" commutePairFromTWFP
  , patch_properties   (undefined :: RealPatch prim wX wY) "tree" patchFromTree
  ]

properties :: forall thing gen. (Show1 gen, Arbitrary (Sealed gen)) =>
              TestGenerator thing gen
           -- -> forall xx yy. thing xx yy
           -> String -> String
           -> forall t. Testable t => [(String, TestCondition thing, TestCheck thing t)]
           -> [Test]
properties gen prefix genname tests =
  [ cond name condition check | (name, condition, check) <- tests ]
  where cond :: forall testable. Testable testable
             => String -> TestCondition thing -> TestCheck thing testable -> Test
        cond t c p =
          testConditional (prefix ++ " (" ++ genname ++ "): " ++ t) (unseal $ gen c) (unseal $ gen p)

type PropList what gen = String -> TestGenerator what gen -> [Test]

pair_properties :: forall p gen x y
                 . ( Show1 gen, Arbitrary (Sealed gen), Patchy p, MightHaveDuplicate p
                   , ShowPatchBasic p, MyEq p
                   )
                => p x y -> PropList (p :> p) gen
pair_properties _ genname gen =
  properties gen "commute" genname
  [ ("recommute"              , const True       , PropG.recommute commute             )
  , ("nontrivial recommute"   , nontrivialCommute, PropG.recommute commute             )
  , ("inverses commute"       , const True       , PropG.commuteInverses commute       )
  , ("nontrivial inverses"    , nontrivialCommute, PropG.commuteInverses commute       )
  , ("inverse composition"    , const True       , PropG.inverseComposition            )
  ]

coalesce_properties :: forall p gen x y
                     . ( Show1 gen, Arbitrary (Sealed gen), Patchy p, PrimPatch p
                       , ArbitraryPrim p, MightBeEmptyHunk p
                       )
                    => p x y -> PropList (p :> p :> p) gen
coalesce_properties p genname gen =
  properties gen "commute" genname
   (if runCoalesceTests p then [ ("coalesce commutes with commute", const True, PropG.coalesceCommute coalesce) ] else [])

-- The following properties do not hold for "Real" patches (conflictors and
-- duplicates, specifically) .
nonreal_commute_properties :: forall p gen x y
                            . (Show1 gen, Arbitrary (Sealed gen), Patchy p, ShowPatchBasic p, MyEq p)
                           => p x y -> PropList (p :> p) gen
nonreal_commute_properties _ genname gen =
  properties gen "commute" genname
  [ ("patch & inverse commute", const True       , PropG.patchAndInverseCommute commute)
  , ("patch & inverse commute", nontrivialCommute, PropG.patchAndInverseCommute commute)
  ]

patch_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p, MyEq p)
                 => p x y -> PropList p gen
patch_properties _ genname gen =
  properties gen "patch" genname
  [ ("inverse . inverse is id"  , const True       , PropG.invertSymmetry)
  ]

patch_repo_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen),
                                            Patchy p, ShowPatchBasic p,
                                            RepoModel (ModelOf (PrimOf p)),
                                            RepoState (ModelOf (PrimOf p)) ~ ApplyState p)
                      => p x y -> PropList (WithState (ModelOf (PrimOf p)) p) gen
patch_repo_properties _ genname gen =
  properties gen "patch/repo" genname
  [ ("invert rollback"          , const True       , PropG.invertRollback)
  ]

pair_repo_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen), Patchy p,
                                            MightBeEmptyHunk p,
                                            RepoModel (ModelOf p),
                                            RepoState (ModelOf p) ~ ApplyState p)
                      => p x y -> PropList (WithState (ModelOf p) (p :> p)) gen
pair_repo_properties _ genname gen =
  properties gen "patch/repo" genname
  [ ("commute is effect preserving" , const True       , PropG.effectPreserving commute )
  ]

merge_properties :: forall p gen x y. (Show1 gen, Arbitrary (Sealed gen)
                                      , Patchy p, MyEq p, Merge p, ShowPatchBasic p
                                      , MightHaveDuplicate p, Show2 p, Check p)
                 => p x y -> PropList (p :\/: p) gen
merge_properties _ genname gen =
  properties gen "merge" genname
  [ ("merge either way"           , const True     , PropG.mergeEitherWay      )
  , ("merge either way valid"     , const True     , Prop1.tMergeEitherWayValid)
  , ("nontrivial merge either way", nontrivialMerge, PropG.mergeEitherWay      )
  , ("merge commute"              , const True     , PropG.mergeCommute        )
  ]

qc_V1P1 :: [Test]
qc_V1P1 =
  [
    testProperty "show and read work right" (unseal Prop2.propReadShow)
  ]
  ++ Prop2.checkSubcommutes Prop2.subcommutesInverse "patch and inverse both commute"
  ++ Prop2.checkSubcommutes Prop2.subcommutesNontrivialInverse "nontrivial commutes are correct"
  ++ Prop2.checkSubcommutes Prop2.subcommutesFailure "inverses fail"
  ++
  [ testProperty "commuting by patch and its inverse is ok" Prop2.propCommuteInverse
  -- , testProperty "conflict resolution is valid... " Prop.propResolveConflictsValid
  , testProperty "a patch followed by its inverse is identity"
    Prop2.propPatchAndInverseIsIdentity
  , testProperty "'simple smart merge'" Prop2.propSimpleSmartMergeGoodEnough
  , testProperty "commutes are equivalent" Prop2.propCommuteEquivalency
  , testProperty "merges are valid" Prop2.propMergeValid
  , testProperty "inverses being valid" Prop2.propInverseValid
  , testProperty "other inverse being valid" Prop2.propOtherInverseValid
  -- The patch generator isn't smart enough to generate correct test cases for
  -- the following: (which will be obsoleted soon, anyhow)
  -- , testProperty "the order dependence of unravel... " Prop.propUnravelOrderIndependent
  -- , testProperty "the unravelling of three merges... " Prop.propUnravelThreeMerge
  -- , testProperty "the unravelling of a merge of a sequence... " Prop.propUnravelSeqMerge
  , testProperty "the order of commutes" Prop2.propCommuteEitherOrder
  , testProperty "commute either way" Prop2.propCommuteEitherWay
  , testProperty "the double commute" Prop2.propCommuteTwice
  , testProperty "merges commute and are well behaved"
    Prop2.propMergeIsCommutableAndCorrect
  , testProperty "merges can be swapped" Prop2.propMergeIsSwapable
  , testProperty "again that merges can be swapped (I'm paranoid) " Prop2.propMergeIsSwapable

  ] -- the following properties are disabled, because they routinely lead to
    -- exponential cases, making the tests run for ever and ever; nevertheless,
    -- we would expect them to hold
 {- ++ merge_properties (undefined :: V1.Patch Prim wX wY) "tree" mergePairFromTree
    ++ merge_properties (undefined :: V1.Patch Prim wX wY) "twfp" mergePairFromTWFP
    ++ commute_properties (undefined :: V1.Patch Prim wX wY) "tree" commutePairFromTree
    ++ commute_properties (undefined :: V1.Patch Prim wX wY) "twfp" commutePairFromTWFP -}

-- tests (either QuickCheck or Unit) that should be run on any type of patch
general_patchTests :: (RepoPatch p, ArbitraryPrim (PrimOf p), Show2 (PrimOf p)) => PatchType p -> [Test]
general_patchTests pt =
     [ testGroup "Rebase patches" $ Rebase.testSuite pt
     ]

-- | This is the big list of tests that will be run using testrunner.
testSuite :: [Test]
testSuite = [ testGroup "Darcs.Patch.Prim.V1" $ qc_prim (undefined :: V1.Prim wX wY)
            , testGroup "Darcs.Patch.V1 (using Prim.V1)" $
                unit_V1P1 ++ qc_V1P1 ++ general_patchTests (PatchType :: PatchType (V1.Patch V1.Prim))
            , testGroup "Darcs.Patch.V2 (using Prim.V1)" $
                unit_V2P1 ++ qc_V2 (undefined :: V1.Prim wX wY) ++ qc_V2P1 ++
                general_patchTests (PatchType :: PatchType (RealPatch V1.Prim))
            -- , testGroup "Darcs.Patch.Prim.V3" $ qc_prim (undefined :: V3.Prim wX wY)
            , testGroup "Darcs.Patch.V2 (using Prim.V3)" $
                qc_V2 (undefined :: V3.Prim wX wY) ++
                general_patchTests (PatchType :: PatchType (RealPatch V3.Prim))
            , Darcs.Test.Patch.Info.testSuite
            ]
