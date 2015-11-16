module Darcs.Test.Patch.Properties.GenericUnwitnessed where

import qualified Darcs.Test.Patch.Properties.Generic as W
     ( permutivity, partialPermutivity
     , mergeConsistent, mergeArgumentsConsistent, mergeEitherWay
     , mergeCommute, patchAndInverseCommute, coalesceCommute, commuteInverses
     , recommute
     , show_read )
import Darcs.Test.Patch.Arbitrary.Generic ( Tree, MightBeEmptyHunk, MightHaveDuplicate )
import Darcs.Test.Patch.RepoModel( RepoModel, RepoState )
import Darcs.Test.Patch.WithState( WithStartState )

import qualified Darcs.Test.Patch.Properties.Real as W ( propConsistentTreeFlattenings )
import Darcs.Test.Patch.WSub
import Darcs.Test.Util.TestResult

import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Patchy ( showPatch )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Sealed( Sealed )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch ( Patchy )
import Darcs.Util.Printer ( Doc, redText, ($$) )
import qualified Storage.Hashed.Tree as HST ( Tree )


permutivity :: (Patchy wp, ShowPatchBasic wp, MyEq wp, WSub wp p)
            => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
            -> (p :> p :> p) wA wB -> TestResult
permutivity f = W.permutivity (fmap toW . f . fromW) . toW

partialPermutivity :: (Patchy wp, ShowPatchBasic wp, MyEq wp, WSub wp p)
                    => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                    -> (p :> p :> p) wA wB -> TestResult
partialPermutivity f = W.partialPermutivity (fmap toW . f . fromW) . toW

mergeEitherWay :: (Patchy wp, ShowPatchBasic wp, MyEq wp, Merge wp, WSub wp p) => (p :\/: p) wX wY -> TestResult
mergeEitherWay = W.mergeEitherWay . toW

commuteInverses :: (Patchy wp, ShowPatchBasic wp, MyEq wp, WSub wp p)
                 => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                 -> (p :> p) wA wB -> TestResult
commuteInverses f = W.commuteInverses (fmap toW . f . fromW) . toW

recommute :: (Patchy wp, ShowPatchBasic wp, MightHaveDuplicate wp, MyEq wp, WSub wp p)
          => (forall wX wY . ((p :> p) wX wY -> Maybe ((p :> p) wX wY)))
          -> (p :> p) wA wB -> TestResult
recommute f = W.recommute (fmap toW . f . fromW) . toW

mergeCommute :: (Patchy wp, MightHaveDuplicate wp, ShowPatchBasic wp, MyEq wp, Merge wp, WSub wp p)
             => (p :\/: p) wX wY -> TestResult
mergeCommute = W.mergeCommute . toW

mergeConsistent :: (Patchy wp, Merge wp, ShowPatchBasic wp, WSub wp p) =>
                           (forall wX wY . p wX wY -> Maybe Doc)
                        -> (p :\/: p) wA wB -> TestResult
mergeConsistent f = W.mergeConsistent (f . fromW) . toW

mergeArgumentsConsistent :: (Patchy wp, ShowPatchBasic wp, WSub wp p) =>
                              (forall wX wY . p wX wY -> Maybe Doc)
                           -> (p :\/: p) wA wB -> TestResult
mergeArgumentsConsistent f = W.mergeArgumentsConsistent (f . fromW) . toW

show_read :: (Patchy p, ShowPatchBasic p, ReadPatch p, MyEq p, Show2 p) => p wX wY -> TestResult
show_read = W.show_read

patchAndInverseCommute :: (Patchy wp, ShowPatchBasic wp, MyEq wp, WSub wp p) =>
                             (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                          -> (p :> p) wA wB -> TestResult
patchAndInverseCommute f = W.patchAndInverseCommute (fmap toW . f . fromW) . toW


coalesceCommute :: MightBeEmptyHunk Prim
                => (forall wX wY . (Prim :> Prim) wX wY -> Maybe (FL Prim wX wY))
                -> (Prim :> Prim :> Prim) wA wB -> TestResult
coalesceCommute f = W.coalesceCommute (fmap toW . f . fromW) . toW

consistentTreeFlattenings :: (RepoState model ~ HST.Tree, RepoModel model)
                          => Sealed (WithStartState model (Tree Prim)) -> TestResult
consistentTreeFlattenings = (\x -> if W.propConsistentTreeFlattenings x
                                      then succeeded
                                      else failed $ redText "oops")

commuteFails :: (MyEq p, Patchy p, ShowPatchBasic p)
             => ((p :> p) wX wY -> Maybe ((p :> p) wX wY))
             -> (p :> p) wX wY
             -> TestResult
commuteFails c (x :> y) = case c (x :> y) of
                            Nothing -> succeeded
                            Just (y' :> x') ->
                              failed $ redText "x" $$ showPatch x $$
                                       redText ":> y" $$ showPatch y $$
                                       redText "y'" $$ showPatch y' $$
                                       redText ":> x'" $$ showPatch x'
