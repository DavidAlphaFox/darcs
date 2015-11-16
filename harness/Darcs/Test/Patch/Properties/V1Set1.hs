{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Test.Patch.Properties.V1Set1
       ( checkMerge, checkMergeEquiv, checkMergeSwap, checkCanon
       , checkCommute, checkCantCommute
       , tShowRead
       , tMergeEitherWayValid, tTestCheck ) where

import Darcs.Patch
     ( Patchy, commute, invert, merge, effect
     , readPatch, showPatch
     , fromPrim, canonize, sortCoalesceFL )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import qualified Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Test.Patch.Properties.Check ( checkAPatch, Check )
import Darcs.Util.Printer ( renderPS, RenderMode(..) )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Patch.Witnesses.Unsafe( unsafeCoercePEnd )
import Darcs.Test.Util.TestResult
import qualified Darcs.Util.Diff as D (DiffAlgorithm(..))
import Darcs.Util.Printer ( text )


type Patch = V1.Patch Prim


quickmerge :: (Patchy p, Merge p) => (p :\/: p ) wX wY -> p wY wZ
quickmerge (p1:\/:p2) = case merge (p1:\/:p2) of
                        _ :/\: p1' -> unsafeCoercePEnd p1'

instance Show2 p => Show ((p :< p) wX wY) where
   show (x :< y) = show2 x ++ " :< " ++ show2 y
instance MyEq p => Eq ((p :/\: p) wX wY) where
   (x :/\: y) == (x' :/\: y') = isIsEq (x =\/= x') && isIsEq (y =\/= y')

-- ----------------------------------------------------------------------------
-- A number of "comparison" properties: these carry out some operation on
-- inputs (first value in the pair) and compare the results with a known
-- expected value (the second value in the pair).
--

checkMerge :: ((FL Patch:\/: FL Patch) wX wY, FL Patch wY wZ) -> TestResult
checkMerge (p1:\/:p2,p1') =
   case merge (p1:\/:p2) of
   _ :/\: p1a ->
       if isIsEq (p1a `eqFL` p1')
       then succeeded
       else failed $ text $ "Merge gave wrong value!\n"++show p1++show p2
            ++"I expected\n"++show p1'
            ++"but found instead\n"++show p1a

checkMergeEquiv :: ((FL Patch:\/:FL Patch) wX wY,FL Patch wY wZ) -> TestResult
checkMergeEquiv (p1:\/: p2, pe) =
    case quickmerge (p1:\/:p2) of
    p1' -> if checkAPatch (invert p1 :>: p2 :>: p1' :>: invert pe :>: NilFL)
           then succeeded
           else failed $ text $ "Oh no, merger isn't equivalent...\n"++show p1++"\n"++show p2
                 ++"in other words\n" ++ show (p1 :\/: p2)
                 ++"merges as\n" ++ show (merge $ p1 :\/: p2)
                 ++"merges to\n" ++ show (quickmerge $ p1 :\/: p2)
                 ++"which is equivalent to\n" ++ show (effect p1')
                 ++ "should all work out to\n"
                 ++ show pe

checkMergeSwap :: (FL Patch wX wY, FL Patch wX wZ) -> TestResult
checkMergeSwap (p1, p2) =
    case merge (p2:\/:p1) of
    _ :/\: p2' ->
        case merge (p1:\/:p2) of
        _ :/\: p1' ->
            case commute (p1:>p2') of
            Just (_:>p1'b) ->
                if not $ p1'b `eqFLUnsafe` p1'
                then failed $ text $ "Merge swapping problem with...\np1 "++
                      show p1++"merged with\np2 "++
                      show p2++"p1' is\np1' "++
                      show p1'++"p1'b is\np1'b  "++
                      show p1'b
                else succeeded
            Nothing -> failed $ text $ "Merge commuting problem with...\np1 "++
                        show p1++"merged with\np2 "++
                        show p2++"gives\np2' "++
                        show p2'++"which doesn't commute with p1.\n"

checkCanon :: forall wX wY . (FL Patch wX wY, FL Patch wX wY) -> TestResult
checkCanon (p1,p2) =
    if isIsEq $ eqFL p1_ p2
    then if isIsEq $ eqFL p1_p p2
         then succeeded
         else failed $ text $ "Canonization with Patience Diff failed:\n"++show p1++"canonized is\n"
               ++show (p1_p :: FL Patch wX wY)
               ++"which is not\n"++show p2
    else failed $ text $ "Canonization with Myers Diff failed:\n"++show p1++"canonized is\n"
          ++show (p1_ :: FL Patch wX wY)
          ++"which is not\n"++show p2
    where p1_ = mapFL_FL fromPrim $ concatFL $ mapFL_FL (canonize D.MyersDiff) $ sortCoalesceFL $ effect p1
          p1_p = mapFL_FL fromPrim $ concatFL $ mapFL_FL (canonize D.PatienceDiff) $ sortCoalesceFL $ effect p1

checkCommute :: ((FL Patch:< FL Patch) wX wY, (FL Patch:< FL Patch) wX wY) -> TestResult
checkCommute (p1:<p2,p2':<p1') =
   case commute (p2:>p1) of
   Just (p1a:>p2a) ->
       if (p2a:< p1a) == (p2':< p1')
       then succeeded
       else failed $ text $ "Commute gave wrong value!\n"++show p1++"\n"++show p2
             ++"should be\n"++show p2'++"\n"++show p1'
             ++"but is\n"++show p2a++"\n"++show p1a
   Nothing -> failed $ text $ "Commute failed!\n"++show p1++"\n"++show p2
   <&&>
   case commute (p1':>p2') of
   Just (p2a:>p1a) ->
       if (p1a:< p2a) == (p1:< p2)
       then succeeded
       else failed $ text $ "Commute gave wrong value!\n"++show p2a++"\n"++show p1a
             ++"should have been\n"++show p2'++"\n"++show p1'
   Nothing -> failed $ text $ "Commute failed!\n"++show p2'++"\n"++show p1'

checkCantCommute :: (FL Patch:< FL Patch) wX wY -> TestResult
checkCantCommute (p1:<p2) =
    case commute (p2:>p1) of
    Nothing -> succeeded
    _ -> failed $ text $ show p1 ++ "\n\n" ++ show p2 ++
          "\nArgh, these guys shouldn't commute!\n"

-- ----------------------------------------------------------------------------
-- A few "test" properties, doing things with input patches and giving a OK/not
-- OK type of answer.

tShowRead :: (Show2 p, Patchy p, ReadPatch p, ShowPatchBasic p)
          => (forall wX wY wW wZ . p wX wY -> p wW wZ -> Bool) -> forall wX wY . p wX wY -> TestResult
tShowRead eq p =
    case readPatch $ renderPS Standard $ showPatch p of
    Just (Sealed p') -> if p' `eq` p then succeeded
                        else failed $ text $ "Failed to read shown:  "++(show2 p)++"\n"
    Nothing -> failed $ text $ "Failed to read at all:  "++(show2 p)++"\n"

tMergeEitherWayValid :: forall wX wY p . (Check p, Show2 p, Merge p, Patchy p) => (p :\/: p) wX wY -> TestResult
tMergeEitherWayValid (p1 :\/: p2) =
  case p2 :>: quickmerge (p1:\/: p2) :>: NilFL of
  combo2 ->
    case p1 :>: quickmerge (p2:\/: p1) :>: NilFL of
    combo1 ->
      if not $ checkAPatch combo1
      then failed $ text $ "oh my combo1 invalid:\n"++show2 p1++"and...\n"++show2 p2++show combo1
      else
        if checkAPatch (invert combo1 :>: combo2 :>: NilFL)
        then succeeded
        else failed $ text $ "merge both ways invalid:\n"++show2 p1++"and...\n"++show2 p2++
              show combo1++
              show combo2

tTestCheck :: forall wX wY . FL Patch wX wY -> TestResult
tTestCheck p = if checkAPatch p
                 then succeeded
                 else failed $ text $ "Failed the check:  "++show p++"\n"
