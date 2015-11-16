{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Darcs.Test.Patch.WSub where

{-
The Examples.Set2Unwitnessed module builds a lot of test cases by pattern matching
on the results of merge/commute in where clauses. This would
be very painful to switch to using witnesses properly, because
we'd have to make them use case in series.

So instead we give up on witnesses for this module, but instead
of preprocessor hacks which make incompatible code with the rest
of darcs, we build a fresh set of witnesses constructors (FL etc)
which aren't actually GADTs or existentials. So the pattern matching
works as before, but we need to translate back and forth a lot.

We call the normal darcs constructors the 'W' variants.
-}

import qualified Darcs.Test.Patch.Arbitrary.Generic as W
     ( getPairs, getTriples )

import qualified Darcs.Patch as W ( commute )
import qualified Darcs.Patch.Merge as W ( merge, mergeFL )
import qualified Darcs.Patch.Prim as W ( coalesce )

import qualified Darcs.Patch.Witnesses.Ordered as W
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart, unsafeCoercePEnd )

import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.Patchy ( Commute, Invert(..) )


infixr 5 :>:
infixr 5 +>+
infixr 1 :>
infix 1 :/\:
infix 1 :\/:

data FL p wX wY where
   NilFL :: FL p wX wY
   (:>:) :: p wX wY -> FL p wX wY -> FL p wX wY

(+>+) :: FL p wX wY -> FL p wX wY -> FL p wX wY
NilFL +>+ ps = ps
(p :>: ps) +>+ qs = p :>: (ps +>+ qs)

data (p :> q) wX wY where
   (:>) :: p wX wY -> q wX wY -> (p :> q) wX wY

data (p :\/: q) wX wY where
   (:\/:) :: p wX wY -> q wX wY -> (p :\/: q) wX wY

data (p :/\: q) wX wY where
   (:/\:) :: p wX wY -> q wX wY -> (p :/\: q) wX wY

class WSub wp p | p -> wp, wp -> p where
   fromW :: wp wX wY -> p wX wY
   toW :: p wX wY -> wp wX wY

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:\/: wp2) (p1 :\/: p2) where
   fromW (x W.:\/: y) = unsafeCoerceP (fromW x) :\/: unsafeCoerceP (fromW y)
   toW (x :\/: y) = unsafeCoerceP (toW x) W.:\/: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:/\: wp2) (p1 :/\: p2) where
   fromW (x W.:/\: y) = unsafeCoerceP (fromW x) :/\: unsafeCoerceP (fromW y)
   toW (x :/\: y) = unsafeCoerceP (toW x) W.:/\: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:> wp2) (p1 :> p2) where
   fromW (x W.:> y) = unsafeCoercePEnd (fromW x) :> unsafeCoercePStart (fromW y)
   toW (x :> y) = unsafeCoercePEnd (toW x) W.:> unsafeCoercePStart (toW y)

instance WSub wp p => WSub (W.FL wp) (FL p) where
   fromW W.NilFL = unsafeCoerceP NilFL
   fromW (x W.:>: xs) = unsafeCoercePEnd (fromW x) :>: unsafeCoercePStart (fromW xs)

   toW NilFL = unsafeCoerceP W.NilFL
   toW (x :>: xs) = unsafeCoercePEnd (toW x) W.:>: unsafeCoercePStart (toW xs)

instance WSub prim prim => WSub (RealPatch prim) (RealPatch prim) where
   fromW = id
   toW = id

instance WSub Prim Prim where
   fromW = id
   toW = id

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :> q) wX wY) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :> q) where
   showDict2 = ShowDictClass

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :\/: q) wX wY) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :\/: q) where
   showDict2 = ShowDictClass

instance (WSub wp p, Show2 wp) => Show (FL p wX wY) where
   show = show . toW

instance (WSub wp p, Show2 wp) => Show2 (FL p) where
   showDict2 = ShowDictClass

instance (WSub wp p, Commute wp, MyEq wp) => MyEq (FL p) where
   unsafeCompare x y = unsafeCompare (toW x) (toW y)

instance (WSub wp p, Commute wp, Invert wp) => Invert (FL p) where
   invert = fromW . invert . toW

instance (WSub wp p, Commute wp) => Commute (FL p) where
   commute (xs W.:> ys) = do ys' W.:> xs' <- W.commute (toW xs W.:> toW ys)
                             return (fromW ys' W.:> fromW xs')

mergeFL :: (WSub wp p, Merge wp) => (p :\/: FL p) wX wY -> (FL p :/\: p) wX wY
mergeFL = fromW . W.mergeFL . toW

merge :: (WSub wp p, Merge wp) => (p :\/: p) wX wY -> (p :/\: p) wX wY
merge = fromW . W.merge . toW

commute :: (WSub wp p, Commute wp) => (p :> p) wX wY -> Maybe ((p :> p) wX wY)
commute = fmap fromW . W.commute . toW

getPairs :: FL (RealPatch Prim) wX wY -> [Sealed2 (RealPatch Prim :> RealPatch Prim)]
getPairs = map (mapSeal2 fromW) . W.getPairs . toW

getTriples :: FL (RealPatch Prim) wX wY -> [Sealed2 (RealPatch Prim :> RealPatch Prim :> RealPatch Prim)]
getTriples = map (mapSeal2 fromW) . W.getTriples . toW

coalesce :: (Prim :> Prim) wX wY -> Maybe (FL Prim wX wY)
coalesce = fmap fromW . W.coalesce . toW

