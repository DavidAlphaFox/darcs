module Darcs.Patch.CommuteFn
    ( CommuteFn,
      commuterIdFL, commuterFLId,
      commuterIdRL, commuterRLId,
      MergeFn,
      mergerIdFL,
      TotalCommuteFn,
      totalCommuterIdFL, totalCommuterFLId, totalCommuterFLFL
    ) where

import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , FL(..)
    , RL(..)
    )

-- |CommuteFn is the basis of a general framework for building up commutation
-- operations between different patch types in a generic manner. Unfortunately
-- type classes are not well suited to the problem because of the multiple possible
-- routes by which the commuter for (FL p1, FL p2) can be built out of the
-- commuter for (p1, p2) - and more complicated problems when we start building
-- multiple constructors on top of each other. The type class resolution machinery
-- really can't cope with selecting some route, because it doesn't know that all
-- possible routes should be equivalent.
type CommuteFn p1 p2 = forall wX wY . (p1 :> p2) wX wY -> Maybe ((p2 :> p1) wX wY)

type TotalCommuteFn p1 p2 = forall wX wY . (p1 :> p2) wX wY -> (p2 :> p1) wX wY

type MergeFn p1 p2 = forall wX wY . (p1 :\/: p2) wX wY -> (p2 :/\: p1) wX wY

commuterIdRL :: CommuteFn p1 p2 -> CommuteFn p1 (RL p2)
commuterIdRL _ (x :> NilRL) = return (NilRL :> x)
commuterIdRL commuter (x :> (y :<: ys))
  = do ys' :> x' <- commuterIdRL commuter (x :> ys)
       y' :> x'' <- commuter (x' :> y)
       return ((y' :<: ys') :> x'')

commuterIdFL :: CommuteFn p1 p2 -> CommuteFn p1 (FL p2)
commuterIdFL _ (x :> NilFL) = return (NilFL :> x)
commuterIdFL commuter (x :> (y :>: ys))
  = do y' :> x' <- commuter (x :> y)
       ys' :> x'' <- commuterIdFL commuter (x' :> ys)
       return ((y' :>: ys') :> x'')

mergerIdFL :: MergeFn p1 p2 -> MergeFn p1 (FL p2)
mergerIdFL _ (x :\/: NilFL) = NilFL :/\: x
mergerIdFL merger (x :\/: (y :>: ys))
  = case merger (x :\/: y) of
      y' :/\: x' -> case mergerIdFL merger (x' :\/: ys) of
          ys' :/\: x'' -> (y' :>: ys') :/\: x''

totalCommuterIdFL :: TotalCommuteFn p1 p2 -> TotalCommuteFn p1 (FL p2)
totalCommuterIdFL _ (x :> NilFL) = NilFL :> x
totalCommuterIdFL commuter (x :> (y :>: ys)) =
   case commuter (x :> y) of
     y' :> x' -> case totalCommuterIdFL commuter (x' :> ys) of
                   ys' :> x'' -> (y' :>: ys') :> x''

commuterFLId :: CommuteFn p1 p2 -> CommuteFn (FL p1) p2
commuterFLId _ (NilFL :> y) = return (y :> NilFL)
commuterFLId commuter ((x :>: xs) :> y)
  = do y' :> xs' <- commuterFLId commuter (xs :> y)
       y'' :> x' <- commuter (x :> y')
       return (y'' :> (x' :>: xs'))

commuterRLId :: CommuteFn p1 p2 -> CommuteFn (RL p1) p2
commuterRLId _ (NilRL :> y) = return (y :> NilRL)
commuterRLId commuter ((x :<: xs) :> y)
  = do y' :> x' <- commuter (x :> y)
       y'' :> xs' <- commuterRLId commuter (xs :> y')
       return (y'' :> (x' :<: xs'))

totalCommuterFLId :: TotalCommuteFn p1 p2 -> TotalCommuteFn (FL p1) p2
totalCommuterFLId _ (NilFL :> y) = y :> NilFL
totalCommuterFLId commuter ((x :>: xs) :> y) =
   case totalCommuterFLId commuter (xs :> y) of
     y' :> xs' -> case commuter (x :> y') of
                    y'' :> x' -> y'' :> (x' :>: xs')

totalCommuterFLFL :: TotalCommuteFn p1 p2 -> TotalCommuteFn (FL p1) (FL p2)
totalCommuterFLFL commuter = totalCommuterFLId (totalCommuterIdFL commuter)