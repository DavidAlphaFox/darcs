module Darcs.Patch.Commute
    ( Commute(..)
    , commuteFL
    , commuteFLorComplain
    , commuteRL
    , commuteRLFL
    , toFwdCommute
    , toRevCommute
    , selfCommuter
    ) where

import Darcs.Patch.CommuteFn ( CommuteFn )

import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseFL, reverseRL,
    (:>)(..), (:<)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed2, seal2 )

-- | Commute represents things that can be (possibly) commuted.
class Commute p where
    commute :: (p :> p) wX wY -> Maybe ((p :> p) wX wY)

instance Commute p => Commute (FL p) where
    commute (NilFL :> x) = Just (x :> NilFL)
    commute (x :> NilFL) = Just (NilFL :> x)
    commute (xs :> ys) = do
        ys' :> rxs' <- commuteRLFL (reverseFL xs :> ys)
        return $ ys' :> reverseRL rxs'

-- |'commuteRLFL' commutes an 'RL' past an 'FL'.
commuteRLFL :: Commute p => (RL p :> FL p) wX wY
            -> Maybe ((FL p :> RL p) wX wY)
commuteRLFL (NilRL :> ys) = Just (ys :> NilRL)
commuteRLFL (xs :> NilFL) = Just (NilFL :> xs)
commuteRLFL (xs :> y :>: ys) = do
    y' :> xs' <- commuteRL (xs :> y)
    ys' :> xs'' <- commuteRLFL (xs' :> ys)
    return (y' :>: ys' :> xs'')

instance Commute p => Commute (RL p) where
    commute (xs :> ys) = do
        fys' :> xs' <- commuteRLFL (xs :> reverseRL ys)
        return (reverseFL fys' :> xs')

-- |'commuteRL' commutes a RL past a single element.
commuteRL :: Commute p => (RL p :> p) wX wY -> Maybe ((p :> RL p) wX wY)
commuteRL (z :<: zs :> w) = do
    w' :> z' <- commute (z :> w)
    w'' :> zs' <- commuteRL (zs :> w')
    return (w'' :> z' :<: zs')
commuteRL (NilRL :> w) = Just (w :> NilRL)

-- |'commuteFL' commutes a single element past a FL.
commuteFL :: Commute p => (p :> FL p) wX wY -> Maybe ((FL p :> p) wX wY)
commuteFL = either (const Nothing) Just . commuteFLorComplain

-- |'commuteFLorComplain' attempts to commute a single element past a FL. If
-- any individual commute fails, then we return the patch that first patch that
-- cannot be commuted past.
commuteFLorComplain :: Commute p => (p :> FL p) wX wY
                    -> Either (Sealed2 p) ((FL p :> p) wX wY)
commuteFLorComplain (p :> NilFL) = Right (NilFL :> p)
commuteFLorComplain (q :> p :>: ps) =
    case commute (q :> p) of
        Just (p' :> q') ->
            case commuteFLorComplain (q' :> ps) of
                Right (ps' :> q'') -> Right (p' :>: ps' :> q'')
                Left l -> Left l
        Nothing -> Left $ seal2 p

-- | Swaps the ordered pair type so that commute can be called directly.
toFwdCommute :: (Commute p, Commute q, Monad m) =>
             ((p :< q) wX wY -> m ((q :< p) wX wY)) -> (q :> p) wX wY
             -> m ((p :> q) wX wY)
toFwdCommute c (x :> y) = do
    x' :< y' <- c (y :< x)
    return (y' :> x')

-- | Swaps the ordered pair type from the order expected by commute to the
-- reverse order.
toRevCommute :: (Commute p, Commute q, Monad m) =>
             ((p :> q) wX wY -> m ((q :> p) wX wY)) -> (q :< p) wX wY
             -> m ((p :< q) wX wY)
toRevCommute c (x :< y) = do
    x' :> y' <- c (y :> x)
    return (y' :< x')

-- |Build a commuter between a patch and itself using the operation from the type class.
selfCommuter :: Commute p => CommuteFn p p
selfCommuter = commute
