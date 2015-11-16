
-- |
-- Module      : Darcs.Patch.Merge
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Merge
    (
      Merge(..)
    , selfMerger
    , mergeFL
    ) where


import Data.Maybe ( fromJust )

import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.CommuteFn ( MergeFn )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..)
    , (:/\:)(..)
    , FL(..)
    , RL
    , reverseFL
    , reverseRL
    )

-- | Things that can always be merged
class Commute p => Merge p where
    merge :: (p :\/: p) wX wY
          -> (p :/\: p) wX wY


selfMerger :: Merge p => MergeFn p p
selfMerger = merge


instance Merge p => Merge (FL p) where
    merge (NilFL :\/: x) = x :/\: NilFL
    merge (x :\/: NilFL) = NilFL :/\: x
    merge ((x:>:xs) :\/: ys) = fromJust $ do
        ys' :/\: x' <- return $ mergeFL (x :\/: ys)
        xs' :/\: ys'' <- return $ merge (ys' :\/: xs)
        return (ys'' :/\: (x' :>: xs'))


instance Merge p => Merge (RL p) where
    merge (x :\/: y) = case merge (reverseRL x :\/: reverseRL y) of
                       (ry' :/\: rx') -> reverseFL ry' :/\: reverseFL rx'


mergeFL :: Merge p
        => (p :\/: FL p) wX wY
        -> (FL p :/\: p) wX wY
mergeFL (p :\/: NilFL) = NilFL :/\: p
mergeFL (p :\/: (x :>: xs)) = fromJust $ do
    x' :/\: p' <- return $ merge (p :\/: x)
    xs' :/\: p'' <- return $ mergeFL (p' :\/: xs)
    return ((x' :>: xs') :/\: p'')

