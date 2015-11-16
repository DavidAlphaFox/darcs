
module Darcs.Test.Util.QuickCheck
  ( upper
  , lower
  , alpha
  , notIn
  , uniques
  , maybeOf
  , bSized
  )
  where


import Control.Applicative
import Test.QuickCheck.Gen



-- | An uppercase alphabetic character.
upper :: Gen Char
upper = choose ('A','Z')

-- | A lowercase alphabetic character.
lower :: Gen Char
lower = choose ('a','z')

-- | An alphabetic character.
alpha :: Gen Char
alpha = oneof [upper, lower]

-- | @gen `notIn` xs@ generate a @x@ that is not in @xs@.
notIn :: Eq a => Gen a -> [a] -> Gen a
gen `notIn` xs = gen `suchThat` (`notElem` xs)

-- | @uniques k gen@ generates a list of @k@ unique values.
uniques :: Eq a => Int -> Gen a -> Gen [a]
uniques k gen = go k []
  where
    go 0 xs = return xs
    go n xs = do x <- gen `notIn` xs
                 go (n-1) (x:xs)

-- | Try to arbitrarily pick some element of the list.
maybeOf :: [a] -> Gen (Maybe a)
maybeOf [] = return Nothing
maybeOf xs = Just <$> elements xs


-- | A bounded sized combinator.
bSized :: Int       -- ^ Lower bound
        -> Double   -- ^ Increment
        -> Int      -- ^ Upper bound
        -> (Int -> Gen a) -> Gen a
bSized low inc upp mkGen = sized $ mkGen . resize'
  where
    resize' :: Int -> Int
    resize' n = let x = fromIntegral n
                 in min upp (floor(inc*x) + low)
