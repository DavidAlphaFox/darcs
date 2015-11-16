module Darcs.Test.Patch.Utils
    ( testConditional, testStringList )
    where

import Test.Framework ( Test, TestName )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit ( assertFailure )
import Test.QuickCheck ( Arbitrary, Testable, (==>) )

-- | Turns a condition and a test function into a conditional quickcheck
--   property that can be run by test-framework.
testConditional
  :: (Arbitrary a, Show a, Testable prop) => TestName     -- ^ Test name
                                          -> (a -> Bool)  -- ^ Condition
                                          -> (a -> prop)  -- ^ Test function
                                          -> Test
testConditional name cond t = testProperty name t'
    where t' x = cond x ==> t x

-- | Utility function to run old tests that return a list of error messages,
--   with the empty list meaning success.
testStringList :: String -> [String] -> Test
testStringList name test = testCase name $ mapM_ assertFailure test
