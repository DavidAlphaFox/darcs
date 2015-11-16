
module Darcs.Test.Util.TestResult
  ( TestResult
  , succeeded
  , failed
  , rejected
  , (<&&>)
  , fromMaybe
  , isOk
  , isFailed
  )
  where


import Darcs.Util.Printer ( Doc, renderString, RenderMode(..) )

import qualified Test.QuickCheck.Property as Q



data TestResult = TestSucceeded
                | TestFailed Doc
                | TestRejected      -- ^ Rejects test case


succeeded :: TestResult
succeeded = TestSucceeded

failed :: Doc             -- ^ Error message
          -> TestResult
failed = TestFailed

rejected :: TestResult
rejected = TestRejected

-- | @t <&&> s@ fails <=> t or s fails
--   @t <&&> s@ succeeds <=> none fails and some succeeds
--   @t <&&> s@ is rejected <=> both are rejected
(<&&>) :: TestResult -> TestResult -> TestResult
t@(TestFailed _) <&&> _s               = t
_t               <&&> s@(TestFailed _) = s
TestRejected     <&&> s                = s
t                <&&> TestRejected     = t
TestSucceeded    <&&> TestSucceeded    = TestSucceeded

-- | 'Nothing' is considered success whilst 'Just' is considered failure.
fromMaybe :: Maybe Doc -> TestResult
fromMaybe Nothing       = succeeded
fromMaybe (Just errMsg) = failed errMsg

isFailed :: TestResult -> Bool
isFailed (TestFailed _) = True
isFailed _other         = False

-- | A test is considered OK if it does not fail.
isOk :: TestResult -> Bool
isOk = not . isFailed

  -- 'Testable' instance is defined by converting 'TestResult' to 'QuickCheck.Property.Result'
instance Q.Testable TestResult where
  property TestSucceeded         = Q.property Q.succeeded
  property (TestFailed errorMsg) = Q.property (Q.failed{Q.reason = renderString Encode errorMsg})
  property TestRejected          = Q.property Q.rejected
