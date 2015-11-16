{-# LANGUAGE EmptyDataDecls #-}
module Darcs.Test.Patch.Rebase ( testSuite ) where

import Control.Monad ( unless )

import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertFailure )

import Darcs.Patch
import Darcs.Patch.Conflict
import Darcs.Patch.Rebase
import Darcs.Patch.Rebase.Viewing
import Darcs.Patch.Type
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Show

import Darcs.Test.Patch.Arbitrary.Generic

testSuite :: forall p . (RepoPatch p, ArbitraryPrim (PrimOf p), Show2 (PrimOf p)) => PatchType p -> [Test]
testSuite pt =
    if hasPrimConstruct (undefined :: PrimOf p WX WX)
        then
           [ duplicateConflictedEffect pt
           ]
        else
           [
           ]

data WX

duplicateConflictedEffect :: forall p . (RepoPatch p, Show2 (PrimOf p)) => PatchType p -> Test
duplicateConflictedEffect _ =
    testCase "duplicate in rebase fixup has a conflicted effect" $
        unless (all (/= Okay) cStatuses) $
            assertFailure ("unexpected conflicted effect: " ++ show cEffect)
    where
        corePrim = addfile "./file"
        rebase :: RebaseChange p WX WX
        rebase = RCFwd (PrimFixup (invert corePrim) :>: NilFL) (fromPrim corePrim :>: NilFL)
        cEffect = conflictedEffect rebase
        cStatuses = map (\(IsC status _) -> status) cEffect
