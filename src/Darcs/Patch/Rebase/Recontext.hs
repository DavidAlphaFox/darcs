--  Copyright (C) 2012 Ganesh Sittampalam
--
--  BSD3
module Darcs.Patch.Rebase.Recontext
    (
      RecontextRebase(..)
    , RecontextRebase1(..)
    , RecontextRebase2(..)
    ) where

import Darcs.Patch.Named ( Named )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup )
import Darcs.Patch.Witnesses.Eq ( EqCheck )
import Darcs.Patch.Witnesses.Ordered ( FL )

-- |Check whether a given patch is a suspended rebase patch, and if so provide
-- evidence that the start and end contexts are the same (from the point of view
-- of the containing repo), and return a function that produces a new version
-- with some fixups added.
--
-- Nested in a type to avoid needing an impredicative argument to 'Maybe'.
newtype RecontextRebase1 p =
    RecontextRebase1 {
        recontextFunc1 :: forall wY wZ . Named p wY wZ -> (EqCheck wY wZ, RecontextRebase2 p wY wZ)
    }

-- |Return a suspended patch with the given fixups added.
--
-- Nested in a type to avoid needing an impredicative argument to a tuple.
newtype RecontextRebase2 p wY wZ =
    RecontextRebase2 {
        recontextFunc2 :: forall wX . FL (RebaseFixup p) wX wY -> IO (Named p wX wX)
    }

-- |Some non-rebase code needs to manipulate the rebase state if one exists.
-- This class provides the hook for them to do so without needing to explicitly
-- detect that there is a rebase state: 'recontextRebase' abstracts out that
-- information.
--
-- The hook is used in amend-record - look there for an explanation of how.
--
-- There is a default so that other patch types only need to declare the instance.
class RecontextRebase p where
    recontextRebase :: Maybe (RecontextRebase1 p)
    recontextRebase = Nothing