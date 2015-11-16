--  Copyright (C) 2009-2012 Ganesh Sittampalam
--
--  BSD3
module Darcs.Patch.MaybeInternal
    (
      InternalChecker(..)
    , MaybeInternal(..)
    , flIsInternal
    ) where

import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( FL )

-- Note: the EqCheck result could be replaced by a Bool if clients were changed to commute the patch
-- out if necessary.
newtype InternalChecker p = InternalChecker { isInternal :: forall wX wY . p wX wY -> EqCheck wX wY }

-- |Provides a hook for flagging whether a patch is "internal" to the repo
-- and therefore shouldn't be referred to externally, e.g. by inclusion in tags.
-- Note that despite the name, every patch type has to implement it, but for
-- normal (non-internal) types the default implementation is fine.
-- Currently only used for rebase internal patches.
class MaybeInternal p where
    -- | @maybe (const NotEq) (fmap isInternal patchInternalChecker) p@
    -- returns 'IsEq' if @p@ is internal, and 'NotEq' otherwise.
    -- The two-level structure is purely for efficiency: 'Nothing' and 'Just (InternalChecker (const NotEq))' are
    -- semantically identical, but 'Nothing' allows clients to avoid traversing an entire list.
    -- The patch type is passed as an 'FL' because that's how the internals of named patches are stored.
    patchInternalChecker :: Maybe (InternalChecker (FL p))
    patchInternalChecker = Nothing

flIsInternal :: MaybeInternal p => FL p wX wY -> EqCheck wX wY
flIsInternal = maybe (const NotEq) isInternal patchInternalChecker
