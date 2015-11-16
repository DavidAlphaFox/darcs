--  Copyright (C) 2009-2012 Ganesh Sittampalam
--
--  BSD3
module Darcs.Patch.Rebase.NameHack
    ( NameHack(..)
    ) where

import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Witnesses.Ordered ( FL )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )

-- |When commuting a @Normal@ patch past a @Suspended@ one, we need to adjust the
-- internals of the @Suspended@ one to take account of the effect of the @Normal@ patch.
-- This includes the name of the @Normal@ patch - but the layering is such that we
-- are actually commuting patches of type @Named (Rebasing p)@ - i.e. @Rebasing p@
-- doesn't actually contain the name. We therefore need to add a hook to the @Commute@
-- instances for @Named@ which @Rebasing@ can then implement.
--
-- There is a default so that other patch types only need to declare the instance.
class NameHack p where
    nameHack :: D.DiffAlgorithm -> Maybe (PatchInfo -> FL p wX wY -> FL p wX wY, PatchInfo -> FL p wW wZ -> FL p wW wZ)
    nameHack = \_ -> Nothing

