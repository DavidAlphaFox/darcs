{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Viewing () where

import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..) )
import Darcs.Patch.Summary ( plainSummary )

import Darcs.Patch.V1.Apply ()
import Darcs.Patch.V1.Core ( Patch(..) )
import Darcs.Patch.V1.Show ( showPatch_ )


instance PrimPatch prim => ShowPatchBasic (Patch prim) where
    showPatch = showPatch_

instance PrimPatch prim => ShowPatch (Patch prim) where
    showContextPatch (PP p) = showContextPatch p
    showContextPatch p = return $ showPatch p
    summary = plainSummary
    summaryFL = plainSummary
    thing _ = "change"

