{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Apply () where

import Darcs.Patch.Apply ( ApplyState, Apply, apply )
import Darcs.Patch.Prim ( PrimPatch, applyPrimFL )
import Darcs.Patch.Repair ( RepairToFL, applyAndTryToFixFL,
                            mapMaybeSnd )
import Darcs.Patch.Effect ( effect )

import Darcs.Patch.V1.Commute ()
import Darcs.Patch.V1.Core ( Patch(..) )

import Darcs.Patch.Witnesses.Ordered ( mapFL_FL )


instance PrimPatch prim => Apply (Patch prim) where
    type ApplyState (Patch prim) = ApplyState prim
    apply p = applyPrimFL $ effect p

instance PrimPatch prim => RepairToFL (Patch prim) where
    applyAndTryToFixFL (PP x) = mapMaybeSnd (mapFL_FL PP) `fmap` applyAndTryToFixFL x
    applyAndTryToFixFL x = do apply x; return Nothing
