{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V3 ( Prim ) where

import Darcs.Patch.Prim.V3.Apply ()
import Darcs.Patch.Prim.V3.Coalesce ()
import Darcs.Patch.Prim.V3.Commute ()
import Darcs.Patch.Prim.V3.Core ( Prim )
import Darcs.Patch.Prim.V3.Details ()
import Darcs.Patch.Prim.V3.Read ()
import Darcs.Patch.Prim.V3.Show ()

import Darcs.Patch.Prim.Class ( PrimPatch, PrimPatchBase(..), FromPrim(..) )
import Darcs.Patch.Patchy ( Patchy )

instance PrimPatch Prim
instance Patchy Prim

instance PrimPatchBase Prim where
  type PrimOf Prim = Prim

instance FromPrim Prim where
  fromPrim = id
