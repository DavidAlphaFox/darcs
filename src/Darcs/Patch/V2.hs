{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V2 ( RealPatch, prim2real ) where

import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.RepoPatch ( RepoPatch )

import Darcs.Patch.V2.Real ( RealPatch, prim2real )

instance PrimPatch prim => Matchable (RealPatch prim)
instance PrimPatch prim => RepoPatch (RealPatch prim)
