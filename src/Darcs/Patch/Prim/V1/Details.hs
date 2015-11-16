{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Details
    ()
    where

import Prelude hiding ( pi )
import Darcs.Patch.Prim.Class ( PrimDetails(..) )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )


instance PrimDetails Prim where
  summarizePrim (FP f (Hunk _ o n)) = [SummFile SummMod f (length o) (length n) 0]
  summarizePrim (FP f (Binary _ _)) = [SummFile SummMod f 0 0 0]
  summarizePrim (FP f AddFile) = [SummFile SummAdd f 0 0 0]
  summarizePrim (FP f RmFile) = [SummFile SummRm f 0 0 0]
  summarizePrim (FP f (TokReplace{})) = [SummFile SummMod f 0 0 1]
  summarizePrim (DP d AddDir) = [SummAddDir d]
  summarizePrim (DP d RmDir) = [SummRmDir d]
  summarizePrim (Move f1 f2) = [SummMv f1 f2]
  summarizePrim (ChangePref{}) = [SummNone]
