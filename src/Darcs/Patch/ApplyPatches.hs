module Darcs.Patch.ApplyPatches
    ( applyPatches
    ) where

import Darcs.Patch.Info ( showPatchInfoUI )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.MonadProgress ( MonadProgress, ProgressAction(..), runProgressActions)

import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Util.Printer ( text, ($$) )

applyPatches :: (MonadProgress m, ApplyMonad m (ApplyState p), Patchy p)
             => FL (PatchInfoAnd p) wX wY -> m ()
applyPatches ps = runProgressActions "Applying patch" (mapFL doApply ps)
  where
    doApply hp = ProgressAction { paAction = apply (hopefully hp)
                                , paMessage = showPatchInfoUI (info hp)
                                , paOnError = text "Unapplicable patch:" $$
                                              showPatchInfoUI (info hp)
                                }
