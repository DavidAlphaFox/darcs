module Darcs.Patch.Effect ( Effect(..) ) where

import Darcs.Patch.Prim.Class ( PrimOf )

import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseFL, reverseRL
    , concatFL, concatRL, mapFL_FL, mapRL_RL
    )


-- | Patches whose concrete effect which can be expressed as a list of
--   primitive patches.
--
--   A minimal definition would be either of @effect@ or @effectRL@.
class Effect p where
    effect :: p wX wY -> FL (PrimOf p) wX wY
    effect = reverseRL . effectRL
    effectRL :: p wX wY -> RL (PrimOf p) wX wY
    effectRL = reverseFL . effect

instance Effect p => Effect (FL p) where
    effect p = concatFL $ mapFL_FL effect p
    effectRL p = concatRL $ mapRL_RL effectRL $ reverseFL p

instance Effect p => Effect (RL p) where
    effect p = concatFL $ mapFL_FL effect $ reverseRL p
    effectRL p = concatRL $ mapRL_RL effectRL p

