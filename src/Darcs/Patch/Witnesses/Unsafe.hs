{-# LANGUAGE MagicHash #-}
module Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP
    , unsafeCoercePStart
    , unsafeCoercePEnd
    , unsafeCoerceP2
    , unsafeCoerceP1
    ) where

import GHC.Base (unsafeCoerce#)

unsafeCoerceP :: a wX wY -> a wB wC
unsafeCoerceP = unsafeCoerce#

unsafeCoercePStart :: a wX1 wY -> a wX2 wY
unsafeCoercePStart = unsafeCoerce#

unsafeCoercePEnd :: a wX wY1 -> a wX wY2
unsafeCoercePEnd = unsafeCoerce#

unsafeCoerceP2 :: t wW wX wY wZ -> t wA wB wC wD
unsafeCoerceP2 = unsafeCoerce#

unsafeCoerceP1 :: a wX -> a wY
unsafeCoerceP1 = unsafeCoerce#
