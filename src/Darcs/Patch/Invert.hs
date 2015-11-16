module Darcs.Patch.Invert
       ( Invert(..), invertFL, invertRL
       )
       where

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..), reverseFL, reverseRL )


class Invert p where
    invert :: p wX wY -> p wY wX

invertFL :: Invert p => FL p wX wY -> RL p wY wX
invertFL NilFL = NilRL
invertFL (x:>:xs) = invert x :<: invertFL xs

invertRL :: Invert p => RL p wX wY -> FL p wY wX
invertRL NilRL = NilFL
invertRL (x:<:xs) = invert x :>: invertRL xs

instance Invert p => Invert (FL p) where
    invert = reverseRL . invertFL

instance Invert p => Invert (RL p) where
    invert = reverseFL . invertRL
