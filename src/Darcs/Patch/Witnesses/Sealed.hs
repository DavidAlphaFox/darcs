-- Copyright (C) 2007 David Roundy, 2009 Ganesh Sittampalam
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Darcs.Patch.Witnesses.Sealed
    ( Sealed(..)
    , seal
    , unseal
    , mapSeal
    , unsafeUnseal
    , unsafeUnsealFlipped
    , unsafeUnseal2
    , Sealed2(..)
    , seal2
    , unseal2
    , mapSeal2
    , FlippedSeal(..)
    , flipSeal
    , unsealFlipped
    , mapFlipped
    , unsealM
    , liftSM
    , Gap(..)
    , FreeLeft
    , unFreeLeft
    , FreeRight
    , unFreeRight
    ) where

import Darcs.Patch.Witnesses.Eq ( MyEq, EqCheck(..) )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Eq ( (=\/=) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP1, unsafeCoerceP )

-- |A 'Sealed' type is a way of hide an existentially quantified type parameter,
-- in this case wX, inside the type. Note that the only thing we can currently
-- recover about the existentially quantified type wX is that it exists.
data Sealed a where
    Sealed :: a wX -> Sealed a

seal :: a wX -> Sealed a
seal = Sealed

instance MyEq a => Eq (Sealed (a wX)) where
    Sealed x == Sealed y | IsEq <- x =\/= y = True
                         | otherwise = False

-- |The same as 'Sealed' but for two parameters (wX and wY).
data Sealed2 a where
    Sealed2 :: !(a wX wY) -> Sealed2 a

seal2 :: a wX wY -> Sealed2 a
seal2 = Sealed2

data FlippedSeal a wY where
    FlippedSeal :: !(a wX wY) -> FlippedSeal a wY

flipSeal :: a wX wY -> FlippedSeal a wY
flipSeal = FlippedSeal

unsafeUnseal :: Sealed a -> a wX
unsafeUnseal (Sealed a) = unsafeCoerceP1 a

unsafeUnsealFlipped :: FlippedSeal a wY -> a wX wY
unsafeUnsealFlipped (FlippedSeal a) = unsafeCoerceP a

unsafeUnseal2 :: Sealed2 a -> a wX wY
unsafeUnseal2 (Sealed2 a) = unsafeCoerceP a

unseal :: (forall wX . a wX -> b) -> Sealed a -> b
unseal f x = f (unsafeUnseal x)

-- laziness property:
-- unseal (const True) undefined == True

unsealM :: Monad m => m (Sealed a) -> (forall wX . a wX -> m b) -> m b
unsealM m1 m2 = do sx <- m1
                   unseal m2 sx

liftSM :: Monad m => (forall wX . a wX -> b) -> m (Sealed a) -> m b
liftSM f m = do sx <- m
                return (unseal f sx)

mapSeal :: (forall wX . a wX -> b wX) -> Sealed a -> Sealed b
mapSeal f = unseal (seal . f)

mapFlipped :: (forall wX . a wX wY -> b wX wZ) -> FlippedSeal a wY -> FlippedSeal b wZ
mapFlipped f (FlippedSeal x) = FlippedSeal (f x)

unseal2 :: (forall wX wY . a wX wY -> b) -> Sealed2 a -> b
unseal2 f a = f (unsafeUnseal2 a)

mapSeal2 :: (forall wX wY . a wX wY -> b wX wY) -> Sealed2 a -> Sealed2 b
mapSeal2 f = unseal2 (seal2 . f)

unsealFlipped :: (forall wX wY . a wX wY -> b) -> FlippedSeal a wZ -> b
unsealFlipped f (FlippedSeal a) = f a

instance Show1 a => Show (Sealed a) where
    showsPrec d (Sealed x) = showParen (d > appPrec) $ showString "Sealed " . showsPrec1 (appPrec + 1) x
instance Show2 a => Show (Sealed2 a) where
    showsPrec d (Sealed2 x) = showParen (d > appPrec) $ showString "Sealed2 " . showsPrec2 (appPrec + 1) x

-- |'Poly' is similar to 'Sealed', but the type argument is
-- universally quantified instead of being existentially quantified.
newtype Poly a = Poly { unPoly :: forall wX . a wX }

-- |'Stepped' is a type level composition operator.
-- For example, @ 'Stepped' ('Sealed' p) @ is equivalent to 
-- @ \\x -> 'Sealed' (p x) @
newtype Stepped (f :: (* -> *) -> *) a wX = Stepped { unStepped :: f (a wX) }

-- |'FreeLeft' p is @ \forall x . \exists y . p x y @
-- In other words the caller is free to specify the left witness,
-- and then the right witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that @ y @ is dependent on the @ x @ that is supplied.
-- This is why 'Stepped' is needed, rather than writing the more obvious
-- 'Sealed' ('Poly' p) which would notionally have the same quantification
-- of the type witnesses.
newtype FreeLeft p = FLInternal (Poly (Stepped Sealed p))

-- |'FreeLeft' p is @ \forall y . \exists x . p x y @
-- In other words the caller is free to specify the right witness,
-- and then the left witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that @ x @ is dependent on the @ y @ that is supplied.
newtype FreeRight p = FRInternal (Poly (FlippedSeal p))

-- |Unwrap a 'FreeLeft' value
unFreeLeft :: FreeLeft p -> Sealed (p wX)
unFreeLeft (FLInternal x) = unStepped (unPoly x)

-- |Unwrap a 'FreeRight' value
unFreeRight :: FreeRight p -> FlippedSeal p wX
unFreeRight (FRInternal x) = unPoly x

-- |'Gap' abstracts over 'FreeLeft' and 'FreeRight' for code constructing these values
class Gap w where
  -- |An empty 'Gap', e.g. 'NilFL' or 'NilRL'
  emptyGap :: (forall wX . p wX wX) -> w p
  -- |A 'Gap' constructed from a completely polymorphic value, for example the constructors
  -- for primitive patches
  freeGap :: (forall wX wY . p wX wY) -> w p

  -- |Compose two 'Gap' values together in series, e.g. 'joinGap (+>+)' or 'joinGap (:>:)'
  joinGap :: (forall wX wY wZ . p wX wY -> q wY wZ -> r wX wZ) -> w p -> w q -> w r

instance Gap FreeLeft where
  emptyGap e = FLInternal (Poly (Stepped (Sealed e)))
  freeGap e =  FLInternal (Poly (Stepped (Sealed e)))
  joinGap op (FLInternal p) (FLInternal q)
    = FLInternal (Poly (case unPoly p of Stepped (Sealed p') -> case unPoly q of Stepped (Sealed q') -> Stepped (Sealed (p' `op` q'))))

instance Gap FreeRight where
  emptyGap e = FRInternal (Poly (FlippedSeal e))
  freeGap e =  FRInternal (Poly (FlippedSeal e))
  joinGap op (FRInternal p) (FRInternal q)
    = FRInternal (Poly (case unPoly q of FlippedSeal q' -> case unPoly p of FlippedSeal p' -> FlippedSeal (p' `op` q')))
