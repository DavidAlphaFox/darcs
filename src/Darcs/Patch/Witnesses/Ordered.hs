-- Copyright (C) 2007 David Roundy
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

{-# LANGUAGE CPP #-}

module Darcs.Patch.Witnesses.Ordered
    ( 
    -- * Directed Types
    -- $DirectedTypes
      (:>)(..)
    , (:<)(..)
    , FL(..)
    , RL(..)
    -- * Merge Types
    -- $MergeTypes
    , (:\/:)(..)
    , (:/\:)(..)
    , (:||:)(..)
    , Fork(..)
    -- * Functions
    , lengthFL
    , mapFL
    , mapFL_FL
    , spanFL
    , foldlFL
    , allFL
    , anyFL
    , filterFL
    , splitAtFL
    , splitAtRL
    , bunchFL
    , foldlRL
    , lengthRL
    , isShorterThanRL
    , mapRL
    , mapRL_RL
    , zipWithFL
    , filterOutFLFL
    , filterOutRLRL
    , filterRL
    , reverseFL
    , reverseRL
    , (+>+)
    , (+<+)
    , nullFL
    , concatFL
    , concatRL
    , consRLSealed
    , nullRL
    , toFL
    , dropWhileFL
    , dropWhileRL
    , spanFL_M
    , mapFL_FL_M
    , eqFL
    , eqFLRev
    , eqFLUnsafe
    ) where

#include "impossible.h"
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..)
    , flipSeal
    , Sealed(..)
    , FreeLeft
    , unFreeLeft
    , Sealed2(..)
    , seal
    )
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )

-- * Directed Types

-- $DirectedTypes
-- Darcs patches have a notion of transforming between contexts. This
-- naturally leads us to container types that are \"directed\" and 
-- transform from one context to another.
-- 
-- For example, the swap of names of files x and y could be represented 
-- with the following sequence of patches:
-- 
-- @ Move x z ':>' Move y x ':>' Move z y @
-- 
-- or using forward lists, like
-- 
-- @ Move x z ':>:' Move y x ':>:' Move z y ':>:' NilFl @

-- | Directed Forward Pairs
data (a1 :> a2) wX wY = forall wZ . (a1 wX wZ) :> (a2 wZ wY)
infixr 1 :>

-- | Directed Reverse Pairs
data (a1 :< a2) wX wY = forall wZ . (a1 wZ wY) :< (a2 wX wZ)
infix 1 :<

-- | Forward lists
data FL a wX wZ where
    (:>:) :: a wX wY -> FL a wY wZ -> FL a wX wZ
    NilFL :: FL a wX wX

-- | Reverse lists
data RL a wX wZ where
    (:<:) :: a wY wZ -> RL a wX wY -> RL a wX wZ
    NilRL :: RL a wX wX

instance Show2 a => Show (FL a wX wZ) where
   showsPrec _ NilFL = showString "NilFL"
   showsPrec d (x :>: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :>: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show1 (FL a wX) where
   showDict1 = ShowDictClass

instance Show2 a => Show2 (FL a) where
   showDict2 = ShowDictClass

instance Show2 a => Show (RL a wX wZ) where
   showsPrec _ NilRL = showString "NilRL"
   showsPrec d (x :<: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :<: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show1 (RL a wX) where
   showDict1 = ShowDictClass

instance Show2 a => Show2 (RL a) where
   showDict2 = ShowDictClass

instance (Show2 a, Show2 b) => Show1 ((a :> b) wX) where
   showDict1 = ShowDictClass

-- * Merge Types

-- $MergeTypes
-- When we have two patches which commute and share the same pre-context we can
-- merge the patches. Whenever patches, or sequences of patches, share a
-- pre-context we say they are Forking Pairs (':\/:'). The same way, when
-- patches or sequences of patches, share a post-context we say they are
-- Joining Pairs (':/\:').
-- 
-- The following diagram shows the symmetry of merge types:
-- 
-- @          wZ
--         ':/\:'
--     a3 &#47;    &#92; a4  
--       &#47;      &#92;    
--      wX      wY       
--       &#92;      &#47;    
--     a1 &#92;    &#47; a2  
--         ':\/:'      
--          wZ
-- @

-- 
-- (non-haddock version)
--      wZ
--     :/\:
-- a3 /    \ a4
--   /      \
--  wX      wY
--   \      /
-- a1 \    / a2
--     :\/:
--      wZ
-- 

infix 1 :/\:, :\/:, :||:
-- | Forking Pairs (Implicit starting context)
data (a1 :\/: a2) wX wY = forall wZ . (a1 wZ wX) :\/: (a2 wZ wY)

-- | Joining Pairs
data (a3 :/\: a4) wX wY = forall wZ . (a3 wX wZ) :/\: (a4 wY wZ)

-- | Forking Pair (Explicit starting context)
-- 
-- @      wX     wY       
--       &#92;     &#47;    
--        &#92;   &#47;
--         &#92; &#47;     
--          wU
--          &#124;
--          &#124;
--          &#124;
--          wA
-- @

-- 
-- (non-haddock version)
-- 
--  wX      wY
--   \      /
--    \    /
--     \  /
--      wU
--      |
--      |
--      |
--      wA
-- 

data Fork common left right wA wX wY =
    forall wU. Fork (common wA wU) (left wU wX) (right wU wY)

-- | Parallel Pairs
data (a1 :||: a2) wX wY = (a1 wX wY) :||: (a2 wX wY)

instance (Show2 a, Show2 b) => Show ( (a :> b) wX wY ) where
    showsPrec d (x :> y) = showOp2 1 ":>" d x y

instance (MyEq a, MyEq b) => MyEq (a :> b) where
    (a1 :> b1) =\/= (a2 :> b2) | IsEq <- a1 =\/= a2 = b1 =\/= b2
                               | otherwise = NotEq

instance (MyEq a, MyEq b) => Eq ((a :> b) wX wY) where
    (==) = unsafeCompare

instance (MyEq a, MyEq b) => MyEq (a :< b) where
    (a1 :< b1) =\/= (a2 :< b2) | IsEq <- b1 =\/= b2 = a1 =\/= a2
                               | otherwise = NotEq

instance (MyEq a, MyEq b) => Eq ((a :< b) wX wY) where
    (==) = unsafeCompare

instance (Show2 a, Show2 b) => Show2 (a :> b) where
    showDict2 = ShowDictClass

instance (Show2 a, Show2 b) => Show ( (a :\/: b) wX wY ) where
    showsPrec d (x :\/: y) = showOp2 9 ":\\/:" d x y

instance (Show2 a, Show2 b) => Show2 (a :\/: b) where
    showDict2 = ShowDictClass

instance (Show2 a, Show2 b) => Show ( (a :/\: b) wX wY ) where
    showsPrec d (x :/\: y) = showOp2 1 ":/\\:" d x y

instance (Show2 a, Show2 b) => Show2 ( (a :/\: b) ) where
    showDict2 = ShowDictClass

-- * Functions

infixr 5 :>:, :<:, +>+, +<+

nullFL :: FL a wX wZ -> Bool
nullFL NilFL = True
nullFL _ = False

nullRL :: RL a wX wZ -> Bool
nullRL NilRL = True
nullRL _ = False

-- | @filterOutFLFL p xs@ deletes any @x@ in @xs@ for which @p x == IsEq@
--   (indicating that @x@ has no effect as far as we are concerned, and can be
--    safely removed from the chain)
filterOutFLFL :: (forall wX wY . p wX wY -> EqCheck wX wY) -> FL p wW wZ -> FL p wW wZ
filterOutFLFL _ NilFL = NilFL
filterOutFLFL f (x:>:xs) | IsEq <- f x = filterOutFLFL f xs
                    | otherwise = x :>: filterOutFLFL f xs

filterOutRLRL :: (forall wX wY . p wX wY -> EqCheck wX wY) -> RL p wW wZ -> RL p wW wZ
filterOutRLRL _ NilRL = NilRL
filterOutRLRL f (x:<:xs) | IsEq <- f x = filterOutRLRL f xs
                         | otherwise = x :<: filterOutRLRL f xs

filterRL :: (forall wX wY . p wX wY -> Bool) -> RL p wA wB ->  [Sealed2 p]
filterRL _ NilRL = []
filterRL f (x :<: xs) | f x = Sealed2 x : (filterRL f xs)
                      | otherwise = filterRL f xs

(+>+) :: FL a wX wY -> FL a wY wZ -> FL a wX wZ
NilFL +>+ ys = ys
(x:>:xs) +>+ ys = x :>: xs +>+ ys

(+<+) :: RL a wY wZ -> RL a wX wY -> RL a wX wZ
NilRL +<+ ys = ys
(x:<:xs) +<+ ys = x :<: xs +<+ ys

reverseFL :: FL a wX wZ -> RL a wX wZ
reverseFL xs = r NilRL xs
  where r :: RL a wL wM -> FL a wM wO -> RL a wL wO
        r ls NilFL = ls
        r ls (a:>:as) = r (a:<:ls) as

reverseRL :: RL a wX wZ -> FL a wX wZ
reverseRL xs = r NilFL xs -- r (xs :> NilFL)
  where r :: FL a wM wO -> RL a wL wM -> FL a wL wO
        r ls NilRL = ls
        r ls (a:<:as) = r (a:>:ls) as

concatFL :: FL (FL a) wX wZ -> FL a wX wZ
concatFL NilFL = NilFL
concatFL (a:>:as) = a +>+ concatFL as

concatRL :: RL (RL a) wX wZ -> RL a wX wZ
concatRL NilRL = NilRL
concatRL (a:<:as) = a +<+ concatRL as

spanFL :: (forall wW wY . a wW wY -> Bool) -> FL a wX wZ -> (FL a :> FL a) wX wZ
spanFL f (x:>:xs) | f x = case spanFL f xs of
                            ys :> zs -> (x:>:ys) :> zs
spanFL _ xs = NilFL :> xs

spanFL_M :: forall a m wX wZ. Monad m =>
            (forall wW wY . a wW wY -> m Bool) -> FL a wX wZ
            -> m ((FL a :> FL a) wX wZ)
spanFL_M f (x:>:xs) =
    do
      continue <- f x
      if continue
       then do (ys :> zs) <- spanFL_M f xs
               return $ (x :>: ys) :> zs
       else return $ NilFL :> (x :>: xs)

spanFL_M _ (NilFL) = return $ NilFL :> NilFL

splitAtFL :: Int -> FL a wX wZ -> (FL a :> FL a) wX wZ
splitAtFL 0 xs = NilFL :> xs
splitAtFL _ NilFL = NilFL :> NilFL
splitAtFL n (x:>:xs) = case splitAtFL (n-1) xs of
                       (xs':>xs'') -> (x:>:xs' :> xs'')

splitAtRL :: Int -> RL a wX wZ -> (RL a :< RL a) wX wZ
splitAtRL 0 xs = NilRL :< xs
splitAtRL _ NilRL = NilRL :< NilRL
splitAtRL n (x:<:xs) = case splitAtRL (n-1) xs of
                       (xs':<xs'') -> (x:<:xs' :< xs'')

-- 'bunchFL n' groups patches into batches of n, except that it always puts
-- the first patch in its own group, this being a recognition that the
-- first patch is often *very* large.

bunchFL :: Int -> FL a wX wY -> FL (FL a) wX wY
bunchFL _ NilFL = NilFL
bunchFL n (x:>:xs) = (x :>: NilFL) :>: bFL xs
    where bFL :: FL a wX wY -> FL (FL a) wX wY
          bFL NilFL = NilFL
          bFL bs = case splitAtFL n bs of
                   a :> b -> a :>: bFL b


allFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> Bool
allFL f xs = and $ mapFL f xs

anyFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> Bool
anyFL f xs = or $ mapFL f xs

foldlFL :: (forall wW wY . a -> b wW wY -> a) -> a -> FL b wX wZ -> a
foldlFL _ x NilFL = x
foldlFL f x (y:>:ys) = foldlFL f (f x y) ys

foldlRL :: (forall wW wY . a -> b wW wY -> a) -> a -> RL b wX wZ -> a
foldlRL _ x NilRL = x
foldlRL f x (y:<:ys) = foldlRL f (f x y) ys

mapFL_FL :: (forall wW wY . a wW wY -> b wW wY) -> FL a wX wZ -> FL b wX wZ
mapFL_FL _ NilFL = NilFL
mapFL_FL f (a:>:as) = f a :>: mapFL_FL f as

mapFL_FL_M :: Monad m => (forall wW wY . a wW wY -> m (b wW wY)) -> FL a wX wZ -> m (FL b wX wZ)
mapFL_FL_M _ NilFL = return NilFL
mapFL_FL_M f (a:>:as) = do b <- f a
                           bs <- mapFL_FL_M f as
                           return (b:>:bs)

zipWithFL :: (forall wX wY . a -> p wX wY -> q wX wY)
          -> [a] -> FL p wW wZ -> FL q wW wZ
zipWithFL f (x:xs) (y :>: ys) = f x y :>: zipWithFL f xs ys
zipWithFL _ _ NilFL = NilFL
zipWithFL _ [] (_:>:_) = bug "zipWithFL called with too short a list"

mapRL_RL :: (forall wW wY . a wW wY -> b wW wY) -> RL a wX wZ -> RL b wX wZ
mapRL_RL _ NilRL = NilRL
mapRL_RL f (a:<:as) = f a :<: mapRL_RL f as

mapFL :: (forall wW wZ . a wW wZ -> b) -> FL a wX wY -> [b]
mapFL _ NilFL = []
mapFL f (a :>: b) = f a : mapFL f b

filterFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> [Sealed2 a]
filterFL _ NilFL = []
filterFL f (a :>: b) = if f a
                       then (Sealed2 a):(filterFL f b)
                       else filterFL f b

mapRL :: (forall wW wZ . a wW wZ -> b) -> RL a wX wY -> [b]
mapRL _ NilRL = []
mapRL f (a :<: b) = f a : mapRL f b

lengthFL :: FL a wX wZ -> Int
lengthFL xs = l xs 0
  where l :: FL a wX wZ -> Int -> Int
        l NilFL n = n
        l (_:>:as) n = l as $! n+1

lengthRL :: RL a wX wZ -> Int
lengthRL xs = l xs 0
  where l :: RL a wX wZ -> Int -> Int
        l NilRL n = n
        l (_:<:as) n = l as $! n+1

isShorterThanRL :: RL a wX wY -> Int -> Bool
isShorterThanRL _ n | n <= 0 = False
isShorterThanRL NilRL _ = True
isShorterThanRL (_:<:xs) n = isShorterThanRL xs (n-1)

consRLSealed :: a wY wZ -> FlippedSeal (RL a) wY -> FlippedSeal (RL a) wZ
consRLSealed a (FlippedSeal as) = flipSeal $ a :<: as

toFL :: [FreeLeft a] -> Sealed (FL a wX)
toFL [] = Sealed NilFL
toFL (x:xs) = case unFreeLeft x of Sealed y -> case toFL xs of Sealed ys -> Sealed (y :>: ys)

dropWhileFL :: (forall wX wY . a wX wY -> Bool) -> FL a wR wV -> FlippedSeal (FL a) wV
dropWhileFL _ NilFL       = flipSeal NilFL
dropWhileFL p xs@(x:>:xs')
          | p x       = dropWhileFL p xs'
          | otherwise = flipSeal xs

dropWhileRL :: (forall wX wY . a wX wY -> Bool) -> RL a wR wV -> Sealed (RL a wR)
dropWhileRL _ NilRL = seal NilRL
dropWhileRL p xs@(x:<:xs')
          | p x       = dropWhileRL p xs'
          | otherwise = seal xs

-- |Check that two 'FL's are equal element by element.
-- This differs from the 'MyEq' instance for 'FL' which
-- uses commutation.
eqFL :: MyEq a => FL a wX wY -> FL a wX wZ -> EqCheck wY wZ
eqFL NilFL NilFL = IsEq
eqFL (x:>:xs) (y:>:ys) | IsEq <- x =\/= y, IsEq <- eqFL xs ys = IsEq
eqFL _ _ = NotEq

eqFLRev :: MyEq a => FL a wX wZ -> FL a wY wZ -> EqCheck wX wY
eqFLRev NilFL NilFL = IsEq
eqFLRev (x:>:xs) (y:>:ys) | IsEq <- eqFLRev xs ys, IsEq <- x =/\= y = IsEq
eqFLRev _ _ = NotEq

eqFLUnsafe :: MyEq a => FL a wX wY -> FL a wZ wW -> Bool
eqFLUnsafe NilFL NilFL = True
eqFLUnsafe (x:>:xs) (y:>:ys) = unsafeCompare x y && eqFLUnsafe xs ys
eqFLUnsafe _ _ = False
