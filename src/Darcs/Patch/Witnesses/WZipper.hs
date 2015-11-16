-- Copyright (C) 2009 Florent Becker
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

module Darcs.Patch.Witnesses.WZipper
    ( FZipper(..)
    , focus
    , leftmost
    , left
    , rightmost
    , right
    , jokers
    , clowns
    , flToZipper
    , lengthFZ
    , nullFZ
    , toEnd
    , toStart
    )
where
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , nullFL
    , nullRL
    , lengthFL
    , lengthRL
    , reverseFL
    , reverseRL
    , (+<+)
    , (+>+)
    )
import Darcs.Patch.Witnesses.Sealed(Sealed2(..), Sealed(..), FlippedSeal(..))

-- forward zipper
data FZipper a wX wZ where
    FZipper :: RL a wX wY -> FL a wY wZ -> FZipper a wX wZ

-- Constructors
flToZipper :: FL a wX wY -> FZipper a wX wY
flToZipper = FZipper NilRL

--destructors
nullFZ :: FZipper a wX wY -> Bool
nullFZ (FZipper l r) = nullRL l && nullFL r

lengthFZ :: FZipper a wX wY -> Int
lengthFZ (FZipper l r) = lengthRL l + lengthFL r

focus :: FZipper a wX wY -> Maybe (Sealed2 a)
focus (FZipper _ (x :>: _)) = Just $ Sealed2 x
focus _ = Nothing

-- | \"Clowns to the left of me, jokers to the right.  Here I am, stuck
--   in the middle of you\"
--   <http://en.wikipedia.org/wiki/Stuck_in_the_Middle>
clowns :: FZipper a wX wY -> Sealed ((RL a) wX)
clowns (FZipper l _) = Sealed l

-- | See 'clowns'
jokers :: FZipper a wX wY -> FlippedSeal (FL a) wY
jokers (FZipper _ r) = FlippedSeal r

rightmost :: FZipper p wX wY -> Bool
rightmost (FZipper _ NilFL) = True
rightmost _ = False

right :: FZipper p wX wY -> FZipper p wX wY
right (FZipper l (b:>:r)) = FZipper (b :<: l) r
right x@(FZipper _ NilFL) = x

leftmost :: FZipper p wX wY -> Bool
leftmost (FZipper NilRL _) = True
leftmost _ = False

left :: FZipper p wX wY -> FZipper p wX wY
left (FZipper (b :<: l) r) = FZipper l (b :>: r)
left x@(FZipper NilRL _) = x

toEnd :: FZipper p wX wY -> FZipper p wX wY
toEnd (FZipper l r) = FZipper (reverseFL r +<+ l) NilFL

toStart :: FZipper p wX wY -> FZipper p wX wY
toStart (FZipper l r) = FZipper NilRL $ reverseRL l +>+ r
