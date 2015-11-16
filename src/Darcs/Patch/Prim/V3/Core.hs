{-# LANGUAGE CPP, OverloadedStrings, StandaloneDeriving #-}

-- Copyright (C) 2011 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.


module Darcs.Patch.Prim.V3.Core
       ( Prim(..), Hunk(..), UUID(..), Location, Object(..), touches, hunkEdit )
       where

import qualified Data.ByteString as BS

import Darcs.Patch.Witnesses.Eq ( MyEq(..) )
import Darcs.Patch.Witnesses.Show ( Show1(..), Show2(..), ShowDict(ShowDictClass) )
import Darcs.Patch.FileHunk( IsHunk(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Prim.Class ( PrimConstruct(..), PrimClassify(..) )
import Darcs.Patch.Prim.V3.ObjectMap

-- TODO: elaborate

data Hunk wX wY where
  Hunk :: !Int -> BS.ByteString -> BS.ByteString -> Hunk wX wY
    deriving Show

instance Show1 (Hunk wX) where
    showDict1 = ShowDictClass

instance Show2 Hunk where
    showDict2 = ShowDictClass

invertHunk :: Hunk wX wY -> Hunk wY wX
invertHunk (Hunk off old new) = Hunk off new old

hunkEdit :: Hunk wX wY -> BS.ByteString -> BS.ByteString
hunkEdit (Hunk off old new) bs = case splice bs (off) (off + BS.length old) of
  x | x == old -> BS.concat [ BS.take off bs, new, BS.drop (off + BS.length old) bs ]
    | otherwise -> error $ "error applying hunk: " ++ show off ++ " " ++ show old ++ " "
                        ++ show new ++ " to " ++ show bs
  where splice bs' x y = BS.drop x $ BS.take y bs'

instance MyEq Hunk where
  unsafeCompare (Hunk i x y) (Hunk i' x' y') = i == i' && x == x' && y == y'

data Prim wX wY where
    BinaryHunk :: !UUID -> Hunk wX wY -> Prim wX wY
    TextHunk :: !UUID -> Hunk wX wY -> Prim wX wY

    -- TODO: String is not the right type here. However, what it represents is
    -- a single file *name* (not a path). No slashes allowed, no "." and ".."
    -- allowed either.
    Manifest :: !UUID -> Location -> Prim wX wY
    Demanifest :: !UUID -> Location -> Prim wX wY
    Move :: !UUID -> Location -> Location -> Prim wX wY
    Identity :: Prim wX wX

deriving instance Show (Prim wX wY)

instance Show1 (Prim wX) where
    showDict1 = ShowDictClass

instance Show2 Prim where
    showDict2 = ShowDictClass

touches :: Prim wX wY -> [UUID]
touches (BinaryHunk x _) = [x]
touches (TextHunk x _) = [x]
touches (Manifest _ (x, _)) = [x]
touches (Demanifest _ (x, _)) = [x]
touches (Move _ (x, _) (y, _)) = [x, y]
touches Identity = []

-- TODO: PrimClassify doesn't make sense for V3 prims
instance PrimClassify Prim where
   primIsAddfile _ = False
   primIsRmfile _ = False
   primIsAdddir _ = False
   primIsRmdir _ = False
   primIsHunk _ = False
   primIsMove _ = False
   primIsBinary _ = False
   primIsTokReplace _ = False
   primIsSetpref _ = False
   is_filepatch _ = Nothing

-- TODO: PrimConstruct makes no sense for V3 prims
instance PrimConstruct Prim where
   addfile _ = error "PrimConstruct addfile"
   rmfile _ = error "PrimConstruct rmfile"
   adddir _ = error "PrimConstruct adddir"
   rmdir _ = error "PrimConstruct rmdir"
   move _ _ = error "PrimConstruct move"
   changepref _ _ _ = error "PrimConstruct changepref"
   hunk _ _ _ _ = error "PrimConstruct hunk"
   tokreplace _ _ _ _ = error "PrimConstruct tokreplace"
   binary _ _ _ = error "PrimConstruct binary"
   primFromHunk _ = error "PrimConstruct primFromHunk"
   anIdentity = Identity

instance IsHunk Prim where
   isHunk _ = Nothing

instance Invert Prim where
   invert (BinaryHunk x h) = BinaryHunk x $ invertHunk h
   invert (TextHunk x h) = TextHunk x $ invertHunk h
   invert (Manifest x y) = Demanifest x y
   invert (Demanifest x y) = Manifest x y
   invert (Move x y z) = Move x z y
   invert Identity = Identity

instance PatchInspect Prim where
    -- We don't need this for V3. Slashes are not allowed in Manifest and
    -- Demanifest patches and nothing else uses working-copy paths.
    listTouchedFiles _ = []

    -- TODO (used for --match 'hunk ...', presumably)
    hunkMatches _ _ = False

instance MyEq Prim where
    unsafeCompare (BinaryHunk a b) (BinaryHunk c d) = a == c && b `unsafeCompare` d
    unsafeCompare (TextHunk a b) (TextHunk c d) = a == c && b `unsafeCompare` d
    unsafeCompare (Manifest a b) (Manifest c d) = a == c && b == d
    unsafeCompare (Demanifest a b) (Demanifest c d) = a == c && b == d
    unsafeCompare Identity Identity = True
    unsafeCompare _ _ = False

instance Eq (Prim wX wY) where
    (==) = unsafeCompare
