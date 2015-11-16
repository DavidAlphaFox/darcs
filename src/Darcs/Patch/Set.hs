-- Copyright (C) 2003 David Roundy
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

{-# LANGUAGE CPP, EmptyDataDecls, StandaloneDeriving #-}

module Darcs.Patch.Set
    ( PatchSet(..)
    , Tagged(..)
    , SealedPatchSet
    , Origin
    , progressPatchSet
    , tags
    , emptyPatchSet
    , appendPSFL
    , newset2RL
    , newset2FL
    ) where

import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Witnesses.Sealed ( Sealed )
import Darcs.Patch.Witnesses.Ordered
    ( FL, RL(..), (+<+), reverseFL, reverseRL,
    mapRL_RL, concatRL, mapRL )
import Darcs.Patch.Witnesses.Show ( Show1(..), Show2(..), ShowDict(ShowDictClass) )

import Darcs.Util.Progress ( progress )

-- |'Origin' is a type used to represent the initial context of a repo.
data Origin

type SealedPatchSet p wStart = Sealed ((PatchSet p) wStart)

-- |The patches in a repository are stored in chunks broken up at \"clean\"
-- tags. A tag is clean if the only patches before it in the current
-- repository ordering are ones that the tag depends on (either directly
-- or indirectly). Each chunk is stored in a separate inventory file on disk.
--
-- A 'PatchSet' represents a repo's history as the list of patches since the
-- last clean tag, and then a list of patch lists each delimited by clean tags.
data PatchSet p wStart wY where
    PatchSet :: RL (PatchInfoAnd p) wX wY -> RL (Tagged p) wStart wX
             -> PatchSet p wStart wY

deriving instance Show2 p => Show (PatchSet p wStart wY)

instance Show2 p => Show1 (PatchSet p wStart) where
    showDict1 = ShowDictClass

instance Show2 p => Show2 (PatchSet p) where
    showDict2 = ShowDictClass


emptyPatchSet :: PatchSet p wX wX
emptyPatchSet = PatchSet NilRL NilRL

-- |A 'Tagged' is a single chunk of a 'PatchSet'.
-- It has a 'PatchInfo' representing a clean tag,
-- the hash of the previous inventory (if it exists),
-- and the list of patches since that previous inventory.
data Tagged p wX wZ where
    Tagged :: PatchInfoAnd p wY wZ -> Maybe String
           -> RL (PatchInfoAnd p) wX wY -> Tagged p wX wZ

deriving instance Show2 p => Show (Tagged p wX wZ)

instance Show2 p => Show1 (Tagged p wX) where
    showDict1 = ShowDictClass

instance Show2 p => Show2 (Tagged p) where
    showDict2 = ShowDictClass


-- |'newset2RL' takes a 'PatchSet' and returns an equivalent, linear 'RL' of
-- patches.
newset2RL :: PatchSet p wStart wX -> RL (PatchInfoAnd p) wStart wX
newset2RL (PatchSet ps ts) = ps +<+ concatRL (mapRL_RL ts2rl ts)
  where
    ts2rl :: Tagged p wY wZ -> RL (PatchInfoAnd p) wY wZ
    ts2rl (Tagged t _ ps2) = t :<: ps2

-- |'newset2FL' takes a 'PatchSet' and returns an equivalent, linear 'FL' of
-- patches.
newset2FL :: PatchSet p wStart wX -> FL (PatchInfoAnd p) wStart wX
newset2FL = reverseRL . newset2RL

-- |'appendPSFL' takes a 'PatchSet' and a 'FL' of patches that "follow" the
-- PatchSet, and concatenates the patches into the PatchSet.
appendPSFL :: PatchSet p wStart wX -> FL (PatchInfoAnd p) wX wY
           -> PatchSet p wStart wY
appendPSFL (PatchSet ps ts) newps = PatchSet (reverseFL newps +<+ ps) ts

-- |Runs a progress action for each tag and patch in a given PatchSet, using
-- the passed progress message. Does not alter the PatchSet.
progressPatchSet :: String -> PatchSet p wStart wX -> PatchSet p wStart wX
progressPatchSet k (PatchSet ps ts) =
    PatchSet (mapRL_RL prog ps) $ mapRL_RL progressTagged ts
  where
    prog = progress k

    progressTagged :: Tagged p wY wZ -> Tagged p wY wZ
    progressTagged (Tagged t h tps) = Tagged (prog t) h (mapRL_RL prog tps)

-- |'tags' returns the PatchInfos corresponding to the tags of a given
-- 'PatchSet'.
tags :: PatchSet p wStart wX -> [PatchInfo]
tags (PatchSet _ ts) = mapRL taggedTagInfo ts
  where
    taggedTagInfo :: Tagged p wY wZ -> PatchInfo
    taggedTagInfo (Tagged t _ _) = info t
