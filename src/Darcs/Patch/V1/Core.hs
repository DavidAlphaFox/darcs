{-# LANGUAGE CPP #-}
module Darcs.Patch.V1.Core
    ( Patch(..),
      isMerger, mergerUndo
    ) where

import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(ListFormatV1) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Patch.Prim ( FromPrim(..), PrimOf, PrimPatchBase, PrimPatch )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase )
import Darcs.Patch.Repair ( Check )

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL )
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..)
    , ShowDict(ShowDictClass)
    , appPrec, showsPrec2
    )

#include "impossible.h"

-- This haddock could be put on the individual bits of Merger instead
-- once haddock supports doc comments on GADT constructors
{- |
The format of a merger is @Merger undos unwindings conflicting original@.

@undos@ = the effect of the merger

@unwindings@ = TODO: eh?

@conflicting@ = the patch we conflict with

@original@ = the patch we really are
-}
data Patch prim wX wY where
    PP :: prim wX wY -> Patch prim wX wY
    Merger :: FL (Patch prim) wX wY
           -> RL (Patch prim) wX wB
           -> Patch prim wC wB
           -> Patch prim wC wD
           -> Patch prim wX wY
    Regrem :: FL (Patch prim) wX wY
           -> RL (Patch prim) wX wB
           -> Patch prim wC wB
           -> Patch prim wC wA
           -> Patch prim wY wX

instance Show2 prim => Show (Patch prim wX wY)  where
    showsPrec d (PP p) =
        showParen (d > appPrec) $ showString "PP " . showsPrec2 (appPrec + 1) p
    showsPrec d (Merger undos unwindings conflicting original) =
        showParen (d > appPrec) $
            showString "Merger " . showsPrec2 (appPrec + 1) undos .
            showString " " . showsPrec2 (appPrec + 1) unwindings .
            showString " " . showsPrec2 (appPrec + 1) conflicting .
            showString " " . showsPrec2 (appPrec + 1) original
    showsPrec d (Regrem undos unwindings conflicting original) =
        showParen (d > appPrec) $
            showString "Regrem " . showsPrec2 (appPrec + 1) undos .
            showString " " . showsPrec2 (appPrec + 1) unwindings .
            showString " " . showsPrec2 (appPrec + 1) conflicting .
            showString " " . showsPrec2 (appPrec + 1) original

instance Show2 prim => Show1 (Patch prim wX) where
    showDict1 = ShowDictClass

instance Show2 prim => Show2 (Patch prim) where
    showDict2 = ShowDictClass

instance MaybeInternal (Patch prim)
instance NameHack (Patch prim)
instance RecontextRebase (Patch prim)

instance PrimPatch prim => PrimPatchBase (Patch prim) where
    type PrimOf (Patch prim) = prim

instance FromPrim (Patch prim) where
    fromPrim = PP

isMerger :: Patch prim wA wB -> Bool
isMerger (Merger{}) = True
isMerger (Regrem{}) = True
isMerger _ = False

mergerUndo :: Patch prim wX wY -> FL (Patch prim) wX wY
mergerUndo (Merger undo _ _ _) = undo
mergerUndo _ = impossible

instance PatchListFormat (Patch prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect Patch to be used with any other argument
   -- anyway, so it doesn't matter.
   patchListFormat = ListFormatV1

instance Check (Patch prim)
   -- no checks

instance PatchDebug prim => PatchDebug (Patch prim)
