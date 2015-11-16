-- Copyright (C) 2006 David Roundy
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


module Darcs.Patch.PatchInfoAnd ( Hopefully(..), SimpleHopefully(..), PatchInfoAnd(..),
                         WPatchInfo, unWPatchInfo, compareWPatchInfo,
                         piap, n2pia, patchInfoAndPatch,
                         fmapPIAP, fmapFLPIAP,
                         conscientiously, hopefully, info, winfo,
                         hopefullyM, createHashed, extractHash,
                         actually, unavailable, patchDesc ) where

import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.Printer
    ( Doc, renderString, errorDoc, text, ($$), vcat
    , RenderMode(..) )
import Darcs.Patch.Info ( PatchInfo, showPatchInfoUI, justName )
import Darcs.Patch ( Named, patch2patchinfo )
import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Named ( fmapNamed, fmapFL_Named )
import Darcs.Patch.Prim ( PrimPatchBase(..) )
import Darcs.Patch.Patchy ( Patchy, ReadPatch(..), Apply(..), Invert(..),
                            ShowPatch(..), Commute(..), PatchInspect(..) )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Repair ( Repair(..), RepairToFL )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), (:\/:)(..), (:/\:)(..), FL, mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1(..), Show2(..), ShowDict(ShowDictClass) )
import Darcs.Util.Exception ( prettyException )
import Storage.Hashed.Tree( Tree )

import Control.Applicative ( (<$>) )

-- | @'Hopefully' p C@ @(x y)@ is @'Either' String (p C@ @(x y))@ in a
-- form adapted to darcs patches. The @C@ @(x y)@ represents the type
-- witness for the patch that should be there. The @Hopefully@ type
-- just tells whether we expect the patch to be hashed or not, and
-- 'SimpleHopefully' does the real work of emulating
-- 'Either'. @Hopefully sh@ represents an expected unhashed patch, and
-- @Hashed hash sh@ represents an expected hashed patch with its hash.
data Hopefully a wX wY = Hopefully (SimpleHopefully a wX wY) | Hashed String (SimpleHopefully a wX wY)
    deriving Show

-- | @SimpleHopefully@ is a variant of @Either String@ adapted for
-- type witnesses. @Actually@ is the equivalent of @Right@, while
-- @Unavailable@ is @Left@.
data SimpleHopefully a wX wY = Actually (a wX wY) | Unavailable String
    deriving Show

-- | @'PatchInfoAnd' p wA wB@ represents a hope we have to get a
-- patch through its info. We're not sure we have the patch, but we
-- know its info.
data PatchInfoAnd p wA wB = PIAP !PatchInfo (Hopefully (Named p) wA wB)
    deriving Show

instance Show2 p => Show1 (PatchInfoAnd p wX) where
    showDict1 = ShowDictClass

instance Show2 p => Show2 (PatchInfoAnd p) where
    showDict2 = ShowDictClass

instance PrimPatchBase p => PrimPatchBase (PatchInfoAnd p) where
   type PrimOf (PatchInfoAnd p) = PrimOf p

-- | @'WPatchInfo' wA wB@ represents the info of a patch, marked with
-- the patch's witnesses.
newtype WPatchInfo wA wB = WPatchInfo { unWPatchInfo :: PatchInfo }

-- This is actually unsafe if we ever commute patches and then compare them
-- using this function. TODO: consider adding an extra existential to WPatchInfo
-- (as with TaggedPatch in Darcs.Patch.Choices)
compareWPatchInfo :: WPatchInfo wA wB -> WPatchInfo wC wD -> EqCheck (wA, wB) (wC, wD)
compareWPatchInfo (WPatchInfo x) (WPatchInfo y) = if x == y then unsafeCoerceP IsEq else NotEq

instance MyEq WPatchInfo where
   WPatchInfo x `unsafeCompare` WPatchInfo y = x == y

fmapH :: (a wX wY -> b wW wZ) -> Hopefully a wX wY -> Hopefully b wW wZ
fmapH f (Hopefully sh) = Hopefully (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e
fmapH f (Hashed h sh) = Hashed h (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e

info :: PatchInfoAnd p wA wB -> PatchInfo
info (PIAP i _) = i

patchDesc :: forall p wX wY . PatchInfoAnd p wX wY -> String
patchDesc p = justName $ info p

winfo :: PatchInfoAnd p wA wB -> WPatchInfo wA wB
winfo (PIAP i _) = WPatchInfo i

-- | @'piap' i p@ creates a PatchInfoAnd containing p with info i.
piap :: PatchInfo -> Named p wA wB -> PatchInfoAnd p wA wB
piap i p = PIAP i (Hopefully $ Actually p)

-- | @n2pia@ creates a PatchInfoAnd representing a @Named@ patch.
n2pia :: Named p wX wY -> PatchInfoAnd p wX wY
n2pia x = patch2patchinfo x `piap` x

patchInfoAndPatch :: PatchInfo -> Hopefully (Named p) wA wB -> PatchInfoAnd p wA wB
patchInfoAndPatch =  PIAP

fmapPIAP :: (forall wA wB . p wA wB -> q wA wB) -> PatchInfoAnd p wX wY -> PatchInfoAnd q wX wY
fmapPIAP f (PIAP i hp) = PIAP i (fmapH (fmapNamed f) hp)

fmapFLPIAP :: (FL p wX wY -> FL q wX wY) -> PatchInfoAnd p wX wY -> PatchInfoAnd q wX wY
fmapFLPIAP f (PIAP i hp) = PIAP i (fmapH (fmapFL_Named f) hp)

-- | @'hopefully' hp@ tries to get a patch from a 'PatchInfoAnd'
-- value. If it fails, it outputs an error \"failed to read patch:
-- \<description of the patch>\". We get the description of the patch
-- from the info part of 'hp'
hopefully :: PatchInfoAnd p wA wB -> Named p wA wB
hopefully = conscientiously $ \e -> text "failed to read patch:" $$ e

-- | @'conscientiously' er hp@ tries to extract a patch from a 'PatchInfoAnd'.
-- If it fails, it applies the error handling function @er@ to a description
-- of the patch info component of @hp@.
conscientiously :: (Doc -> Doc)
                -> PatchInfoAnd p wA wB -> Named p wA wB
conscientiously er (PIAP pinf hp) =
    case hopefully2either hp of
      Right p -> p
      Left e -> errorDoc $ er (showPatchInfoUI pinf $$ text e)

-- | @hopefullyM@ is a version of @hopefully@ which calls @fail@ in a
-- monad instead of erroring.
hopefullyM :: Monad m => PatchInfoAnd p wA wB -> m (Named p wA wB)
hopefullyM (PIAP pinf hp) = case hopefully2either hp of
                              Right p -> return p
                              Left e -> fail $ renderString Encode
                                                     (showPatchInfoUI pinf $$ text e)

-- Any recommendations for a nice adverb to name the below?
hopefully2either :: Hopefully a wX wY -> Either String (a wX wY)
hopefully2either (Hopefully (Actually p)) = Right p
hopefully2either (Hashed _ (Actually p)) = Right p
hopefully2either (Hopefully (Unavailable e)) = Left e
hopefully2either (Hashed _ (Unavailable e)) = Left e

actually :: a wX wY -> Hopefully a wX wY
actually = Hopefully . Actually

createHashed :: String -> (String -> IO (Sealed (a wX))) -> IO (Sealed (Hopefully a wX))
createHashed h f = mapSeal (Hashed h) `fmap` unsafeInterleaveIO (f' `catchNonSignal` handler)
  where
  f' = do Sealed x <- f h
          return (Sealed (Actually x))
  handler e = return $ seal $ Unavailable $ prettyException e

extractHash :: PatchInfoAnd p wA wB -> Either (Named p wA wB) String
extractHash (PIAP _ (Hashed s _)) = Right s
extractHash hp = Left $ conscientiously (\e -> text "unable to read patch:" $$ e) hp

unavailable :: String -> Hopefully a wX wY
unavailable = Hopefully . Unavailable

-- Equality on PatchInfoAnd is solely determined by the PatchInfo
-- It is a global invariant of darcs that once a patch is recorded,
-- it should always have the same representation in the same context.
instance MyEq (PatchInfoAnd p) where
    unsafeCompare (PIAP i _) (PIAP i2 _) = i == i2

instance Invert p => Invert (PatchInfoAnd p) where
    invert (PIAP i p) = PIAP i (invert `fmapH` p)

instance PatchListFormat (PatchInfoAnd p)

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (PatchInfoAnd p) where
    showPatch (PIAP n p) = case hopefully2either p of
                           Right x -> showPatch x
                           Left _ -> showPatchInfoUI n

instance (Apply p, Conflict p, CommuteNoConflicts p, IsHunk p, PatchListFormat p, PrimPatchBase p,
          ShowPatch p, ApplyState p ~ Tree) => ShowPatch (PatchInfoAnd p) where
    showContextPatch (PIAP n p) = case hopefully2either p of
                                    Right x -> showContextPatch x
                                    Left _ -> return $ showPatchInfoUI n
    description (PIAP n _) = showPatchInfoUI n
    summary (PIAP n p) = case hopefully2either p of
                         Right x -> summary x
                         Left _ -> showPatchInfoUI n
    summaryFL = vcat . mapFL summary
    showNicely (PIAP n p) = case hopefully2either p of
                            Right x -> showNicely x
                            Left _ -> showPatchInfoUI n

instance (Commute p, NameHack p) => Commute (PatchInfoAnd p) where
    commute (x :> y) = do y' :> x' <- commute (hopefully x :> hopefully y)
                          return $ (info y `piap` y') :> (info x `piap` x')

instance (Merge p, NameHack p) => Merge (PatchInfoAnd p) where
    merge (x :\/: y) = case merge (hopefully x :\/: hopefully y) of
                       y' :/\: x' -> (info y `piap` y') :/\: (info x `piap` x')

instance PatchInspect p => PatchInspect (PatchInfoAnd p) where
    listTouchedFiles = listTouchedFiles . hopefully
    hunkMatches _ _ = error "hunkmatches not implemented for PatchInfoAnd"

instance Apply p => Apply (PatchInfoAnd p) where
    type ApplyState (PatchInfoAnd p) = ApplyState p
    apply p = apply $ hopefully p

instance RepairToFL p => Repair (PatchInfoAnd p) where
    applyAndTryToFix p = do mp' <- applyAndTryToFix $ hopefully p
                            case mp' of
                              Nothing -> return Nothing
                              Just (e,p') -> return $ Just (e, n2pia p')

instance (ReadPatch p, PatchListFormat p) => ReadPatch (PatchInfoAnd p) where
    readPatch' = mapSeal n2pia <$> readPatch'

instance Effect p => Effect (PatchInfoAnd p) where
    effect = effect . hopefully
    effectRL = effectRL . hopefully

instance IsHunk (PatchInfoAnd p) where
    isHunk _ = Nothing

instance PatchDebug p => PatchDebug (PatchInfoAnd p)

instance (Patchy p, NameHack p, ApplyState p ~ Tree) => Patchy (PatchInfoAnd p)

