-- Copyright (C) 2002-2004,2007 David Roundy
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

module Darcs.Patch.Bundle
    ( hashBundle
    , makeBundle2
    , makeBundleN
    , scanBundle
    , contextPatches
    , scanContextFile
    , patchFilename
    , getContext
    , minContext
    , parseBundle
    ) where

import Data.Char ( isAlpha, toLower, isDigit, isSpace )
import qualified Data.ByteString as B ( ByteString, length, null, drop,
                                        isPrefixOf )
import qualified Data.ByteString.Char8 as BC ( unpack, break, pack )

import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeIO )

import Darcs.Patch ( RepoPatch, Named, showPatch, showContextPatch,
                     readPatchPartial )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Bracketed ( Bracketed, unBracketedFL )
import Darcs.Patch.Bracketed.Instances ()
import Darcs.Patch.Commute( commute )
import Darcs.Patch.Depends ( slightlyOptimizePatchset )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo,
                          showPatchInfoUI, isTag )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, piap, fmapFLPIAP, info,
                                  patchInfoAndPatch, unavailable, hopefully )
import Darcs.Patch.ReadMonads ( parseStrictly )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..), FL(..), (:>)(..), reverseFL, (+<+),
    mapFL, mapFL_FL, mapRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.ByteString ( mmapFilePS, linesPS, unlinesPS, dropSpace, substrPS)
import Darcs.Util.Crypt.SHA1 ( sha1PS )
import Darcs.Util.Printer ( Doc, renderPS, newline, text, ($$),
                 (<>), vcat, vsep, renderString, RenderMode(..) )

-- |hashBundle creates a SHA1 string of a given a FL of named patches. This
-- allows us to ensure that the patches in a received patchBundle have not been
-- modified in transit.
hashBundle :: (PatchListFormat p, ShowPatchBasic p) => FL (Named p) wX wY
           -> String
hashBundle to_be_sent =
    show $ sha1PS $ renderPS Standard $ vcat (mapFL showPatch to_be_sent) <> newline

makeBundleN :: (ApplyState p ~ Tree, RepoPatch p) => Maybe (Tree IO)
            -> PatchSet p wStart wX -> FL (Named p) wX wY -> IO Doc
makeBundleN the_s (PatchSet ps (Tagged t _ _ :<: _)) to_be_sent =
    makeBundle2 the_s (ps +<+ (t :<: NilRL)) to_be_sent to_be_sent
makeBundleN the_s (PatchSet ps NilRL) to_be_sent =
    makeBundle2 the_s ps to_be_sent to_be_sent

-- | In makeBundle2, it is presumed that the two patch sequences are
-- identical, but that they may be lazily generated.  If two different
-- patch sequences are passed, a bundle with a mismatched hash will be
-- generated, which is not the end of the world, but isn't very useful
-- either.
makeBundle2 :: (ApplyState p ~ Tree, RepoPatch p) => Maybe (Tree IO)
            -> RL (PatchInfoAnd p) wStart wX -> FL (Named p) wX wY
            -> FL (Named p) wX wY -> IO Doc
makeBundle2 the_s common' to_be_sent to_be_sent2 = do
    patches <- case the_s of
                   Just tree -> fst `fmap` virtualTreeIO (showContextPatch to_be_sent) tree
                   Nothing -> return (vsep $ mapFL showPatch to_be_sent)
    return $ format patches
  where
    format the_new = text ""
                     $$ text "New patches:"
                     $$ text ""
                     $$ the_new
                     $$ text ""
                     $$ text "Context:"
                     $$ text ""
                     $$ vcat (map showPatchInfo common)
                     $$ text "Patch bundle hash:"
                     $$ text (hashBundle to_be_sent2)
                     $$ text ""
    common = mapRL info common'

parseBundle :: forall p. RepoPatch p => B.ByteString
            -> Either String
                      (Sealed ((PatchSet p :> FL (PatchInfoAnd p)) Origin))
parseBundle input | B.null input = Left "Bad patch bundle!"
parseBundle input = case sillyLex input of
    ("New patches:", rest) -> case getPatches rest of
        (Sealed bracketedPatches, rest') -> case sillyLex rest' of
            ("Context:", rest'') -> case getContext rest'' of
                (cont, maybe_hash) ->
                    let sealedCtxAndPs = sealCtxAndPs cont bracketedPatches in
                    case substrPS (BC.pack "Patch bundle hash:") maybe_hash of
                        Just n ->
                            let hPs = mapFL_FL hopefully bracketedPatches
                                realHash = hashBundle hPs
                                getHash = fst . sillyLex . snd . sillyLex
                                bundleHash = getHash $ B.drop n maybe_hash in
                            if realHash == bundleHash
                                then sealedCtxAndPs
                                else Left hashFailureMessage
                        Nothing -> sealedCtxAndPs
            (a, r) -> Left $ "Malformed patch bundle: '" ++ a
                             ++ "' is not 'Context:'\n" ++ BC.unpack r
    ("Context:", rest) -> case getContext rest of
        (cont, rest') -> case sillyLex rest' of
            ("New patches:", rest'') -> case getPatches rest'' of
                (Sealed bracketedPatches, _) ->
                    Right $ sealContextWithPatches cont bracketedPatches
            (a, _) -> Left $ "Malformed patch bundle: '" ++ a
                             ++ "' is not 'New patches:'"
    ("-----BEGIN PGP SIGNED MESSAGE-----",rest) ->
        parseBundle $ filterGpgDashes rest
    (_, rest) -> parseBundle rest
  where
    hashFailureMessage = "Patch bundle failed hash!\n"
                         ++ "This probably means that the patch has been "
                         ++ "corrupted by a mailer.\n"
                         ++ "The most likely culprit is CRLF newlines."

    sealCtxAndPs ctx ps = Right $ sealContextWithPatches ctx ps

    sealContextWithPatches :: RepoPatch p => [PatchInfo]
                           -> FL (PatchInfoAnd (Bracketed p)) wX wY
                           -> Sealed
                                  ((PatchSet p :> FL (PatchInfoAnd p)) Origin)
    sealContextWithPatches context bracketedPatches =
        let patches = mapFL_FL (fmapFLPIAP unBracketedFL) bracketedPatches in
        case reverse context of
            (x : ry) | isTag x ->
                  let ps = unavailablePatches (reverse ry)
                      t = Tagged (piUnavailable x) Nothing NilRL in
                  Sealed $ PatchSet ps (t :<: NilRL) :> patches
            _ -> let ps = PatchSet (unavailablePatches context) NilRL in
                 Sealed $ ps :> patches
                 -- The above NilRLs aren't quite right, because ther *are*
                 -- earlier patches, but we can't set this to undefined
                 -- because there are situations where we look at the rest.
                 -- :{

scanBundle :: forall p . RepoPatch p => B.ByteString
           -> Either String (SealedPatchSet p Origin)
scanBundle bundle = do
  Sealed (PatchSet recent tagged :> ps) <- parseBundle bundle
  return . Sealed $ PatchSet (reverseFL ps +<+ recent) tagged

-- |filterGpgDashes unescapes a clearsigned patch, which will have had any
-- lines starting with dashes escaped with a leading "- ".
filterGpgDashes :: B.ByteString -> B.ByteString
filterGpgDashes ps =
    unlinesPS $ map drop_dashes $
    takeWhile (/= BC.pack "-----END PGP SIGNED MESSAGE-----") $
    dropWhile not_context_or_newpatches $ linesPS ps
  where
    drop_dashes x
        | B.length x < 2 = x
        | BC.pack "- " `B.isPrefixOf` x = B.drop 2 x
        | otherwise = x

    not_context_or_newpatches s = (s /= BC.pack "Context:") &&
                                  (s /= BC.pack "New patches:")

-- |unavailablePatches converts a list of PatchInfos into a RL of PatchInfoAnd
-- Unavailable patches. This is used to represent the Context of a patchBundle.
unavailablePatches :: RepoPatch p => [PatchInfo] -> RL (PatchInfoAnd p) wX wY
unavailablePatches = foldr ((:<:) . piUnavailable) (unsafeCoerceP NilRL)

-- |piUnavailable returns an Unavailable within a PatchInfoAnd given a
-- PatchInfo.
piUnavailable :: RepoPatch p => PatchInfo -> PatchInfoAnd p wX wY
piUnavailable i = patchInfoAndPatch i . unavailable $
    "Patch not stored in patch bundle:\n" ++ renderString Encode (showPatchInfoUI i)

-- |getContext parses a context list, returning a tuple containing the list,
-- and remaining ByteString input.
getContext :: B.ByteString -> ([PatchInfo],B.ByteString)
getContext ps = case parseStrictly readPatchInfo ps of
    Just (pinfo, r') -> case getContext r' of
        (pis, r'') -> (pinfo : pis, r'')
    Nothing -> ([], ps)

-- |(-:-) is used to build up a Sealed FL of patches and tuple it, along with
-- any unconsumed input.
(-:-) :: a wX wY -> (Sealed (FL a wY), b) -> (Sealed (FL a wX), b)
p -:- (Sealed ps, r) = (Sealed (p :>: ps), r)

-- |getPatches attempts to parse a sequence of patches from a ByteString,
-- returning the FL of as many patches-with-info as were successfully parsed,
-- along with any unconsumed input.
getPatches :: RepoPatch p => B.ByteString
           -> (Sealed (FL (PatchInfoAnd (Bracketed p)) wX), B.ByteString)
getPatches ps = case parseStrictly readPatchInfo ps of
    Nothing -> (Sealed NilFL, ps)
    Just (pinfo, _) -> case readPatchPartial ps of
        Nothing -> (Sealed NilFL, ps)
        Just (Sealed p, r) -> (pinfo `piap` p) -:- getPatches r

-- |sillyLex takes a ByteString and breaks it upon the first newline, having
-- removed any leading spaces. The before-newline part is unpacked to a String,
-- and tupled up with the remaining ByteString.
sillyLex :: B.ByteString -> (String, B.ByteString)
sillyLex ps = (BC.unpack a, b)
  where
    (a, b) = BC.break (== '\n') (dropSpace ps)

contextPatches :: RepoPatch p => PatchSet p Origin wX
               -> (PatchSet p :> RL (PatchInfoAnd p)) Origin wX
contextPatches set = case slightlyOptimizePatchset set of
    PatchSet ps (Tagged t _ ps' :<: ts) ->
        PatchSet ps' ts :> (ps +<+ (t :<: NilRL))
    PatchSet ps NilRL -> PatchSet NilRL NilRL :> ps

-- |'scanContextFile' scans the context in the file of the given name.
scanContextFile :: RepoPatch p => FilePath -> IO (PatchSet p Origin wX)
scanContextFile filename = scanContext `fmap` mmapFilePS filename
  where
    -- are the type witnesses sensible?
    scanContext :: RepoPatch p => B.ByteString -> PatchSet p Origin wX
    scanContext input
        | B.null input = error "Bad context!"
        | otherwise = case sillyLex input of
            ("Context:",rest) -> case getContext rest of
                (cont@(_ : _), _) | isTag (last cont) ->
                    let ps = unavailablePatches $ init cont
                        t = Tagged (piUnavailable $ last cont) Nothing NilRL in
                    PatchSet ps (t :<: NilRL)
                (cont, _) -> PatchSet (unavailablePatches cont) NilRL
            ("-----BEGIN PGP SIGNED MESSAGE-----",rest) ->
                scanContext $ filterGpgDashes rest
            (_, rest) -> scanContext rest

-- | Minimize the context of a bundle to be sent, taking into account
--   the patches selected to be sent 
minContext :: (RepoPatch p)
          => PatchSet p wStart wB
          -> FL (PatchInfoAnd p) wB wC
          -> Sealed ((PatchSet p :> FL (PatchInfoAnd p)) wStart)
minContext (PatchSet topCommon behindTag) to_be_sent =
  case go topCommon NilFL to_be_sent of
    Sealed (c :> to_be_sent') -> seal (PatchSet c behindTag :> to_be_sent') 
  where
    go :: (RepoPatch p)
       => RL (PatchInfoAnd p) wA wB -- context we attempt to minimize
       -> FL (PatchInfoAnd p) wB wC -- patches we cannot remove from context
       -> FL (PatchInfoAnd p) wC wD -- patches to be included in the bundle
       -> Sealed (( RL (PatchInfoAnd p) :> FL (PatchInfoAnd p) ) wA )
    go NilRL necessary to_be_sent' = seal (reverseFL necessary :> to_be_sent')
    go (candidate :<: rest) necessary to_be_sent' =
      let fl1 = (candidate :>: NilFL) in
      case commute (fl1 :> necessary) of
        Nothing                   -> go rest (candidate :>: necessary) to_be_sent'
        Just (necessary' :> fl1') ->
            case commute (fl1' :> to_be_sent') of
                Nothing                  -> go rest (candidate :>: necessary) to_be_sent'
                Just (to_be_sent'' :> _) -> -- commutation work, we can drop the patch
                  go rest necessary' to_be_sent''

-- |patchFilename maps a patch description string to a safe (lowercased, spaces
-- removed and ascii-only characters) patch filename.
patchFilename :: String -> String
patchFilename the_summary = name ++ ".dpatch"
  where
    name = map safeFileChar the_summary
    safeFileChar c | isAlpha c = toLower c
                   | isDigit c = c
                   | isSpace c = '-'
    safeFileChar _ = '_'
