{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, OverloadedStrings #-}
module Darcs.Patch.Prim.V3.Show
    ( showHunk )
    where

import Prelude hiding ( pi )

import Data.Char ( isSpace, ord )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..) )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Prim.Class ( PrimShow(..) )
import Darcs.Patch.Prim.V3.Core ( Prim(..), Hunk(..), UUID(..) )
import Darcs.Patch.Prim.V3.Details ()
import Darcs.Util.Printer ( text, packedString, blueText, (<+>), (<>), Doc )

#include "impossible.h"

-- TODO this instance shouldn't really be necessary, as Prims aren't used generically
instance PatchListFormat Prim

instance ShowPatchBasic Prim where
    showPatch = showPrim OldFormat

instance ShowPatch Prim where
    showContextPatch p = return $ showPatch p
    summary = plainSummaryPrim
    summaryFL = plainSummaryPrims
    thing _ = "change"

instance PrimShow Prim where
  showPrim _ (TextHunk u h) = showHunk "hunk" u h
  showPrim _ (BinaryHunk u h) = showHunk "binhunk" u h
  showPrim _ (Manifest f (d,p)) = showManifest "manifest" d f p
  showPrim _ (Demanifest f (d,p)) = showManifest "demanifest" d f p
  showPrim _ Identity = blueText "identity"
  showPrim _ (Move{}) = bug "show for move not implemented"

showManifest :: String -> UUID -> UUID -> BC.ByteString -> Doc
showManifest txt dir file path = blueText txt <+>
                                 formatUUID file <+>
                                 formatUUID dir <+>
                                 packedString (encodeWhite path)

showHunk :: String -> UUID -> Hunk wX wY -> Doc
showHunk txt uid (Hunk off old new) = blueText txt <+>
                                      formatUUID uid <+>
                                      text (show off) <+>
                                      hunktext old <+>
                                      hunktext new
    where hunktext bit | B.null bit = text "!"
                       | otherwise = text "." <> packedString (encodeWhite bit)

formatUUID :: UUID -> Doc
formatUUID (UUID x) = packedString x

-- XXX a bytestring version of encodeWhite from Darcs.FileName
encodeWhite :: B.ByteString -> B.ByteString
encodeWhite = BC.concatMap encode
  where encode c
          | isSpace c || c == '\\' = B.concat [ "\\", BC.pack $ show $ ord c, "\\" ]
          | otherwise = BC.singleton c

