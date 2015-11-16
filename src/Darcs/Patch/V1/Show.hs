{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Show ( showPatch_ ) where

import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Prim ( showPrim, PrimPatch )

import Darcs.Patch.V1.Core ( Patch(..) )

import Darcs.Util.Printer ( Doc,
                 text, blueText,
                 ($$), (<+>) )

showPatch_ :: PrimPatch prim => Patch prim wA wB -> Doc
showPatch_ (PP p) = showPrim OldFormat p
showPatch_ (Merger _ _ p1 p2) = showMerger "merger" p1 p2
showPatch_ (Regrem _ _ p1 p2) = showMerger "regrem" p1 p2

showMerger :: PrimPatch prim => String -> Patch prim wA wB -> Patch prim wD wE -> Doc
showMerger merger_name p1 p2 =
    blueText merger_name <+> text "0.0" <+> blueText "("
                           $$ showPatch_ p1
                           $$ showPatch_ p2
                           $$ blueText ")"
