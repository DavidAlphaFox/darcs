--  Copyright (C) 2002-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}

module Darcs.Patch.Show
     ( ShowPatchBasic(..)
     , ShowPatch(..)
     , showNamedPrefix
     , formatFileName
     ) where

import Prelude hiding ( pi )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.ApplyMonad ( ApplyMonadTrans, ApplyMonad )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo )
import Darcs.Patch.Witnesses.Ordered ( FL )

import Darcs.Util.English ( plural, Noun(Noun) )
import Darcs.Util.Path ( FileName, fn2ps, encodeWhite, fn2fp )
import Darcs.Util.Printer ( Doc, vcat, blueText, ($$), text, packedString )

showNamedPrefix :: PatchInfo -> [PatchInfo] -> Doc
showNamedPrefix n d = showPatchInfo n
                      $$ blueText "<"
                      $$ vcat (map showPatchInfo d)
                      $$ blueText ">"

class ShowPatchBasic p where
    showPatch :: p wX wY -> Doc

class ShowPatchBasic p => ShowPatch p where
    showNicely :: p wX wY -> Doc
    showNicely = showPatch

    -- | showContextPatch is used to add context to a patch, as diff
    -- -u does. Thus, it differs from showPatch only for hunks. It is
    -- used for instance before putting it into a bundle. As this
    -- unified context is not included in patch representation, this
    -- requires access to the tree.
    showContextPatch :: (Monad m, ApplyMonadTrans m (ApplyState p),
                         ApplyMonad m (ApplyState p))
                     => p wX wY -> m Doc
    showContextPatch p = return $ showPatch p

    description :: p wX wY -> Doc
    description = showPatch

    summary :: p wX wY -> Doc

    summaryFL :: FL p wX wY -> Doc

    thing :: p wX wY -> String
    thing _ = "patch"

    things :: p wX wY -> String
    things x = plural (Noun $ thing x) ""

formatFileName :: FileNameFormat -> FileName -> Doc
formatFileName OldFormat = packedString . fn2ps
formatFileName NewFormat = text . encodeWhite . fn2fp
