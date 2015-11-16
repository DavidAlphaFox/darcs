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

{-# LANGUAGE CPP #-}

module Darcs.UI.PrintPatch
    ( printPatch
    , contextualPrintPatch
    , printPatchPager
    , printFriendly
    , showFriendly
    ) where

import Storage.Hashed.Monad( virtualTreeIO )
import Storage.Hashed.Tree( Tree )

import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch ( showContextPatch, showPatch, showNicely, description,
                     summary )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.UI.External ( viewDocWith )
--import Darcs.UI.Flags ( DarcsFlag(Summary, Verbose), isUnified )
import Darcs.UI.Options.All ( Verbosity(..), Summary(..), WithContext(..) )

import Darcs.Util.Printer ( Doc, putDocLnWith, RenderMode(..) )

-- | @'printFriendly' opts patch@ prints @patch@ in accordance with the flags
-- in opts, ie, whether @--verbose@ or @--summary@ were passed at the
-- command-line.
printFriendly :: (ShowPatch p, ApplyState p ~ Tree) => Maybe (Tree IO)
              -> Verbosity -> Summary -> WithContext -> p wX wY -> IO ()
printFriendly (Just pristine) _ _ YesContext = contextualPrintPatch pristine
printFriendly _ v s _ = putDocLnWith fancyPrinters . showFriendly v s

-- | @'showFriendly' flags patch@ returns a 'Doc' representing the right
-- way to show @patch@ given the list @flags@ of flags darcs was invoked with.
showFriendly :: ShowPatch p => Verbosity -> Summary -> p wX wY -> Doc
showFriendly Verbose _          = showNicely
showFriendly _       YesSummary = summary
showFriendly _       NoSummary  = description

-- | 'printPatch' prints a patch on standard output.
printPatch :: ShowPatch p => p wX wY -> IO ()
printPatch p = putDocLnWith fancyPrinters $ showPatch p

-- | 'printPatchPager' runs '$PAGER' and shows a patch in it.
printPatchPager :: ShowPatch p => p wX wY -> IO ()
printPatchPager p = viewDocWith fancyPrinters Standard $ showPatch p

-- | 'contextualPrintPatch' prints a patch, together with its context, on
-- standard output.
contextualPrintPatch :: (ShowPatch p, ApplyState p ~ Tree) => Tree IO
                     -> p wX wY -> IO ()
contextualPrintPatch s p = do
    (contextedPatch, _) <- virtualTreeIO (showContextPatch p) s
    putDocLnWith fancyPrinters contextedPatch
