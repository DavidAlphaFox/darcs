-- Copyright (C) 2002-2004 David Roundy
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


module Darcs.Patch.TouchesFiles ( lookTouch, chooseTouching, choosePreTouching,
                      selectTouching,
                      deselectNotTouching, selectNotTouching,
                    ) where
import Control.Applicative ( (<$>) )
import Data.List ( isSuffixOf, nub )

import Darcs.Patch.Choices ( PatchChoices, Label, LabelledPatch,
                             patchChoices, label, getChoices,
                      forceFirsts, forceLasts, lpPatch,
                    )
import Darcs.Patch ( Patchy, invert )
import Darcs.Patch.Apply ( ApplyState, applyToFilePaths, effectOnFilePaths )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), mapFL_FL, (+>+) )
import Darcs.Patch.Witnesses.Sealed ( Sealed, seal )
import Storage.Hashed.Tree( Tree )

labelTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => Bool
            -> [FilePath] -> FL (LabelledPatch p) wX wY -> [Label]
labelTouching _ _ NilFL = []
labelTouching wantTouching fs (lp :>: lps) =
    case lookTouchOnlyEffect fs (lpPatch lp) of
        (doesTouch, fs') ->
            let rest = labelTouching wantTouching fs' lps
            in (if doesTouch == wantTouching then (label lp :) else id) rest

labelNotTouchingFM :: (PatchInspect p, Patchy p, ApplyState p ~ Tree)
                 => [FilePath] -> PatchChoices p wX wY -> [Label]
labelNotTouchingFM files pc = case getChoices pc of
    fc :> mc :> _ -> labelTouching False (map fix files) (fc +>+ mc)

selectTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
               => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectTouching Nothing pc = pc
selectTouching (Just files) pc = forceFirsts xs pc
    where xs = case getChoices pc of
               _ :> mc :> lc -> labelTouching True (map fix files) (mc +>+ lc)

deselectNotTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                    => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
deselectNotTouching Nothing pc = pc
deselectNotTouching (Just files) pc = forceLasts (labelNotTouchingFM files pc) pc

selectNotTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                  => Maybe [FilePath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectNotTouching Nothing pc = pc
selectNotTouching (Just files) pc = forceFirsts (labelNotTouchingFM files pc) pc

fix :: FilePath -> FilePath
fix f | "/" `isSuffixOf` f = fix $ init f
fix "" = "."
fix "." = "."
fix f = "./" ++ f

chooseTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
               => Maybe [FilePath] -> FL p wX wY -> Sealed (FL p wX)
chooseTouching Nothing p = seal p
chooseTouching files p = case getChoices $ selectTouching files $ patchChoices p of
                          fc :> _ :> _ -> seal $ mapFL_FL lpPatch fc

choosePreTouching :: (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                  => Maybe [FilePath] -> FL p wX wY -> Sealed (FL p wX)
choosePreTouching files patch = chooseTouching filesBeforePatch patch where
    filesBeforePatch = effectOnFilePaths (invert patch) <$> files

lookTouchOnlyEffect :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => [FilePath] -> p wX wY
    -> (Bool, [FilePath])
lookTouchOnlyEffect fs p = (wasTouched, fs') where
    (wasTouched, _, fs', _) = lookTouch Nothing fs p


lookTouch :: (Patchy p, PatchInspect p, ApplyState p ~ Tree) => Maybe [(FilePath, FilePath)]
    -> [FilePath] -> p wX wY
    -> (Bool, [FilePath], [FilePath], [(FilePath, FilePath)])
lookTouch renames fs p = (anyTouched, touchedFs, fs', renames')
    where
          touchedFs = nub . concatMap fsAffectedBy $ affected
          fsAffectedBy af = filter (affectedBy af) fs
          anyTouched = length touchedFs > 0
          affectedBy :: FilePath -> FilePath -> Bool
          touched `affectedBy` f =  touched == f
                                 || touched `isSubPathOf` f
                                 || f `isSubPathOf` touched
          isSubPathOf :: FilePath -> FilePath -> Bool
          path `isSubPathOf` parent = case splitAt (length parent) path of
                                 (path', '/':_) -> path' == parent
                                 _ -> False
          (affected, fs', renames') = applyToFilePaths p renames fs
