--  Copyright (C) 2002-2003 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, UndecidableInstances #-} -- XXX Undecidable only in GHC < 7


module Darcs.Patch
    ( RepoPatch
    , PrimOf
    , Named
    , Patchy
    , fromPrim
    , fromPrims
    , rmfile
    , addfile
    , rmdir
    , adddir
    , move
    , hunk
    , tokreplace
    , namepatch
    , anonymous
    , binary
    , description
    , showContextPatch
    , showPatch
    , showNicely
    , infopatch
    , changepref
    , thing
    , things
    , primIsAddfile
    , primIsHunk
    , primIsSetpref
    , merge
    , commute
    , listTouchedFiles
    , hunkMatches
    , forceTokReplace
    , PrimPatch
      -- * for PatchTest
    , resolveConflicts
    , Effect
    , effect
    , primIsBinary
    , primIsAdddir
    , invert
    , invertFL
    , invertRL
    , commuteFLorComplain
    , commuteRL
    , readPatch
    , readPatchPartial
    , canonize
    , sortCoalesceFL
    , tryToShrink
    , patchname
    , patchcontents
    , applyToFilePaths
    , apply
    , applyToTree
    , effectOnFilePaths
    , patch2patchinfo
    , summary
    , summaryFL
    , plainSummary
    , xmlSummary
    , plainSummaryPrims
    , adddeps
    , getdeps
    , listConflictedFiles
    , isInconsistent
    ) where


import Darcs.Patch.Apply ( applyToFilePaths, effectOnFilePaths, applyToTree,
                           ApplyState )
import Darcs.Patch.Commute ( commuteFLorComplain, commuteRL )
import Darcs.Patch.Conflict ( listConflictedFiles, resolveConflicts )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.Invert ( invertRL, invertFL )
import Darcs.Patch.Named ( Named,
                           adddeps, namepatch,
                           anonymous,
                           getdeps,
                           infopatch,
                           patch2patchinfo, patchname, patchcontents )
import Darcs.Patch.Patchy ( Patchy,
                            showPatch, showNicely, showContextPatch,
                            invert,
                            thing, things,
                            apply,
                            description, summary, summaryFL,
                            commute, listTouchedFiles, hunkMatches
                          )
import Darcs.Patch.Prim ( fromPrims, fromPrim,
                          canonize,
                          sortCoalesceFL,
                          rmdir, rmfile, tokreplace, adddir, addfile,
                          binary, changepref, hunk, move,
                          primIsAdddir, primIsAddfile,
                          primIsHunk, primIsBinary, primIsSetpref,
                          tryToShrink,
                          PrimPatch, PrimPatchBase(..) )
import Darcs.Patch.Read ( readPatch, readPatchPartial )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Repair ( isInconsistent )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Summary ( xmlSummary, plainSummary, plainSummaryPrims )
import Darcs.Patch.TokenReplace ( forceTokReplace )
import Darcs.Patch.V1.Commute ( merge )

import Storage.Hashed.Tree( Tree )


instance (Patchy p, NameHack p, ApplyState p ~ Tree) => Patchy (Named p)
