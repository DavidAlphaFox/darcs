-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
-- Copyright (C) 2009 Petr Rockai
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


module Darcs.Repository.Merge
    ( tentativelyMergePatches
    , considerMergeToWorking
    ) where

import Control.Monad ( when )
import Storage.Hashed.Tree( Tree )

import Darcs.Repository.External ( backupByCopying )
import Darcs.Repository.Flags
    ( UseIndex
    , ScanKnown
    , AllowConflicts (..)
    , Reorder (..)
    , UpdateWorking (..)
    , ExternalMerge (..)
    , Verbosity (..)
    , Compression (..)
    , WantGuiPause (..)
    , DiffAlgorithm (..)
    )
import Darcs.Patch ( RepoPatch, PrimOf, merge, listTouchedFiles, patchcontents,
                     anonymous, fromPrims, effect )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends( merge2FL )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully )
import Darcs.Patch.Progress( progressFL )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:\/:)(..), (:/\:)(..), (+>+),
    mapFL_FL, concatFL )
import Darcs.Patch.Witnesses.Sealed( Sealed(Sealed), seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd )
import Darcs.Repository.InternalTypes( Repository(..) )
import Darcs.Repository.State( unrecordedChanges, readUnrecorded )
import Darcs.Repository.Resolution ( standardResolution, externalResolution )
import Darcs.Repository.Internal ( announceMergeConflicts,
                                   checkUnrecordedConflicts, MakeChanges(..),
                                   setTentativePending, tentativelyAddPatch_,
                                   applyToTentativePristine,
                                   tentativelyReplacePatches,
                                   UpdatePristine(..) )
import Darcs.Util.Progress( debugMessage )

tentativelyMergePatches_ :: forall p wR wU wT wY wX. (RepoPatch p,
                         ApplyState p ~ Tree) => MakeChanges
                         -> Repository p wR wU wT -> String
                         -> AllowConflicts -> UpdateWorking
                         -> ExternalMerge -> WantGuiPause
                         -> Compression -> Verbosity -> Reorder
                         -> ( UseIndex, ScanKnown, DiffAlgorithm )
                         -> FL (PatchInfoAnd p) wX wT
                         -> FL (PatchInfoAnd p) wX wY
                         -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches_ mc r cmd allowConflicts updateWorking externalMerge wantGuiPause compression verbosity reorder diffingOpts@(_, _, dflag) usi themi = do
    let us = mapFL_FL hopefully usi
        them = mapFL_FL hopefully themi
    ((pc :: FL (PatchInfoAnd p) wT wMerged) :/\: us_merged)
         <- return $ merge2FL (progressFL "Merging us" usi)
                              (progressFL "Merging them" themi)
    pend <- unrecordedChanges diffingOpts r Nothing
    anonpend <- n2pia `fmap` anonymous (fromPrims pend)
    pend' :/\: pw <- return $ merge (pc :\/: anonpend :>: NilFL)
    let pwprim = concatFL $ progressFL "Examining patches for conflicts" $
                                mapFL_FL (patchcontents . hopefully) pw
    Sealed standard_resolved_pw <- return $ standardResolution pwprim
    debugMessage "Checking for conflicts..."
    when (allowConflicts == YesAllowConflictsAndMark) $
        mapM_ backupByCopying $ listTouchedFiles standard_resolved_pw
    debugMessage "Announcing conflicts..."
    have_conflicts <- announceMergeConflicts cmd allowConflicts externalMerge standard_resolved_pw
    debugMessage "Checking for unrecorded conflicts..."
    have_unrecorded_conflicts <- checkUnrecordedConflicts updateWorking $
                                     mapFL_FL hopefully pc
    debugMessage "Reading working directory..."
    working <- readUnrecorded r Nothing
    debugMessage "Working out conflicts in actual working directory..."
    let haveConflicts = have_conflicts || have_unrecorded_conflicts
    Sealed pw_resolution <-
        case (externalMerge , haveConflicts) of
            (NoExternalMerge, _)       -> return $ if allowConflicts == YesAllowConflicts
                                                     then seal NilFL
                                                     else seal standard_resolved_pw
            (_, False)                 -> return $ seal standard_resolved_pw
            (YesExternalMerge c, True) -> externalResolution dflag working c wantGuiPause
                                             (effect us +>+ pend) (effect them) pwprim
    debugMessage "Applying patches to the local directories..."
    when (mc == MakeChanges) $ do
        let doChanges :: FL (PatchInfoAnd p) wX wT -> IO (Repository p wR wU wMerged)
            -- This first case is a possible optimisation: if 'usi' is empty, then
            -- the merge2FL call above will return pc = themi, but the wMerged
            -- witness is quantified in the :/\: constructor so we lose the
            -- information that wX=wT => wMerged=wY so we have to coerce.
            -- TODO: it's not really clear why if this is an optimisation in
            -- practice, as pc would be trivial to calculate in this case and
            -- there aren't any obvious memory benefits.
            doChanges NilFL = applyps r (unsafeCoercePEnd themi)
            doChanges _     = applyps r pc
        r' <- doChanges usi
        setTentativePending r' updateWorking (effect pend' +>+ pw_resolution)
        when (reorder == Reorder) $
            -- TODO: we end up applying the merged remote patches on top of the unmerged
            -- local patches, then commuting out the unmerged local patches and finally
            -- adding the merged local patches.
            -- It would better to just remove the unmerged local patche, then apply the
            -- unmerged remote patches and then the merged local patches.
            -- The handling of 'unrecorded' might complicate this slightly so this
            -- refactoring may be better deferred until we have reliable witness tracking
            -- for repositories.
            tentativelyReplacePatches r' compression YesUpdateWorking verbosity us_merged
    return $ seal (effect pwprim +>+ pw_resolution)
  where
    mapAdd :: Repository p wM wL wI -> FL (PatchInfoAnd p) wI wJ
           -> IO (Repository p wM wL wJ)
    mapAdd repo NilFL = return repo
    mapAdd repo (a:>:as) = do
        repo' <- tentativelyAddPatch_ DontUpdatePristine repo
                     compression verbosity updateWorking a
        mapAdd repo' as
    applyps :: Repository p wM wL wI -> FL (PatchInfoAnd p) wI wJ -> IO (Repository p wM wL wJ)
    applyps repo ps = do
        debugMessage "Adding patches to inventory..."
        repo' <- mapAdd repo ps
        debugMessage "Applying patches to pristine..."
        applyToTentativePristine repo verbosity ps
        return repo'

tentativelyMergePatches :: (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT -> String
                        -> AllowConflicts -> UpdateWorking
                        -> ExternalMerge -> WantGuiPause
                        -> Compression -> Verbosity -> Reorder
                        -> ( UseIndex, ScanKnown, DiffAlgorithm )
                        -> FL (PatchInfoAnd p) wX wT
                        -> FL (PatchInfoAnd p) wX wY
                        -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches = tentativelyMergePatches_ MakeChanges


considerMergeToWorking :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository p wR wU wT -> String
                       -> AllowConflicts -> UpdateWorking
                       -> ExternalMerge -> WantGuiPause
                       -> Compression -> Verbosity -> Reorder
                       -> ( UseIndex, ScanKnown, DiffAlgorithm )
                       -> FL (PatchInfoAnd p) wX wT
                       -> FL (PatchInfoAnd p) wX wY
                       -> IO (Sealed (FL (PrimOf p) wU))
considerMergeToWorking = tentativelyMergePatches_ DontMakeChanges

