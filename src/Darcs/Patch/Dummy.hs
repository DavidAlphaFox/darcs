{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE EmptyDataDecls #-}
module Darcs.Patch.Dummy ( DummyPatch ) where

import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Patch.Patchy
    ( Patchy, ShowPatch, Invert, Commute, Apply(..), PatchInspect
    , ReadPatch )
import Darcs.Patch.Prim ( FromPrim, PrimPatch, PrimPatchBase(..) )
import Darcs.Patch.Prim.Class
        ( PrimConstruct, PrimCanonize, PrimClassify
        , PrimDetails, PrimShow, PrimRead, PrimApply )
import Darcs.Patch.Merge ( Merge)
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase )
import Darcs.Patch.Repair ( Check, RepairToFL )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Witnesses.Eq ( MyEq )
import Storage.Hashed.Tree( Tree )


data DummyPrim wX wY
data DummyPatch wX wY

instance IsHunk DummyPrim
instance PatchListFormat DummyPrim
instance MyEq DummyPrim
instance Invert DummyPrim
instance PatchInspect DummyPrim
instance ReadPatch DummyPrim
instance ShowPatchBasic DummyPrim
instance ShowPatch DummyPrim
instance Commute DummyPrim
instance Apply DummyPrim
instance Patchy DummyPrim

instance RepairToFL DummyPrim
instance PrimConstruct DummyPrim
instance PrimCanonize DummyPrim
instance PrimClassify DummyPrim
instance PrimDetails DummyPrim
instance PrimShow DummyPrim
instance PrimRead DummyPrim
instance PrimApply DummyPrim
instance PrimPatch DummyPrim

instance PatchDebug DummyPrim

instance IsHunk DummyPatch
instance PatchListFormat DummyPatch
instance MyEq DummyPatch
instance Invert DummyPatch
instance PatchInspect DummyPatch
instance ReadPatch DummyPatch
instance ShowPatchBasic DummyPatch
instance ShowPatch DummyPatch
instance Commute DummyPatch
instance Apply DummyPatch where
  type ApplyState DummyPatch = Tree
instance Matchable DummyPatch
instance Patchy DummyPatch

instance Effect DummyPatch
instance Merge DummyPatch
instance Conflict DummyPatch
instance FromPrim DummyPatch
instance CommuteNoConflicts DummyPatch
instance Check DummyPatch
instance RepairToFL DummyPatch
instance PrimPatchBase DummyPatch where
   type PrimOf DummyPatch = DummyPrim
instance MaybeInternal DummyPatch
instance NameHack DummyPatch
instance RecontextRebase DummyPatch
instance RepoPatch DummyPatch

instance PatchDebug DummyPatch
