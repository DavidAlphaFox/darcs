module Darcs.Patch.RepoPatch ( RepoPatch )
    where

import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Patchy.Instances ()
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf, FromPrim )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Rebase.NameHack ( NameHack )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase )
import Darcs.Patch.Repair ( RepairToFL, Check )
import Darcs.Patch.Show ( ShowPatch )

class (Patchy p, Merge p, Effect p, IsHunk p,
       PatchInspect p, ReadPatch p, ShowPatch p,
       FromPrim p, Conflict p, CommuteNoConflicts p,
       Check p, RepairToFL p, PatchListFormat p,
       PrimPatchBase p, Patchy (PrimOf p), IsHunk (PrimOf p),
       MaybeInternal p, RecontextRebase p, NameHack p,
       Matchable p
      )
    => RepoPatch p
