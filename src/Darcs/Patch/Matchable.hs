--  Copyright (C) 2013 Ganesh Sittampalam
--
--  BSD3

module Darcs.Patch.Matchable ( Matchable ) where

import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Rebase.NameHack ( NameHack )

class (Patchy p, PatchInspect p, MaybeInternal p, NameHack p)
    => Matchable p
