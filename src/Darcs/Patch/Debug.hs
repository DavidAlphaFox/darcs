module Darcs.Patch.Debug ( PatchDebug(..) )where

import Darcs.Patch.Witnesses.Ordered ( FL, RL )

-- | PatchDebug is a hook class for temporarily adding debug information.
-- To use it, add any methods that are required, implement those methods
-- where needed, and then make it available in the relevant contexts.
-- For example it can be temporarily added as a superclass of `Patchy`.
-- The advantage of having it here already is that everything is
-- (or should be) declared as an instance of it, so you can use
-- defaulting or just leave out declarations of instance methods and
-- code will still compile.
class PatchDebug p where
   -- | A dummy method so we can export/import PatchDebug(..) without
   -- triggering warnings
   patchDebugDummy :: p wX wY -> ()
   patchDebugDummy _ = ()

instance PatchDebug p => PatchDebug (FL p)
instance PatchDebug p => PatchDebug (RL p)
