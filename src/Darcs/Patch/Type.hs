module Darcs.Patch.Type ( PatchType(..), patchType ) where

-- |Used for indicating a patch type without having a concrete patch
data PatchType (p :: * -> * -> *) = PatchType

patchType :: p wX wY -> PatchType p
patchType _ = PatchType
