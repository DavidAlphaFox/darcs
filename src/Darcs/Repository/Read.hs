module Darcs.Repository.Read ( readRepo ) where

import Darcs.Patch (RepoPatch)
import Darcs.Patch.Apply ( ApplyState )
import Storage.Hashed.Tree ( Tree )
import Darcs.Repository.InternalTypes ( Repository(Repo) )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Repository.Format ( formatHas, RepoProperty(HashedInventory) )
import qualified Darcs.Repository.HashedRepo as HashedRepo ( readRepo )
import qualified Darcs.Repository.Old as Old ( readOldRepo )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

 
-- @todo: we should not have to open the result of HashedRepo and
-- seal it.  Instead, update this function to work with type witnesses
-- by fixing DarcsRepo to match HashedRepo in the handling of
-- Repository state.
readRepo :: (RepoPatch p, ApplyState p ~ Tree)
         => Repository p wR wU wT
         -> IO (PatchSet p Origin wR)
readRepo repo@(Repo r rf _ _)
    | formatHas HashedInventory rf = HashedRepo.readRepo repo r
    | otherwise = do Sealed ps <- Old.readOldRepo r
                     return $ unsafeCoerceP ps
