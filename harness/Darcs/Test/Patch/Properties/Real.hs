{-# LANGUAGE CPP #-}
module Darcs.Test.Patch.Properties.Real
       ( propConsistentTreeFlattenings ) where

import Darcs.Test.Patch.Arbitrary.Generic ( Tree, flattenTree, G2(..), mapTree )
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel ( RepoModel, repoApply, showModel, eqModel, RepoState
                                  , Fail, maybeFail )
import qualified Storage.Hashed.Tree as HST ( Tree )

import Darcs.Patch.Witnesses.Sealed( Sealed(..) )
import Darcs.Patch.V2.Real( prim2real )
import Darcs.Patch.Prim.V1 ( Prim )

#include "impossible.h"

assertEqualFst :: (RepoModel a, Show b, Show c) => (Fail (a x), b) -> (Fail (a x), c) -> Bool
assertEqualFst (x,bx) (y,by)
    | Just x' <- maybeFail x, Just y' <- maybeFail y, x' `eqModel` y' = True
    | Nothing <- maybeFail x, Nothing <- maybeFail y = True
    | otherwise = error ("Not really equal:\n" ++ showx ++ "\nand\n" ++ showy
                         ++ "\ncoming from\n" ++ show bx ++ "\nand\n" ++ show by)
      where showx | Just x' <- maybeFail x = showModel x'
                  | otherwise = "Nothing"
            showy | Just y' <- maybeFail y = showModel y'
                  | otherwise = "Nothing"

propConsistentTreeFlattenings :: (RepoState model ~ HST.Tree, RepoModel model)
                              => Sealed (WithStartState model (Tree Prim)) -> Bool
propConsistentTreeFlattenings (Sealed (WithStartState start t))
  = fromJust $
    do Sealed (G2 flat) <- return $ flattenTree $ mapTree prim2real t
       rms <- return $ map (start `repoApply`) flat
       return $ and $ zipWith assertEqualFst (zip rms flat) (tail $ zip rms flat)

