{-# LANGUAGE CPP, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module Darcs.Patch.Prim.V3.Apply ( ObjectMap(..) ) where

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..), ApplyMonadTrans(..), ToTree(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )

import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.V3.Core ( Prim(..), hunkEdit )
import Darcs.Patch.Prim.V3.ObjectMap

import Control.Monad.State( StateT, runStateT, gets, lift, put )
import qualified Data.Map as M

-- import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )

import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Storage.Hashed.Hash( Hash(..) )

#include "impossible.h"

instance Apply Prim where
    type ApplyState Prim = ObjectMap
    apply (Manifest i (dirid, name)) = editDirectory dirid (M.insert name i)
    apply (Demanifest _ (dirid, name)) = editDirectory dirid (M.delete name)
    apply (TextHunk i hunk) = editFile i (hunkEdit hunk)
    apply (BinaryHunk i hunk) = editFile i (hunkEdit hunk)
    apply Identity = return ()
    apply (Move{}) = bug "apply for move not implemented"

instance RepairToFL Prim where
    applyAndTryToFixFL p = apply p >> return Nothing

instance PrimApply Prim where
    applyPrimFL NilFL = return ()
    applyPrimFL (p:>:ps) = apply p >> applyPrimFL ps

instance ToTree ObjectMap -- TODO

editObject :: (Monad m) => UUID -> (Maybe (Object m) -> Object m) -> (StateT (ObjectMap m) m) ()
editObject i edit = do load <- gets getObject
                       store <- gets putObject
                       obj <- lift $ load i
                       new <- lift $ store i $ edit obj
                       put new
                       return ()

instance (Functor m, Monad m) => ApplyMonad (StateT (ObjectMap m) m) ObjectMap where
    type ApplyMonadBase (StateT (ObjectMap m) m) = m
    editFile i edit = editObject i edit'
      where edit' (Just (Blob x _)) = Blob (edit `fmap` x) NoHash
            edit' (Just (Directory x)) = Directory x -- error?
            edit' Nothing = Blob (return $ edit "") NoHash
    editDirectory i edit = editObject i edit'
      where edit' (Just (Directory x)) = Directory $ edit x
            edit' (Just (Blob x y)) = Blob x y -- error?
            edit' Nothing = Directory $ edit M.empty

instance (Functor m, Monad m) => ApplyMonadTrans m ObjectMap where
  type ApplyMonadOver m ObjectMap = StateT (ObjectMap m) m
  runApplyMonad = runStateT

