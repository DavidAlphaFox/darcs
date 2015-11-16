{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
-- Copyright (C) 2010, 2011 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
module Darcs.Patch.ApplyMonad( ApplyMonad(..), ApplyMonadTrans(..), withFileNames, withFiles, ToTree(..) ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Storage.Hashed.Monad as HSM
import Data.Maybe ( fromMaybe )
import Storage.Hashed.Tree ( Tree )
import Darcs.Util.ByteString( linesPS, unlinesPS )
import Darcs.Util.Path ( FileName, movedirfilename, fn2fp, isParentOrEqOf,
                    floatPath, AnchoredPath )
import Control.Monad.State.Strict
import Control.Monad.Identity( Identity )
import Darcs.Patch.MonadProgress

-- TODO should UUID/Object live somewhere more central?
import Darcs.Patch.Prim.V3.ObjectMap ( UUID, ObjectMap, DirContent )

fn2ap :: FileName -> AnchoredPath
fn2ap = floatPath . fn2fp

class ToTree s where
  toTree :: s m -> Tree m

instance ToTree Tree where
  toTree = id

class (Functor m, Monad m, ApplyMonad (ApplyMonadOver m state) state)
      => ApplyMonadTrans m (state :: (* -> *) -> *) where
  type ApplyMonadOver m state :: * -> *
  runApplyMonad :: (ApplyMonadOver m state) x -> state m -> m (x, state m)

instance (Functor m, Monad m) => ApplyMonadTrans m Tree where
  type ApplyMonadOver m Tree = HSM.TreeMonad m
  runApplyMonad = HSM.virtualTreeMonad

class (Functor m, Monad m, Functor (ApplyMonadBase m), Monad (ApplyMonadBase m), ToTree state)
       -- ApplyMonadOver (ApplyMonadBase m) ~ m is *not* required in general,
       -- since ApplyMonadBase is not injective
       => ApplyMonad m (state :: (* -> *) -> *) where
    type ApplyMonadBase m :: * -> *

    nestedApply :: m x -> state (ApplyMonadBase m) -> m (x, state (ApplyMonadBase m))
    liftApply :: (state (ApplyMonadBase m) -> (ApplyMonadBase m) x) -> state (ApplyMonadBase m)
                 -> m (x, state (ApplyMonadBase m))

    getApplyState :: m (state (ApplyMonadBase m))
    putApplyState :: state m -> m ()

    -- a semantic, ObjectMap-based interface for patch application
    editFile :: (state ~ ObjectMap) => UUID -> (B.ByteString -> B.ByteString) -> m ()
    editDirectory :: (state ~ ObjectMap) => UUID -> (DirContent -> DirContent) -> m ()

    -- a semantic, Tree-based interface for patch application
    mDoesDirectoryExist :: (state ~ Tree) => FileName -> m Bool
    mDoesFileExist :: (state ~ Tree) => FileName -> m Bool
    mReadFilePS :: (state ~ Tree) => FileName -> m B.ByteString
    mReadFilePSs :: (state ~ Tree) => FileName -> m [B.ByteString]
    mReadFilePSs f = linesPS `fmap` mReadFilePS f
    mCreateDirectory :: (state ~ Tree) => FileName -> m ()
    mRemoveDirectory :: (state ~ Tree) => FileName -> m ()
    mCreateFile :: (state ~ Tree) => FileName -> m ()
    mCreateFile f = mModifyFilePS f $ \_ -> return B.empty
    mRemoveFile :: (state ~ Tree) => FileName -> m ()
    mRename :: (state ~ Tree) => FileName -> FileName -> m ()
    mModifyFilePS :: (state ~ Tree) => FileName -> (B.ByteString -> m B.ByteString) -> m ()
    mModifyFilePSs :: (state ~ Tree) => FileName -> ([B.ByteString] -> m [B.ByteString]) -> m ()
    mModifyFilePSs f j = mModifyFilePS f (fmap unlinesPS . j . linesPS)
    mChangePref :: (state ~ Tree) => String -> String -> String -> m ()
    mChangePref _ _ _ = return ()

instance (Functor m, Monad m) => ApplyMonad (HSM.TreeMonad m) Tree where
    type ApplyMonadBase (HSM.TreeMonad m) = m
    getApplyState = gets HSM.tree
    nestedApply a start = lift $ runApplyMonad a start
    liftApply a start = do x <- gets HSM.tree
                           lift $ runApplyMonad (lift $ a x) start

    -- putApplyState needs some support from HSM

    mDoesDirectoryExist d = HSM.directoryExists (fn2ap d)
    mDoesFileExist d = HSM.fileExists (fn2ap d)
    mReadFilePS p = B.concat `fmap` BL.toChunks `fmap` HSM.readFile (fn2ap p)
    mModifyFilePS p j = do have <- HSM.fileExists (fn2ap p)
                           x <- if have then B.concat `fmap` BL.toChunks `fmap` HSM.readFile (fn2ap p)
                                        else return B.empty
                           HSM.writeFile (fn2ap p) . BL.fromChunks . (:[]) =<< j x
    mCreateDirectory p = HSM.createDirectory (fn2ap p)
    mRename from to = HSM.rename (fn2ap from) (fn2ap to)
    mRemoveDirectory = HSM.unlink . fn2ap
    mRemoveFile = HSM.unlink . fn2ap

-- Latest name, current original name.
type OrigFileNameOf = (FileName, FileName)
-- Touched files, new file list (after removes etc.) and rename details
type FilePathMonadState = ([FileName], [FileName], [OrigFileNameOf])
type FilePathMonad = State FilePathMonadState

-- |trackOrigRename takes an old and new name and attempts to apply the mapping
-- to the OrigFileNameOf pair. If the old name is the most up-to-date name of
-- the file in question, the first element of the OFNO will match, otherwise if
-- the up-to-date name was originally old, the second element will match.
trackOrigRename :: FileName -> FileName -> OrigFileNameOf -> OrigFileNameOf
trackOrigRename old new pair@(latest, from)
    | old `isParentOrEqOf` latest = (latest, movedirfilename old new latest)
    | old `isParentOrEqOf` from = (latest, movedirfilename old new from)
    | otherwise = pair

-- |withFileNames takes a maybe list of existing rename-pairs, a list of
-- filenames and an action, and returns the resulting triple of affected files,
-- updated filename list and new rename details. If the rename-pairs are not
-- present, a new list is generated from the filesnames.
withFileNames :: Maybe [OrigFileNameOf] -> [FileName] -> FilePathMonad a
    -> FilePathMonadState
withFileNames mbofnos fps x = execState x ([], fps, ofnos) where
    ofnos = fromMaybe (map (\y -> (y, y)) fps) mbofnos

instance ApplyMonad FilePathMonad Tree where
    type ApplyMonadBase FilePathMonad = Identity
    -- We can't check it actually is a directory here
    mDoesDirectoryExist d = gets $ \(_, fs, _) -> d `elem` fs

    mCreateDirectory = mCreateFile
    mCreateFile f = modify $ \(ms, fs, rns) -> (f : ms, fs, rns)
    mRemoveFile f = modify $ \(ms, fs, rns) -> (f : ms, filter (/= f) fs, rns)
    mRemoveDirectory = mRemoveFile
    mRename a b =
        modify $ \(ms, fs, rns) -> ( a : b : ms
                                   , map (movedirfilename a b) fs
                                   , map (trackOrigRename a b) rns)
    mModifyFilePS f _ = mCreateFile f

instance MonadProgress FilePathMonad where
  runProgressActions = silentlyRunProgressActions

type RestrictedApply = State (M.Map FileName B.ByteString)

instance ApplyMonad RestrictedApply Tree where
  type ApplyMonadBase RestrictedApply = Identity
  mDoesDirectoryExist _ = return True
  mCreateDirectory _ = return ()
  mRemoveFile f = modify $ M.delete f
  mRemoveDirectory _ = return ()
  mRename a b = modify $ M.mapKeys (movedirfilename a b)
  mModifyFilePS f j = do look <- gets $ M.lookup f
                         case look of
                           Nothing -> return ()
                           Just bits -> do
                             new <- j bits
                             modify $ M.insert f new

instance MonadProgress RestrictedApply where
  runProgressActions = silentlyRunProgressActions

withFiles :: [(FileName, B.ByteString)] -> RestrictedApply a -> [(FileName, B.ByteString)]
withFiles p x = M.toList $ execState x $ M.fromList p
