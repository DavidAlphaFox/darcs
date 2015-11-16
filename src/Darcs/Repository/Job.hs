-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# LANGUAGE CPP, ScopedTypeVariables, Rank2Types, RankNTypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Darcs.Repository.Job
    ( RepoJob(..)
    , withRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryDirectory
    ) where


import Darcs.Util.Global ( darcsdir )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.Named ( Named )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Prim ( PrimOf )
import Darcs.Patch.Rebase ( Rebasing )
import Darcs.Patch.RepoPatch ( RepoPatch )

import Darcs.Repository.Flags
    ( UseCache(..), UpdateWorking(..), DryRun(..), UMask (..)
    , Compression, Verbosity )
import Darcs.Repository.Format
    ( RepoProperty( Darcs2
                  , RebaseInProgress
                  )
    , formatHas
    , writeProblem
    )
import Darcs.Repository.Internal
    ( identifyRepository
    , revertRepositoryChanges
    )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository.Rebase
    ( repoJobOnRebaseRepo
    , startRebaseJob
    , rebaseJob
    )
import Darcs.Repository.Lock ( withLock, withLockCanFail )

import Darcs.Util.Progress ( debugMessage )

import Control.Monad ( when )
import Control.Exception ( bracket_ )

import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno )
import Foreign.C.Types ( CInt(..) )

import Storage.Hashed.Tree ( Tree )

#include "impossible.h"

getUMask :: UMask -> Maybe String
getUMask (YesUMask s) = Just s
getUMask NoUMask = Nothing

withUMaskFlag :: UMask -> IO a -> IO a
withUMaskFlag = maybe id withUMask . getUMask

foreign import ccall unsafe "umask.h set_umask" set_umask
    :: CString -> IO CInt
foreign import ccall unsafe "umask.h reset_umask" reset_umask
    :: CInt -> IO CInt

withUMask :: String
          -> IO a
          -> IO a
withUMask umask job =
    do rc <- withCString umask set_umask
       when (rc < 0) (throwErrno "Couldn't set umask")
       bracket_
           (return ())
           (reset_umask rc)
           job

-- |A @RepoJob@ wraps up an action to be performed with a repository. Because repositories
-- can contain different types of patches, such actions typically need to be polymorphic
-- in the kind of patch they work on. @RepoJob@ is used to wrap up the polymorphism,
-- and the various functions that act on a @RepoJob@ are responsible for instantiating
-- the underlying action with the appropriate patch type.
data RepoJob a
    -- = RepoJob (forall p wR wU . RepoPatch p => Repository p wR wU wR -> IO a)
    -- TODO: Unbind Tree from RepoJob, possibly renaming existing RepoJob
    =
    -- |The most common @RepoJob@; the underlying action can accept any patch type that
    -- a darcs repository may use.
      RepoJob (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
               => Repository p wR wU wR -> IO a)
    -- |A job that only works on darcs 1 patches
    | V1Job (forall wR wU . Repository (Patch Prim) wR wU wR -> IO a)
    -- |A job that only works on darcs 2 patches
    | V2Job (forall wR wU . Repository (RealPatch Prim) wR wU wR -> IO a)
    -- |A job that works on any repository where the patch type @p@ has 'PrimOf' @p@ = 'Prim'.
    --
    -- This was added to support darcsden, which inspects the internals of V1 prim patches.
    --
    -- In future this should be replaced with a more abstract inspection API as part of 'PrimPatch'.
    | PrimV1Job (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, PrimOf p ~ Prim)
               => Repository p wR wU wR -> IO a)
    -- A job that works on normal darcs repositories, but will want access to the rebase patch if it exists.
    | RebaseAwareJob Compression Verbosity UpdateWorking (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree, PrimOf (Named p) ~ PrimOf p) => Repository p wR wU wR -> IO a)
    | RebaseJob Compression Verbosity UpdateWorking (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree, PrimOf (Named p) ~ PrimOf p) => Repository (Rebasing p) wR wU wR -> IO a)
    | StartRebaseJob Compression Verbosity UpdateWorking (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree, PrimOf (Named p) ~ PrimOf p) => Repository (Rebasing p) wR wU wR -> IO a)

onRepoJob :: RepoJob a
          -> (forall p wR wU . (RepoPatch p, ApplyState p ~ Tree) => (Repository p wR wU wR -> IO a) -> Repository p wR wU wR -> IO a)
          -> RepoJob a
onRepoJob (RepoJob job) f = RepoJob (f job)
-- onRepoJob (TreeJob job) f = TreeJob (f job)
onRepoJob (V1Job job) f = V1Job (f job)
onRepoJob (V2Job job) f = V2Job (f job)
onRepoJob (PrimV1Job job) f = PrimV1Job (f job)
onRepoJob (RebaseAwareJob compr verb uw job) f = RebaseAwareJob compr verb uw (f job)
onRepoJob (RebaseJob compr verb uw job) f      = RebaseJob compr verb uw (f job)
onRepoJob (StartRebaseJob compr verb uw job) f = StartRebaseJob compr verb uw (f job)

-- | apply a given RepoJob to a repository in the current working directory
withRepository :: UseCache -> RepoJob a -> IO a
withRepository useCache = withRepositoryDirectory useCache "."

-- | apply a given RepoJob to a repository in a given url
withRepositoryDirectory :: UseCache -> String -> RepoJob a -> IO a
withRepositoryDirectory useCache url repojob = do
    Repo dir rf t c <- identifyRepository useCache url

    let
        startRebase =
            case repojob of
                StartRebaseJob {} -> True
                _ -> False

    case (formatHas Darcs2 rf, startRebase || formatHas RebaseInProgress rf) of

        (True,  False)  -> do
            debugMessage $ "Identified darcs-2 repo: " ++ dir
            let therepo = Repo dir rf t c :: Repository (RealPatch Prim) wR wU wR
            case repojob of
                RepoJob job -> job therepo
                PrimV1Job job -> job therepo
                -- TreeJob job -> job therepo
                V2Job job -> job therepo
                V1Job _ -> fail $    "This repository contains darcs v1 patches,"
                                  ++ " but the command requires darcs v2 patches."
                RebaseAwareJob _compr _verb _uw job -> job therepo
                RebaseJob {} -> fail "No rebase in progress. Try 'darcs rebase suspend' first."
                StartRebaseJob {} -> impossible

        (False, False)  -> do
            debugMessage $ "Identified darcs-1 repo: " ++ dir
            let therepo = Repo dir rf t c :: Repository (Patch Prim) wR wU wR
            case repojob of
                RepoJob job -> job therepo
                PrimV1Job job -> job therepo
                V1Job job -> job therepo
                V2Job _ -> fail $    "This repository contains darcs v2 patches,"
                                  ++ " but the command requires darcs v1 patches."
                RebaseAwareJob _compr _verb _uw job -> job therepo
                RebaseJob {} -> fail "No rebase in progress. Try 'darcs rebase suspend' first."
                StartRebaseJob {} -> impossible

        (True,  True )  -> do
            debugMessage $ "Identified darcs-2 rebase repo: " ++ dir
            let therepo = Repo dir rf t c :: Repository (Rebasing (RealPatch Prim)) wR wU wR
            case repojob of
                RepoJob job -> repoJobOnRebaseRepo job therepo
                PrimV1Job job -> repoJobOnRebaseRepo job therepo
                -- TreeJob job -> job therepo
                V2Job _ -> fail "This command is not supported while a rebase is in progress."
                V1Job _ -> fail $    "This repository contains darcs v1 patches,"
                                  ++ " but the command requires darcs v2 patches."
                RebaseAwareJob compr verb uw job -> rebaseJob job therepo compr verb uw
                RebaseJob compr verb uw job -> rebaseJob job therepo compr verb uw
                StartRebaseJob compr verb uw job -> startRebaseJob job therepo compr verb uw

        (False,  True ) -> do
            debugMessage $ "Identified darcs-1 rebase repo: " ++ dir
            let therepo = Repo dir rf t c :: Repository (Rebasing (Patch Prim)) wR wU wR
            case repojob of
                RepoJob job -> repoJobOnRebaseRepo job therepo
                PrimV1Job job -> repoJobOnRebaseRepo job therepo
                V1Job _ -> fail "This command is not supported while a rebase is in progress."
                V2Job _ -> fail $    "This repository contains darcs v2 patches,"
                                  ++ " but the command requires darcs v1 patches."
                RebaseAwareJob compr verb uw job -> rebaseJob job therepo compr verb uw
                RebaseJob compr verb uw job -> rebaseJob job therepo compr verb uw
                StartRebaseJob compr verb uw job -> startRebaseJob job therepo compr verb uw

-- | apply a given RepoJob to a repository in the current working directory,
--   taking a lock
withRepoLock :: DryRun -> UseCache -> UpdateWorking -> UMask -> RepoJob a -> IO a
withRepoLock dry useCache uw um repojob =
    withRepository useCache $ onRepoJob repojob $ \job repository@(Repo _ rf _ _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFlag um $
         if dry == YesDryRun
           then job repository
           else withLock name (revertRepositoryChanges repository uw >> job repository)

-- | apply a given RepoJob to a repository in the current working directory,
--   taking a lock. If lock not takeable, do nothing.
withRepoLockCanFail :: UseCache -> UpdateWorking -> UMask -> RepoJob () -> IO ()
withRepoLockCanFail useCache uw um repojob =
    withRepository useCache $ onRepoJob repojob $ \job repository@(Repo _ rf _ _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFlag um $ do
         eitherDone <- withLockCanFail name (revertRepositoryChanges repository uw >> job repository)
         case eitherDone of
           Left  _ -> debugMessage "Lock could not be obtained, not doing the job."
           Right _ -> return ()

