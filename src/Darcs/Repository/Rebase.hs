--  Copyright (C) 2009-2012 Ganesh Sittampalam
--
--  BSD3
{-# LANGUAGE CPP #-}
module Darcs.Repository.Rebase
    (
      withManualRebaseUpdate
    , rebaseJob
    , startRebaseJob
    , repoJobOnRebaseRepo
    ) where

import Darcs.Util.Global ( darcsdir )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.CommuteFn ( commuterIdRL )
import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.PatchInfoAnd ( n2pia, hopefully )
import Darcs.Patch.Rebase ( RebaseFixup
                          , Rebasing
                          , mkSuspended
                          , takeHeadRebase
                          , takeAnyRebase
                          , takeAnyRebaseAndTrailingPatches
                          , countToEdit
                          )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase(..)
                                    , RecontextRebase1(..)
                                    , RecontextRebase2(..)
                                    )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Set ( PatchSet(..) )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), RL(..), reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed2(..), FlippedSeal(..) )


import Darcs.Repository.Flags
    ( Compression
    , UpdateWorking(..)
    , Verbosity
    )
import Darcs.Repository.Format
    ( RepoProperty ( RebaseInProgress )
    , formatHas
    , addToFormat
    , removeFromFormat
    , writeRepoFormat
    )
import Darcs.Repository.Internal
    ( tentativelyAddPatch
    , tentativelyAddPatch_
    , tentativelyAddPatches_
    , tentativelyRemovePatches
    , tentativelyRemovePatches_
    , finalizeRepositoryChanges
    , revertRepositoryChanges
    , readTentativeRepo
    , readRepo
    , UpdatePristine(..)
    )
import Darcs.Repository.InternalTypes ( Repository(..) )

import Darcs.Util.Progress ( debugMessage )

import Storage.Hashed.Tree ( Tree )

import Control.Applicative ( (<$>) )
import Control.Exception ( finally )

import System.FilePath.Posix ( (</>) )

#include "impossible.h"

withManualRebaseUpdate
   :: forall p x wR wU wT1 wT2
    . (RepoPatch p, ApplyState p ~ Tree)
   => Compression
   -> Verbosity
   -> UpdateWorking
   -> Repository p wR wU wT1
   -> (Repository p wR wU wT1 -> IO (Repository p wR wU wT2, FL (RebaseFixup p) wT2 wT1, x))
   -> IO (Repository p wR wU wT2, x)
withManualRebaseUpdate compr verb uw r subFunc
 | Just (RecontextRebase1 recontext1) <- recontextRebase :: Maybe (RecontextRebase1 p)
 = do patches <- readTentativeRepo r
      let go :: PatchSet p wS wT1 -> IO (Repository p wR wU wT2, x)
          go (PatchSet NilRL _) = bug "trying to recontext rebase without rebase patch at head (tag)"
          go (PatchSet (q :<: _) _) =
              case recontext1 (hopefully q) of
                 (NotEq, _) -> bug "trying to recontext rebase without rebase patch at head (not match)"
                 (IsEq, recontext2) -> do
                    r' <- tentativelyRemovePatches r compr uw (q :>: NilFL)
                    (r'', fixups, x) <- subFunc r'
                    q' <- n2pia <$> recontextFunc2 recontext2 fixups
                    r''' <- tentativelyAddPatch r'' compr verb uw q'
                    return (r''', x)
      go patches
withManualRebaseUpdate _compr _verb _uw r subFunc
 = do (r', _, x) <- subFunc r
      return (r', x)


-- got a normal darcs operation to run on a repo that happens to have a rebase in progress
repoJobOnRebaseRepo :: (RepoPatch p, ApplyState p ~ Tree)
                    => (Repository (Rebasing p) wR wU wR -> IO a)
                    -> Repository (Rebasing p) wR wU wR
                    -> IO a
repoJobOnRebaseRepo job repo = do
    res <- job repo -- TODO can we munge the repo here to hide the rebase patch?
    displaySuspendedStatus repo
    return res

-- got a rebase operation to run where it is required that a rebase is already in progress
rebaseJob :: (RepoPatch p, ApplyState p ~ Tree)
          => (Repository (Rebasing p) wR wU wR -> IO a)
          -> Repository (Rebasing p) wR wU wR
          -> Compression
          -> Verbosity
          -> UpdateWorking
          -> IO a
rebaseJob job repo compr verb uw = do
    repo' <- moveRebaseToEnd repo compr verb uw
    job repo'
      -- the use of finally here is because various things in job
      -- might cause an "expected" early exit leaving us needing
      -- to remove the rebase-in-progress state (e.g. when suspending,
      -- conflicts with recorded, user didn't specify any patches).
      -- It's a bit questionable/non-standard as it's doing quite a bit
      -- of cleanup and if there was an unexpected error then this
      -- may may things worse.
      -- The better fix would be to standardise expected early exits
      -- e.g. using a layer on top of IO or a common Exception type
      -- and then just catch those.
      `finally` checkSuspendedStatus repo' compr verb uw

-- got a rebase operation to run where we may need to initialise the rebase state first
startRebaseJob :: (RepoPatch p, ApplyState p ~ Tree)
               => (Repository (Rebasing p) wR wU wR -> IO a)
               -> Repository (Rebasing p) wR wU wR
               -> Compression
               -> Verbosity
               -> UpdateWorking
               -> IO a
startRebaseJob job repo compr verb uw = do
    repo' <- startRebaseIfNecessary repo compr verb uw
    rebaseJob job repo' compr verb uw

checkSuspendedStatus :: (RepoPatch p, ApplyState p ~ Tree)
                     => Repository (Rebasing p) wR wU wR
                     -> Compression
                     -> Verbosity
                     -> UpdateWorking
                     -> IO ()
checkSuspendedStatus repo@(Repo _ rf _ _) compr verb uw = do
    allpatches <- readRepo repo
    (_, Sealed2 ps) <- return $ takeAnyRebase allpatches
    case countToEdit ps of
         0 -> do
               debugMessage "Removing the rebase patch file..."
               -- this shouldn't actually be necessary since the count should
               -- only go to zero after an actual rebase operation which would
               -- leave the patch at the end anyway, but be defensive.
               repo' <- moveRebaseToEnd repo compr verb uw
               revertRepositoryChanges repo' uw
               -- in theory moveRebaseToEnd could just return the commuted one,
               -- but since the repository has been committed and re-opened
               -- best to just do things carefully
               (rebase, _, _) <- takeHeadRebase <$> readRepo repo'
               repo'' <- tentativelyRemovePatches repo' compr uw (rebase :>: NilFL)
               finalizeRepositoryChanges repo'' uw compr
               writeRepoFormat (removeFromFormat RebaseInProgress rf) (darcsdir </> "format")
               putStrLn "Rebase finished!"
         n -> putStrLn $ "Rebase in progress: " ++ show n ++ " suspended patches"

moveRebaseToEnd :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository (Rebasing p) wR wU wR
                -> Compression
                -> Verbosity
                -> UpdateWorking
                -> IO (Repository (Rebasing p) wR wU wR)
moveRebaseToEnd repo compr verb uw = do
    allpatches <- readRepo repo
    case takeAnyRebaseAndTrailingPatches allpatches of
        FlippedSeal (_ :> NilRL) -> return repo -- already at head
        FlippedSeal (r :> ps) -> do
            Just (ps' :> r') <- return $ commuterIdRL selfCommuter (r :> ps)
            debugMessage "Moving rebase patch to head..."
            revertRepositoryChanges repo uw
            repo' <- tentativelyRemovePatches_ DontUpdatePristine repo compr uw (reverseRL ps)
            repo'' <- tentativelyRemovePatches_ DontUpdatePristine repo' compr uw (r :>: NilFL)
            repo''' <- tentativelyAddPatches_ DontUpdatePristine repo'' compr verb uw (reverseRL ps')
            repo'''' <- tentativelyAddPatch_ DontUpdatePristine repo''' compr verb uw r'
            finalizeRepositoryChanges repo'''' uw compr
            return repo''''

displaySuspendedStatus :: (RepoPatch p, ApplyState p ~ Tree) => Repository (Rebasing p) wR wU wR -> IO ()
displaySuspendedStatus repo = do
    allpatches <- readRepo repo
    (_, Sealed2 ps) <- return $ takeAnyRebase allpatches
    putStrLn $ "Rebase in progress: " ++ show (countToEdit ps) ++ " suspended patches"

startRebaseIfNecessary :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository (Rebasing p) wR wU wT
                       -> Compression
                       -> Verbosity
                       -> UpdateWorking
                       -> IO (Repository (Rebasing p) wR wU wT)
startRebaseIfNecessary repo@(Repo _ rf _ _) compr verb uw =
    if formatHas RebaseInProgress rf
    then return repo
    else do -- TODO this isn't under the repo lock, and it should be
           writeRepoFormat (addToFormat RebaseInProgress rf) (darcsdir </> "format")
           debugMessage "Writing the rebase patch file..."
           revertRepositoryChanges repo uw
           mypatch <- mkSuspended NilFL
           repo' <- tentativelyAddPatch_ UpdatePristine repo compr verb uw $ n2pia mypatch
           finalizeRepositoryChanges repo' uw compr
           return repo'

