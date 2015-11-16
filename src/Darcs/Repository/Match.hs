--  Copyright (C) 2004-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Darcs.Repository.Match
    (
      getNonrangeMatch
    , getPartialNonrangeMatch
    , getFirstMatch
    , getOnePatchset
    ) where

import Darcs.Patch.Match
    ( getNonrangeMatchS
    , getFirstMatchS
    , nonrangeMatcherIsTag
    , getMatchingTag
    , matchAPatchset
    , nonrangeMatcher
    , applyNInv
    , hasIndexRange
    , MatchFlag(..)
    )

import Darcs.Patch.Bundle ( scanContextFile )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( PatchSet(..), SealedPatchSet, Origin )
import Darcs.Patch.Witnesses.Sealed ( seal )

import Darcs.Repository.Flags
    ( WithWorkingDir (WithWorkingDir) )
import Darcs.Repository.ApplyPatches ( DefaultIO, runDefault )
import Darcs.Repository.Internal
    ( Repository, readRepo, createPristineDirectoryTree )

import Storage.Hashed.Tree ( Tree )

import Darcs.Util.Path ( FileName, toFilePath )

#include "impossible.h"

getNonrangeMatch :: (ApplyMonad DefaultIO (ApplyState p), RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT
                 -> [MatchFlag]
                 -> IO ()
getNonrangeMatch r = withRecordedMatch r . getMatch where
  getMatch fs = case hasIndexRange fs of
    Just (n, m) | n == m -> applyNInv (n-1)
                | otherwise -> fail "Index range is not allowed for this command."
    _ -> getNonrangeMatchS fs

getPartialNonrangeMatch :: (RepoPatch p, ApplyMonad DefaultIO (ApplyState p), ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> [MatchFlag]
                        -> [FileName]
                        -> IO ()
getPartialNonrangeMatch r fs _ =
    withRecordedMatch r (getNonrangeMatchS fs)

getFirstMatch :: (ApplyMonad DefaultIO (ApplyState p), RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> [MatchFlag]
              -> IO ()
getFirstMatch r fs = withRecordedMatch r (getFirstMatchS fs)

getOnePatchset :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository p wR wU wT
               -> [MatchFlag]
               -> IO (SealedPatchSet p Origin)
getOnePatchset repository fs =
    case nonrangeMatcher fs of
        Just m -> do ps <- readRepo repository
                     if nonrangeMatcherIsTag fs
                        then return $ getMatchingTag m ps
                        else return $ matchAPatchset m ps
        Nothing -> seal `fmap` (scanContextFile . toFilePath . context_f $ fs)
    where context_f [] = bug "Couldn't match_nonrange_patchset"
          context_f (Context f:_) = f
          context_f (_:xs) = context_f xs

withRecordedMatch :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository p wR wU wT
                  -> (PatchSet p Origin wR -> DefaultIO ())
                  -> IO ()
withRecordedMatch r job
    = do createPristineDirectoryTree r "." WithWorkingDir
         readRepo r >>= runDefault . job
