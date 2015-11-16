-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP #-}


module Darcs.Repository.InternalTypes ( Repository(..), Pristine(..)
                                      , extractCache, modifyCache
                                      ) where

import Data.List ( nub, sortBy )
import Darcs.Repository.Cache ( Cache (..) , compareByLocality )
import Darcs.Repository.Format ( RepoFormat )
import Darcs.Patch ( RepoPatch )

data Pristine
  = NoPristine
  | PlainPristine
  | HashedPristine
    deriving ( Show, Eq )

-- |A @Repository@ is a token representing the state of a repository on disk.
-- It is parameterized by the patch type in the repository, and witnesses for
-- the recorded state of the repository (i.e. what darcs get would retrieve),
-- the unrecorded state (what's in the working directory now),
-- and the tentative state, which represents work in progress that will
-- eventually become the new recorded state unless something goes wrong.
data Repository (p :: * -> * -> *) wRecordedstate wUnrecordedstate wTentativestate =
  Repo !String !RepoFormat !Pristine Cache deriving ( Show )

extractCache :: Repository p wR wU wT -> Cache
extractCache (Repo _ _ _ c) = c

-- | 'modifyCache' @repository function@ modifies the cache of
--   @repository@ with @function@, remove duplicates and sort the results with 'compareByLocality'.
modifyCache :: forall p wR wU wT . (RepoPatch p)  => Repository p wR wU wT -> (Cache -> Cache) -> Repository p wR wU wT
modifyCache (Repo dir rf pristine cache) f
   = Repo dir rf pristine $ cmap ( sortBy compareByLocality . nub ) $ f cache
  where cmap g (Ca c) = Ca (g c)
