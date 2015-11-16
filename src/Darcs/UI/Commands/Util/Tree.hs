--  Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Commands.Util.Tree
    ( 
    -- * Tree lookup.
      treeHas
    , treeHasDir
    , treeHasFile
    , treeHasAnycase
    ) where

import Control.Monad ( forM )
import Control.Monad.State.Strict( gets )

import qualified Data.ByteString.Char8 as BSC
import Data.Char ( toLower )

import Storage.Hashed.Monad
    ( withDirectory, fileExists, directoryExists
    , virtualTreeMonad, currentDirectory
    , TreeMonad )
import qualified Storage.Hashed.Monad as HS ( exists, tree )
import Storage.Hashed.Tree ( Tree, listImmediate, findTree )
import Storage.Hashed ( floatPath )

import Darcs.Util.Path
    ( AnchoredPath(..), Name(..) )

treeHasAnycase :: (Functor m, Monad m)
               => Tree m
               -> FilePath
               -> m Bool
treeHasAnycase tree path =
    fst `fmap` virtualTreeMonad (existsAnycase $ floatPath path) tree


existsAnycase :: (Functor m, Monad m)
              => AnchoredPath
              -> TreeMonad m Bool
existsAnycase (AnchoredPath []) = return True
existsAnycase (AnchoredPath (Name x:xs)) = do
     wd <- currentDirectory
     Just tree <- gets (flip findTree wd . HS.tree)
     let subs = [ AnchoredPath [Name n] | (Name n, _) <- listImmediate tree,
                                          BSC.map toLower n == BSC.map toLower x ]
     or `fmap` forM subs (\path -> do
       file <- fileExists path
       if file then return True
               else withDirectory path (existsAnycase $ AnchoredPath xs))


treeHas :: (Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHas tree path = fst `fmap` virtualTreeMonad (HS.exists $ floatPath path) tree

treeHasDir :: (Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasDir tree path = fst `fmap` virtualTreeMonad (directoryExists $ floatPath path) tree

treeHasFile :: (Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasFile tree path = fst `fmap` virtualTreeMonad (fileExists $ floatPath path) tree
