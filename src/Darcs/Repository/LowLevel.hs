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

{-# LANGUAGE CPP #-}

module Darcs.Repository.LowLevel
    ( readPending
    , readTentativePending
    , writeTentativePending
    -- deprecated interface:
    , readNewPending
    , writeNewPending
    , pendingName
    ) where

import Control.Applicative
import qualified Data.ByteString as BS ( empty )

import Darcs.Util.Global ( darcsdir )
import Darcs.Repository.Lock ( writeDocBinFile )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Patch ( readPatch, RepoPatch, PrimOf )
import Darcs.Patch.Read ( ReadPatch(..), bracketedFL )
import Darcs.Patch.ReadMonads ( ParserM )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Util.Exception ( catchall )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), mapSeal )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Printer ( Doc, ($$), (<>), text, vcat )

pendingName :: String
pendingName = darcsdir ++ "/patches/pending"

newSuffix, tentativeSuffix :: String
newSuffix = ".new"
tentativeSuffix = ".tentative"

-- | Read the contents of pending.
-- The return type is currently incorrect as it refers to the tentative
-- state rather than the recorded state.
readPending :: RepoPatch p => Repository p wR wU wT
            -> IO (Sealed (FL (PrimOf p) wT))
readPending = readPendingFile ""

-- |Read the contents of tentative pending.
readTentativePending :: RepoPatch p => Repository p wR wU wT
                     -> IO (Sealed (FL (PrimOf p) wT))
readTentativePending = readPendingFile tentativeSuffix

-- |Read the contents of tentative pending.
readNewPending :: RepoPatch p => Repository p wR wU wT
               -> IO (Sealed (FL (PrimOf p) wT))
readNewPending = readPendingFile newSuffix

-- |Read the pending file with the given suffix. CWD should be the repository
-- directory.
readPendingFile :: ReadPatch prim => String -> Repository p wR wU wT
                -> IO (Sealed (FL prim wX))
readPendingFile suffix _ = do
    pend <- gzReadFilePS (pendingName ++ suffix) `catchall` return BS.empty
    return . maybe (Sealed NilFL) (mapSeal unFLM) . readPatch $ pend

-- Wrapper around FL where printed format uses { } except around singletons.
-- Now that the Show behaviour of FL p can be customised (using
-- showFLBehavior), we could instead change the general behaviour of FL Prim;
-- but since the pending code can be kept nicely compartmentalised, it's nicer
-- to do it this way.
newtype FLM p wX wY = FLM { unFLM :: FL p wX wY }

instance ReadPatch p => ReadPatch (FLM p) where
    readPatch' = mapSeal FLM <$> readMaybeBracketedFL readPatch' '{' '}'

instance ShowPatchBasic p => ShowPatchBasic (FLM p) where
    showPatch = showMaybeBracketedFL showPatch '{' '}' . unFLM

readMaybeBracketedFL :: forall m p wX . ParserM m
                     => (forall wY . m (Sealed (p wY))) -> Char -> Char
                     -> m (Sealed (FL p wX))
readMaybeBracketedFL parser pre post =
    bracketedFL parser pre post <|> (mapSeal (:>:NilFL) <$> parser)

showMaybeBracketedFL :: (forall wX wY . p wX wY -> Doc) -> Char -> Char
                     -> FL p wA wB -> Doc
showMaybeBracketedFL _ pre post NilFL = text [pre] $$ text [post]
showMaybeBracketedFL printer _ _ (p :>: NilFL) = printer p
showMaybeBracketedFL printer pre post ps = text [pre] $$
                                           vcat (mapFL printer ps) $$
                                           text [post]

-- |Write the contents of tentative pending.
writeTentativePending :: RepoPatch p => Repository p wR wU wT
                      -> FL (PrimOf p) wT wY -> IO ()
writeTentativePending = writePendingFile tentativeSuffix

-- |Write the contents of new pending. CWD should be the repository directory.
writeNewPending :: RepoPatch p => Repository p wR wU wT
                               -> FL (PrimOf p) wT wY -> IO ()
writeNewPending = writePendingFile newSuffix

-- Write a pending file, with the given suffix. CWD should be the repository
-- directory.
writePendingFile :: ShowPatchBasic prim => String -> Repository p wR wU wT
                 -> FL prim wX wY -> IO ()
writePendingFile suffix _ = writePatch name . FLM
  where
    name = pendingName ++ suffix

writePatch :: ShowPatchBasic p => FilePath -> p wX wY -> IO ()
writePatch f p = writeDocBinFile f $ showPatch p <> text "\n"
