{-# LANGUAGE DeriveFunctor #-}
-- Copyright (C) 2009-2010 Benedikt Schmidt
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

module Darcs.Patch.Index.Types where

import Darcs.Util.Crypt.SHA1( SHA1(..) )
import Darcs.Util.Path ( fp2fn, fn2fp, FileName )
import Darcs.Patch.Info ( makePatchname, PatchInfo )
import Data.Binary ( Binary(..) )
import Data.Word ( Word32 )

-- | The FileId for a file consists of the FilePath (creation name)
--   and an index. The index denotes how many files
--   with the same name have been added before (and subsequently
--   deleted or moved)
data FileId = FileId {cname::FileName,count::Int}
  deriving (Eq,Show,Ord)

instance Binary FileId where
  put (FileId  rfp i) = put (rfp,i)
  get = do
   (rfp,cnt) <- get
   return $ FileId rfp cnt

-- | Parse FileId from a string
parseFileId :: String -> FileId
parseFileId s = let (f,'.':i) = break (=='.') s in FileId (fp2fn f) (read i)

-- | Convert FileId to string
showFileId :: FileId -> String
showFileId (FileId fn i) = show i++"#"++fn2fp fn

-- | The PatchId identifies a patch and can be created from a PatchInfo with makePatchname
newtype PatchId = PID {patchId :: SHA1}
  deriving (Show,Ord,Eq)

instance Binary PatchId where
  put (PID p) = put p
  get = PID `fmap` get

pid2string :: PatchId -> String
pid2string = show . patchId

-- | describes a filepath that is interpreted relative to a certain
--   point in the history of the repository. The point is given by
--   Just pid which denotes the history up to (including) pid or
--   Nothing which denotes the history including the last patch
data DatedFilePath = DatedFilePath FilePath (Maybe PatchId)

-- | This is used to track changes to files
data PatchMod a = PTouch a
                | PCreateFile a
                | PCreateDir a
                | PRename a a
                | PRemove a
                | PInvalid a         -- ^ This is an invalid patch
                                     --   e.g. there is a patch 'Move Autoconf.lhs Autoconf.lhs.in'
                                     --   where there is no Autoconf.lhs in the darcs repo
                | PDuplicateTouch a  -- ^ this is used for duplicate patches that don't
                                     --   have any effect, but we still want to keep
                                     --   track of them
 deriving (Show, Eq, Functor)

makePatchID :: PatchInfo -> PatchId
makePatchID = PID . makePatchname

short :: PatchId -> Word32
short (PID (SHA1 a _ _ _ _)) = a

