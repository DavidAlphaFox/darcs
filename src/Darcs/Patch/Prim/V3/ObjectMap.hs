-- Copyright (C) 2011 Petr Rockai
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

module Darcs.Patch.Prim.V3.ObjectMap( UUID(..), Location, Object(..),
                                      ObjectMap(..), DirContent ) where

import Storage.Hashed.Hash( Hash )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Map as M


newtype UUID = UUID BS.ByteString deriving (Eq, Ord, Show)
type Location = (UUID, BS.ByteString)
type DirContent = M.Map BS.ByteString UUID
data Object (m :: * -> *) = Directory DirContent
                          | Blob (m BS.ByteString) !Hash

data ObjectMap (m :: * -> *) = ObjectMap { getObject :: UUID -> m (Maybe (Object m))
                                         , putObject :: UUID -> Object m -> m (ObjectMap m)
                                         , listObjects :: m [UUID]
                                         }
