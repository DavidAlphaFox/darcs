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

module Darcs.Repository.Motd
    ( getMotd
    , showMotd
    ) where

import Control.Monad ( unless )
import qualified Data.ByteString as B (null, hPut, empty, ByteString)
import System.IO ( stdout )

import Darcs.Repository.External ( fetchFilePS, Cachable(..) )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Exception ( catchall )

-- | Fetch and return the message of the day for a given repository.
getMotd :: String -> IO B.ByteString
getMotd repo = fetchFilePS motdPath (MaxAge 600) `catchall` return B.empty
  where
    motdPath = repo ++ "/" ++ darcsdir ++ "/prefs/motd"

-- | Display the message of the day for a given repository,
showMotd :: String -> IO ()
showMotd repo = do
    motd <- getMotd repo
    unless (B.null motd) $ do
        B.hPut stdout motd
        putStrLn $ replicate 22 '*'
