-- Copyright (C) 2005 Tomasz Zielonka
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

-- |
-- Module      : Darcs.Util.Global
-- Copyright   : 2005 Tomasz Zielonka
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- This was originally Tomasz Zielonka's AtExit module, slightly generalised
-- to include global variables.  Here, we attempt to cover broad, global
-- features, such as exit handlers.  These features slightly break the Haskellian
-- purity of darcs, in favour of programming convenience.

module Darcs.Util.Global
    (
      timingsMode
    , setTimingsMode
    , whenDebugMode
    , withDebugMode
    , setDebugMode
    , debugMessage
    , debugFail
    , putTiming
    , addCRCWarning
    , getCRCWarnings
    , resetCRCWarnings
    , addBadSource
    , getBadSourcesList
    , isBadSource
    , darcsdir
    , darcsLastMessage
    , darcsSendMessage
    , darcsSendMessageFinal
    , isReachableSource
    , addReachableSource
    ) where


import Control.Monad ( when )
import Data.IORef ( modifyIORef, IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe (unsafePerformIO)
import System.IO ( hPutStrLn, hPutStr, stderr )
import System.Time ( calendarTimeToString, toCalendarTime, getClockTime )
import System.FilePath.Posix ( combine, (<.>) )
import Prelude hiding (catch)


-- Write-once-read-many global variables make it easier to implement flags, such
-- as --no-ssh-cm. Using global variables reduces the number of parameters that
-- we have to pass around, but it is rather unsafe and should be used sparingly.


_debugMode :: IORef Bool
_debugMode = unsafePerformIO $ newIORef False
{-# NOINLINE _debugMode #-}


setDebugMode :: IO ()
setDebugMode = writeIORef _debugMode True


whenDebugMode :: IO () -> IO ()
whenDebugMode j = do b <- readIORef _debugMode
                     when b j


withDebugMode :: (Bool -> IO a) -> IO a
withDebugMode j = readIORef _debugMode >>= j


debugMessage :: String -> IO ()
debugMessage m = whenDebugMode $ do putTiming; hPutStrLn stderr m


debugFail :: String -> IO a
debugFail m = debugMessage m >> fail m


putTiming :: IO ()
putTiming = when timingsMode $ do
    t <- getClockTime >>= toCalendarTime
    hPutStr stderr (calendarTimeToString t++": ")


_timingsMode :: IORef Bool
_timingsMode = unsafePerformIO $ newIORef False
{-# NOINLINE _timingsMode #-}


setTimingsMode :: IO ()
setTimingsMode = writeIORef _timingsMode True


timingsMode :: Bool
timingsMode = unsafePerformIO $ readIORef _timingsMode
{-# NOINLINE timingsMode #-}


type CRCWarningList = [FilePath]
_crcWarningList :: IORef CRCWarningList
_crcWarningList = unsafePerformIO $ newIORef []
{-# NOINLINE _crcWarningList #-}


addCRCWarning :: FilePath -> IO ()
addCRCWarning fp = modifyIORef _crcWarningList (fp:)


getCRCWarnings :: IO [FilePath]
getCRCWarnings = readIORef _crcWarningList


resetCRCWarnings :: IO ()
resetCRCWarnings = writeIORef _crcWarningList []


_badSourcesList :: IORef [String]
_badSourcesList = unsafePerformIO $ newIORef []
{-# NOINLINE _badSourcesList #-}


addBadSource :: String -> IO ()
addBadSource cache = modifyIORef _badSourcesList (cache:)


getBadSourcesList :: IO [String]
getBadSourcesList = readIORef _badSourcesList


isBadSource :: IO (String -> Bool)
isBadSource = do
    badSources <- getBadSourcesList
    return (`elem` badSources)


_reachableSourcesList :: IORef [String]
_reachableSourcesList = unsafePerformIO $ newIORef []
{-# NOINLINE _reachableSourcesList #-}


addReachableSource :: String -> IO ()
addReachableSource src = modifyIORef _reachableSourcesList (src:)


getReachableSources :: IO [String]
getReachableSources = readIORef _reachableSourcesList


isReachableSource :: IO (String -> Bool)
isReachableSource =  do
    reachableSources <- getReachableSources
    return (`elem` reachableSources)


darcsdir :: String
darcsdir = "_darcs"

darcsLastMessage :: String
darcsLastMessage = combine darcsdir "patch_description.txt"

darcsSendMessage :: String 
darcsSendMessage = combine darcsdir "darcs-send"

darcsSendMessageFinal :: String
darcsSendMessageFinal = darcsSendMessage <.> "final"
