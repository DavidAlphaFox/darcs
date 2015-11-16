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
-- Module      : Darcs.Util.AtExit
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

module Darcs.Util.AtExit
    (
      atexit
    , withAtexit
    ) where


import Control.Concurrent.MVar
import Control.Exception
    ( bracket_, catch, SomeException
    , mask
    )
import System.IO.Unsafe (unsafePerformIO)
import System.IO ( hPutStrLn, stderr, hPrint )
import Prelude hiding (catch)


atexitActions :: MVar (Maybe [IO ()])
atexitActions = unsafePerformIO (newMVar (Just []))
{-# NOINLINE atexitActions #-}


-- | Registers an IO action to run just before darcs exits. Useful for removing
-- temporary files and directories, for example. Referenced in Issue1914.
atexit :: IO ()
       -> IO ()
atexit action =
    modifyMVar_ atexitActions $ \ml ->
        case ml of
            Just l ->
                return (Just (action : l))
            Nothing -> do
                hPutStrLn stderr "It's too late to use atexit"
                return Nothing


withAtexit :: IO a -> IO a
withAtexit = bracket_ (return ()) exit
  where
    exit = mask $ \unmask -> do
        Just actions <- swapMVar atexitActions Nothing
        -- from now on atexit will not register new actions
        mapM_ (runAction unmask) actions
    runAction unmask action =
        catch (unmask action) $ \(exn :: SomeException) -> do
            hPutStrLn stderr "Exception thrown by an atexit registered action:"
            hPrint stderr exn

