-- Copyright (C) 2002-2003 David Roundy
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
-- Module      : Main
-- Copyright   : 2002-2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Main ( main ) where

import Control.Exception ( AssertionFailed(..), handle )
import Control.Monad ( forM_ )
import System.IO ( stdin, stdout, stderr, hSetBinaryMode )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs )

import Darcs.UI.RunCommand ( runTheCommand )
import Darcs.UI.Commands.Help ( helpCmd, listAvailableCommands, printVersion,
                             commandControlList )
import Darcs.UI.Flags ( DarcsFlag(Verbose) )
import Darcs.Util.AtExit ( withAtexit, atexit )
import Darcs.Repository( reportBadSources )
import Darcs.Util.SignalHandler ( withSignalsHandled )

import Darcs.Util.ByteString ( decodeString )
import Darcs.UI.External ( setDarcsEncodings )
import Darcs.Util.Exec ( ExecException(..) )
import Darcs.Util.Path ( getCurrentDirectory )
import Version ( version, context, builddeps )

#include "impossible.h"

execExceptionHandler :: ExecException -> IO a
execExceptionHandler (ExecException cmd args redirects reason) = do
    putStrLn . unlines $
        [ "Failed to execute external command: " ++ unwords (cmd:args)
        , "Lowlevel error: " ++ reason
        , "Redirects: " ++ show redirects
        ]
    exitWith $ ExitFailure 3

main :: IO ()
main = withAtexit . withSignalsHandled . handleExecFail . handleAssertFail $ do
    atexit reportBadSources
    setDarcsEncodings
    argv <- getArgs >>= mapM decodeString
    here <- getCurrentDirectory
    let runHelpCmd = helpCmd (here, here) [] []
    -- Explicitly handle no-args and special "help" arguments.
    case argv of
        [] -> printVersion >> runHelpCmd
        ["-h"] -> runHelpCmd
        ["--help"] -> runHelpCmd
        ["--overview"] -> helpCmd (here, here) [Verbose] []
        ["--commands"] -> listAvailableCommands
        ["-v"] -> putStrLn version
        ["--version"] -> putStrLn version
        ["--exact-version"] -> printExactVersion
        _ -> do
            forM_ [stdout, stdin, stderr] $ \h -> hSetBinaryMode h True
            runTheCommand commandControlList (head argv) (tail argv)
  where
    handleExecFail = handle execExceptionHandler
    handleAssertFail = handle $ \(AssertionFailed e) -> bug e
    printExactVersion =  do
        putStrLn $ "darcs compiled on " ++ __DATE__ ++ ", at " ++ __TIME__
        putStrLn context
        putStrLn "Compiled with:\n"
        putStr builddeps

