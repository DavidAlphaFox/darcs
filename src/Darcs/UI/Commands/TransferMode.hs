--  Copyright (C) 2008 David Roundy
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

{-# LANGUAGE CPP #-}

-- The pragma above is only for pattern guards.
module Darcs.UI.Commands.TransferMode ( transferMode ) where

import Prelude hiding ( catch )
import Control.Exception ( catch )
import System.IO ( stdout, hFlush )

import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Exception ( prettyException )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( DarcsOption, oid, odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Progress ( setProgressMode )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Path ( AbsolutePath )

import qualified Data.ByteString as B (hPut, readFile, length, ByteString)

transferModeDescription :: String
transferModeDescription = "Internal command for efficient ssh transfers."

transferModeHelp :: String
transferModeHelp =
 "When pulling from or pushing to a remote repository over ssh, if both\n" ++
 "the local and remote ends have Darcs 2, the `transfer-mode' command\n" ++
 "will be invoked on the remote end.  This allows Darcs to intelligently\n" ++
 "transfer information over a single ssh connection.\n" ++
 "\n" ++
 "If either end runs Darcs 1, a separate ssh connection will be created\n" ++
 "for each transfer.  As well as being less efficient, this means users\n" ++
 "who do not run ssh-agent will be prompted for the ssh password tens or\n" ++
 "hundreds of times!\n"

transferModeBasicOpts :: DarcsOption a (Maybe String -> a)
transferModeBasicOpts = O.workingRepoDir

transferModeOpts :: DarcsOption a
                    (Maybe String
                     -> Maybe O.StdCmdAction
                     -> Bool
                     -> Bool
                     -> O.Verbosity
                     -> Bool
                     -> O.UseCache
                     -> Maybe String
                     -> Bool
                     -> Maybe String
                     -> Bool
                     -> a)
transferModeOpts = transferModeBasicOpts `withStdOpts` oid

transferMode :: DarcsCommand [DarcsFlag]
transferMode = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "transfer-mode"
    , commandHelp = transferModeHelp
    , commandDescription = transferModeDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandGetArgPossibilities = return []
    , commandCommand = transferModeCmd
    , commandPrereq = amInRepository
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc transferModeBasicOpts
    , commandDefaults = defaultFlags transferModeOpts
    , commandCheckOptions = ocheck transferModeOpts
    , commandParseOptions = onormalise transferModeOpts
    }

transferModeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
transferModeCmd _ _ _ = do setProgressMode False
                           putStrLn "Hello user, I am darcs transfer mode"
                           hFlush stdout
                           withCurrentDirectory darcsdir transfer

transfer :: IO ()
transfer = do 'g':'e':'t':' ':fn <- getLine
              x <- readfile fn
              case x of
                Right c -> do putStrLn $ "got " ++ fn
                              print $ B.length c
                              B.hPut stdout c
                              hFlush stdout
                Left e -> do putStrLn $ "error " ++ fn
                             print e
                             hFlush stdout
              transfer

readfile :: String -> IO (Either String B.ByteString)
readfile fn = (Right `fmap` B.readFile fn) `catch` (return . Left  . prettyException)

