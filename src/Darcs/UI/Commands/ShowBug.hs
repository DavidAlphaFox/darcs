--  Copyright (C) 2007 Eric Kow
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

module Darcs.UI.Commands.ShowBug ( showBug ) where

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( DarcsOption, oid, odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O ( workingRepoDir, StdCmdAction, Verbosity, UseCache )
import Darcs.Util.Path ( AbsolutePath )
#include "impossible.h"

showBugDescription :: String
showBugDescription = "Simulate a run-time failure."

showBugHelp :: String
showBugHelp =
  "Show bug can be used to see what darcs would show you if you encountered.\n"
  ++"a bug in darcs.\n"

showBugBasicOpts :: DarcsOption a (Maybe String -> a)
showBugBasicOpts = O.workingRepoDir

showBugOpts :: DarcsOption a
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

showBugOpts = showBugBasicOpts `withStdOpts` oid

showBug :: DarcsCommand [DarcsFlag]
showBug = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "bug"
    , commandHelp = showBugHelp
    , commandDescription = showBugDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = showBugCmd
    , commandPrereq = findRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showBugBasicOpts
    , commandDefaults = defaultFlags showBugOpts
    , commandCheckOptions = ocheck showBugOpts
    , commandParseOptions = onormalise showBugOpts
    }

showBugCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showBugCmd _ _ _ = bug "This is actually a fake bug in darcs."

