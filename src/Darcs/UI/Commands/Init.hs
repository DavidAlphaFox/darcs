--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Init ( initialize, initializeCmd ) where

import Prelude hiding ( (^) )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amNotInRepository, putInfo, formatPath )
import Darcs.UI.Flags ( DarcsFlag( WorkRepoDir ), withWorkingDir, patchFormat, runPatchIndex )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
    ( patchFormat, useWorkingDir, workingRepoDir, patchIndex, hashed
    , PatchFormat, WithWorkingDir, WithPatchIndex
    , StdCmdAction, Verbosity, UseCache, PatchFormat(..)
    )
import Darcs.UI.Options.All (  )
import Darcs.Util.Printer ( text )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Repository ( createRepository )

initializeDescription :: String
initializeDescription = "Make the current directory or the specified directory a repository."

initializeHelp :: String
initializeHelp =
 "The `darcs initialize` command turns the current directory into a\n" ++
 "Darcs repository.  Any existing files and subdirectories become\n" ++
 "UNSAVED changes: record them with `darcs record --look-for-adds`.\n" ++
 "\n" ++
 "This command creates the `_darcs` directory, which stores version\n" ++
 "control metadata.  It also contains per-repository settings in\n" ++
 "`_darcs/prefs/`, which you can read about in the user manual.\n" ++
 "\n" ++
 "By default, patches of the new repository are in the darcs-2 semantics.\n" ++
 "However it is possible to create a repository in darcs-1 semantics with\n" ++
 "the flag `--darcs-1`, althought this is not recommended except for sharing\n" ++
 "patches with a project that uses patches in the darcs-1 semantics.\n" ++
 "\n" ++
 "Initialize is commonly abbreviated to `init`.\n"

initBasicOpts :: DarcsOption a (O.PatchFormat -> O.WithWorkingDir -> Maybe String -> a)
initBasicOpts = O.patchFormat ^ O.useWorkingDir ^ O.workingRepoDir

initAdvancedOpts :: DarcsOption a (O.WithPatchIndex -> () -> a)
initAdvancedOpts = O.patchIndex ^ O.hashed

initOpts :: DarcsOption a
            (O.PatchFormat
             -> O.WithWorkingDir
             -> Maybe String
             -> Maybe O.StdCmdAction
             -> Bool
             -> Bool
             -> O.Verbosity
             -> Bool
             -> O.WithPatchIndex
             -> ()
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
initOpts = initBasicOpts `withStdOpts` initAdvancedOpts

initialize :: DarcsCommand [DarcsFlag]
initialize = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "initialize"
    , commandHelp = initializeHelp
    , commandDescription = initializeDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DIRECTORY>]"]
    , commandPrereq = \_ -> return $ Right ()
    , commandCommand = initializeCmd
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc initAdvancedOpts
    , commandBasicOptions = odesc initBasicOpts
    , commandDefaults = defaultFlags initOpts
    , commandCheckOptions = ocheck initOpts
    , commandParseOptions = onormalise initOpts
    }

initializeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
initializeCmd aps opts [outname] | null [ () | WorkRepoDir _ <- opts ] =
  initializeCmd aps (WorkRepoDir outname:opts) []
initializeCmd _ opts [] = do
  location <- amNotInRepository opts
  case location of
    Left msg -> fail $ "Unable to " ++ formatPath ("darcs " ++ commandName initialize)
                        ++ " here.\n\n" ++ msg
    Right () -> do
      createRepository (patchFormat opts == O.PatchFormat1) (withWorkingDir opts) (runPatchIndex opts)
      putInfo opts $ text "Repository initialized."
initializeCmd _ _ _ = fail "You must provide 'initialize' with either zero or one argument."
