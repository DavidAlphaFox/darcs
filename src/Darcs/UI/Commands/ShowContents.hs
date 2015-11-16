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

module Darcs.UI.Commands.ShowContents ( showContents ) where

import Prelude hiding ( (^) )

import Control.Monad ( filterM, forM_, forM )
import System.IO ( stdout )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Flags ( DarcsFlag, useCache, fixSubPaths )
import Darcs.UI.Options ( DarcsOption, (^), oid, odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
    ( MatchFlag
    , matchOne
    , workingRepoDir
    , StdCmdAction
    , Verbosity
    , UseCache )
import Darcs.Patch.Match ( haveNonrangeMatch )
import Darcs.Repository ( withRepository, RepoJob(..), readRecorded, repoPatchType )
import Darcs.Repository.Lock ( withDelayedDir )
import Darcs.Repository.Match ( getNonrangeMatch )
import Storage.Hashed.Plain( readPlainTree )
import qualified Storage.Hashed.Monad as HSM
import Darcs.Util.Path( floatPath, sp2fn, toFilePath, AbsolutePath )

showContentsDescription :: String
showContentsDescription = "Outputs a specific version of a file."

showContentsHelp :: String
showContentsHelp =
  "Show contents can be used to display an earlier version of some file(s).\n"++
  "If you give show contents no version arguments, it displays the recorded\n"++
  "version of the file(s).\n"

showContentsBasicOpts :: DarcsOption a ([O.MatchFlag] -> Maybe String -> a)
showContentsBasicOpts = O.matchOne ^ O.workingRepoDir

showContentsOpts :: DarcsOption a
                    ([O.MatchFlag]
                     -> Maybe String
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
showContentsOpts = O.matchOne ^ O.workingRepoDir `withStdOpts` oid

showContents :: DarcsCommand [DarcsFlag]
showContents = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "contents"
    , commandHelp = showContentsHelp
    , commandDescription = showContentsDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE]..."]
    , commandCommand = showContentsCmd
    , commandPrereq = findRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showContentsBasicOpts
    , commandDefaults = defaultFlags showContentsOpts
    , commandCheckOptions = ocheck showContentsOpts
    , commandParseOptions = onormalise showContentsOpts
    }

showContentsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showContentsCmd _ _ [] = fail "show contents needs at least one argument."
showContentsCmd fps opts args = do
  floatedPaths <- map (floatPath . toFilePath . sp2fn) `fmap` fixSubPaths fps args
  let matchFlags = parseFlags O.matchOne opts
  withRepository (useCache opts) $ RepoJob $ \repository -> do
    let readContents = do
          okpaths <- filterM HSM.fileExists floatedPaths
          forM okpaths $ \f -> (B.concat . BL.toChunks) `fmap` HSM.readFile f
        -- Note: The two calls to execReadContents below are from
        -- different working directories. This matters despite our
        -- use of virtualTreeIO.
        execReadContents tree = fst `fmap` HSM.virtualTreeIO readContents tree
    files <- if haveNonrangeMatch (repoPatchType repository) matchFlags then
               withDelayedDir "show.contents" $ \_ -> do
                 -- this call populates our temporary directory, but note that
                 -- it does so lazily: the tree gets (partly) expanded inside
                 -- execReadContents, so it is important that we execute the
                 -- latter from the same working directory.
                 getNonrangeMatch repository matchFlags
                 readPlainTree "." >>= execReadContents
             else do
               -- we can use the existing pristine tree because we don't modify
               -- anything in this case
               readRecorded repository >>= execReadContents
    forM_ files $ B.hPut stdout
