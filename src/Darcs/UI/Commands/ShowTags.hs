--  Copyright (C) 2007 Florian Weimer
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

module Darcs.UI.Commands.ShowTags
    ( showTags
    ) where

import Control.Monad ( unless, join )
import Data.Maybe ( fromMaybe )
import System.IO ( stderr, hPutStrLn )

import Darcs.Patch.Set ( PatchSet(..) )
import Darcs.Patch.MaybeInternal ( MaybeInternal )
import Darcs.Repository ( readRepo, withRepositoryDirectory, RepoJob(..) )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Commands.Tag ( getTags )
import Darcs.UI.Flags ( DarcsFlag, useCache, getRepourl )
import Darcs.UI.Options
    ( DarcsOption, PrimDarcsOption
    , oid, odesc, ocheck, onormalise, defaultFlags
    )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Text ( formatText )
import Darcs.Util.Path ( AbsolutePath )

showTagsDescription :: String
showTagsDescription = "Show all tags in the repository."

showTagsHelp :: String
showTagsHelp = formatText 80
    [ "The tags command writes a list of all tags in the repository to "
      ++ "standard output."
    , "Tab characters (ASCII character 9) in tag names are changed to spaces "
      ++ "for better interoperability with shell tools. A warning is printed "
      ++ "if this happens."
    ]

showTagsBasicOpts :: PrimDarcsOption (Maybe String)
showTagsBasicOpts = O.possiblyRemoteRepo

showTagsOpts :: DarcsOption a
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
showTagsOpts = showTagsBasicOpts `withStdOpts` oid

showTags :: DarcsCommand [DarcsFlag]
showTags = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tags"
    , commandHelp = showTagsHelp
    , commandDescription = showTagsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = tagsCmd
    , commandPrereq = findRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showTagsBasicOpts
    , commandDefaults = defaultFlags showTagsOpts
    , commandCheckOptions = ocheck showTagsOpts
    , commandParseOptions = onormalise showTagsOpts
    }

tagsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagsCmd _ opts _ = let repodir = fromMaybe "." (getRepourl opts) in
    withRepositoryDirectory (useCache opts) repodir $ RepoJob $ \repo ->
        readRepo repo >>= printTags
  where
    printTags :: MaybeInternal p => PatchSet p wW wZ -> IO ()
    printTags = join . fmap (sequence_ . map process) . getTags
    process :: String -> IO ()
    process t = normalize t t False >>= putStrLn
    normalize :: String -> String -> Bool -> IO String
    normalize _ [] _ = return []
    normalize t (x : xs) flag =
        if x == '\t' then do
            unless flag $
                hPutStrLn stderr $ "warning: tag with TAB character: " ++ t
            rest <- normalize t xs True
            return $ ' ' : rest
        else do
            rest <- normalize t xs flag
            return $ x : rest
