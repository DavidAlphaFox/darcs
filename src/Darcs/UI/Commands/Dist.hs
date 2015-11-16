--  Copyright (C) 2003 David Roundy
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

-- |
-- Module      : Darcs.UI.Commands.Dist
-- Copyright   : 2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Dist
    (
      dist
    , doFastZip -- libdarcs export
    , doFastZip'
    ) where

import Prelude hiding ( (^), writeFile )

import Data.ByteString.Lazy ( writeFile )
import Data.Char ( isAlphaNum )
import Control.Monad ( when )
import System.Directory ( setCurrentDirectory )
import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath.Posix ( takeFileName, (</>) )

import Darcs.Util.Workaround ( getCurrentDirectory )
import Codec.Archive.Tar ( pack, write )
import Codec.Archive.Tar.Entry ( entryPath )
import Codec.Compression.GZip ( compress )

import Codec.Archive.Zip ( emptyArchive, fromArchive, addEntryToArchive, toEntry )
import Darcs.Repository.External ( fetchFilePS, Cachable( Uncachable ) )
import Darcs.Util.Global ( darcsdir )
import Darcs.Repository.HashedRepo ( inv2pris )
import Darcs.Repository.HashedIO ( pathsAndContents )
import Darcs.Repository.InternalTypes ( Repository (..) )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Darcs.UI.Flags
    ( DarcsFlag(Verbose, Quiet, DistName, DistZip, SetScriptsExecutable), useCache )
import Darcs.UI.Options
    ( DarcsOption, (^), oid, odesc, ocheck, onormalise
    , defaultFlags, parseFlags
    )
import qualified Darcs.UI.Options.All as O

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Patch.Match
    ( haveNonrangeMatch
    , firstMatch
    )
import Darcs.Repository.Match
    ( getFirstMatch
    , getNonrangeMatch
    )
import Darcs.Repository ( withRepository, withRepositoryDirectory, RepoJob(..),
                          setScriptsExecutable, repoPatchType,
                          createPartialsPristineDirectoryTree )
import Darcs.Repository.Prefs ( getPrefval )

import Darcs.Util.DateTime ( getCurrentTime, toSeconds )
import Darcs.Util.Path ( AbsolutePath, toFilePath )
import Darcs.Util.File ( withCurrentDirectory )


distDescription :: String
distDescription = "Create a distribution archive."


distHelp :: String
distHelp = unlines
  [ "`darcs dist` creates a compressed archive in the repository's root"
  , "directory, containing the recorded state of the working tree"
  , "(unrecorded changes and the `_darcs` directory are excluded)."
  , "The command accepts matchers to create an archive of some past"
  , "repository state, for instance `--tag`."
  , ""
  , "By default, the archive (and the top-level directory within the"
  , "archive) has the same name as the repository, but this can be"
  , "overridden with the `--dist-name` option."
  , ""
  , "If a predist command is set (see `darcs setpref`), that command will"
  , "be run on the recorded state prior to archiving.  For example,"
  , "autotools projects would set it to `autoconf && automake`."
  , ""
  , "If `--zip` is used, matchers and the predist command are ignored."
  ]

distBasicOpts :: DarcsOption a
                 (Maybe String
                  -> Bool
                  -> Maybe String
                  -> [O.MatchFlag]
                  -> O.SetScriptsExecutable
                  -> Bool
                  -> a)
distBasicOpts
    = O.distname
    ^ O.distzip
    ^ O.workingRepoDir
    ^ O.matchOne
    ^ O.setScriptsExecutable
    ^ O.storeInMemory

distOpts :: DarcsOption a
            (Maybe String
             -> Bool
             -> Maybe String
             -> [O.MatchFlag]
             -> O.SetScriptsExecutable
             -> Bool
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
distOpts = distBasicOpts `withStdOpts` oid

dist :: DarcsCommand [DarcsFlag]
dist = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "dist"
    , commandHelp = distHelp
    , commandDescription = distDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = distCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc distBasicOpts
    , commandDefaults = defaultFlags distOpts
    , commandCheckOptions = ocheck distOpts
    , commandParseOptions = onormalise distOpts
    }

distCmd :: (AbsolutePath, AbsolutePath)
        -> [DarcsFlag]
        -> [String]
        -> IO ()
distCmd _ opts _ | DistZip `elem` opts = doFastZip opts
distCmd _ opts _ = withRepository (useCache opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchOne opts
  formerdir <- getCurrentDirectory
  let distname = getDistName formerdir [x | DistName x <- opts]
  predist <- getPrefval "predist"
  let resultfile = formerdir </> distname ++ ".tar.gz"
  withTempDir "darcsdist" $ \tempdir -> do
    setCurrentDirectory formerdir
    withTempDir (toFilePath tempdir </> takeFileName distname) $ \ddir -> do
      if haveNonrangeMatch (repoPatchType repository) matchFlags
        then
            if firstMatch matchFlags
                then withCurrentDirectory ddir $ getFirstMatch repository matchFlags
                else withCurrentDirectory ddir $ getNonrangeMatch repository matchFlags
        else createPartialsPristineDirectoryTree repository [""] (toFilePath ddir)
      ec <- case predist of Nothing -> return ExitSuccess
                            Just pd -> system pd
      if ec == ExitSuccess
          then
              do
              withCurrentDirectory ddir $
                  when (SetScriptsExecutable `elem` opts) setScriptsExecutable
              doDist opts tempdir ddir resultfile
          else
              do
              putStrLn "Dist aborted due to predist failure"
              exitWith ec


-- | This function performs the actual distribution action itself.
-- NB - it does /not/ perform the pre-dist, that should already
-- have completed successfully before this is invoked.
doDist :: [DarcsFlag] -> AbsolutePath -> AbsolutePath -> FilePath -> IO ()
doDist opts tempdir ddir resultfile = do
    setCurrentDirectory (toFilePath tempdir)
    let safeddir = safename $ takeFileName $ toFilePath ddir
    entries <- pack "." [safeddir]
    when (Verbose `elem` opts) $ putStr $ unlines $ map entryPath entries
    writeFile resultfile $ compress $ write entries
    when (Quiet `notElem` opts) $ putStrLn $ "Created dist as " ++ resultfile
  where
    safename n@(c:_) | isAlphaNum c  = n
    safename n = "./" ++ n


getDistName :: FilePath -> [String] -> FilePath
getDistName _ (dn:_) = dn
getDistName currentDirectory _ = takeFileName currentDirectory

doFastZip :: [DarcsFlag]
          -> IO ()
doFastZip opts = do
  currentdir <- getCurrentDirectory
  let distname = getDistName currentdir [x | DistName x <- opts]  
  let resultfile = currentdir </> distname ++ ".zip"
  doFastZip' opts currentdir (writeFile resultfile)
  when (Quiet `notElem` opts) $ putStrLn $ "Created " ++ resultfile

doFastZip' :: [DarcsFlag]              -- ^ Flags/options
           -> FilePath                 -- ^ The path to the repository
           -> (BL.ByteString -> IO a)  -- ^ An action to perform on the archive contents
           -> IO a
doFastZip' opts path act = withRepositoryDirectory (useCache opts) path $ RepoJob $ \(Repo _ _ _ c) -> do
  when (SetScriptsExecutable `elem` opts) $
    putStrLn "WARNING: Zip archives cannot store executable flag."  
  let distname = getDistName path [x | DistName x <- opts]
  i <- fetchFilePS (path </> darcsdir </> "hashed_inventory") Uncachable
  pristine <- pathsAndContents (distname ++ "/") c (inv2pris i)
  epochtime <- toSeconds `fmap` getCurrentTime
  let entries = [ toEntry filepath epochtime (toLazy contents) | (filepath,contents) <- pristine ]
  let archive = foldr addEntryToArchive emptyArchive entries
  act (fromArchive archive)


toLazy :: B.ByteString -> BL.ByteString
toLazy bs = BL.fromChunks [bs]
