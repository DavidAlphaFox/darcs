--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.UI.Commands.Clone
    ( get
    , put
    , clone
    , makeRepoName
    , cloneToSSH
    ) where

import Prelude hiding ( (^), catch )

import System.Directory ( doesDirectoryExist, doesFileExist
                        , setCurrentDirectory )
import System.Exit ( ExitCode(..) )
import Control.Exception ( catch, SomeException )
import Control.Monad ( when, unless )
import Data.Maybe ( listToMaybe )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts
                      , nodefaults
                      , commandStub
                      , commandAlias
                      , putInfo
                      )
import Darcs.UI.Flags( DarcsFlag( NewRepo
                                  , UpToPattern
                                  , UpToPatch
                                  , OnePattern
                                  , OnePatch
                                  )
                     , toMatchFlags, useCache, umask, remoteRepos
                     , setDefault , DarcsFlag(Quiet), usePacks
                     , remoteDarcs, cloneKind, verbosity, setScriptsExecutable
                     , withWorkingDir, runPatchIndex )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands.Util ( getUniqueRepositoryName )
import Darcs.Repository ( cloneRepository )
import Darcs.Repository.Format ( identifyRepoFormat
                               , RepoProperty ( HashedInventory
                                              , RebaseInProgress
                                              )
                               , formatHas
                               )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Repository.Ssh ( getSSH, SSHCmd(SCP) )
import Darcs.Repository.Flags
    ( CloneKind(CompleteClone), SetDefault(NoSetDefault), ForgetParent(..) )
import Darcs.Patch.Bundle ( scanContextFile )
import Darcs.Patch.Dummy ( DummyPatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Repository.Motd ( showMotd )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Printer ( text, ($$) )
import Darcs.Util.Path ( toFilePath, toPath, ioAbsoluteOrRemote, AbsolutePath )
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.Util.URL ( isSshUrl )
import Darcs.Util.Exec ( exec, Redirect(..), )

cloneDescription :: String
cloneDescription = "Make a copy of an existing repository."

cloneHelp :: String
cloneHelp =
 unlines
  [ "Clone creates a copy of a repository.  The optional second"
  , "argument specifies a destination directory for the new copy;"
  , "if omitted, it is inferred from the source location."
  , ""
  , "By default Darcs will copy every patch from the original repository."
  , "This means the copy is completely independent of the original; you can"
  , "operate on the new repository even when the original is inaccessible."
  , "If you expect the original repository to remain accessible, you can"
  , "use `--lazy` to avoid copying patches until they are needed (`copy on"
  , "demand').  This is particularly useful when copying a remote"
  , "repository with a long history that you don't care about."
  , ""
  , "When cloning locally, Darcs automatically uses hard linking where"
  , "possible.  As well as saving time and space, this enables to move or"
  , "delete the original repository without affecting the copy."
  , "Hard linking requires that the copy be on the same filesystem as the"
  , "original repository, and that the filesystem support hard linking."
  , "This includes NTFS, HFS+ and all general-purpose Unix filesystems"
  , "(such as ext, UFS and ZFS). FAT does not support hard links."
  , ""
  , "When cloning from a remote location, Darcs will look for and attempt"
  , "to use packs created by `darcs optimize http` in the remote repository."
  , "Packs are single big files that can be downloaded instead of many"
  , "little files, which makes cloning faster over HTTP."
  , ""
  , "Darcs clone will not copy unrecorded changes to the source repository's"
  , "working tree."
  , ""
  , "You can copy a repository to a ssh url, in which case the new repository"
  , "will always be complete."
  , ""
  ] ++ cloneHelpTag
    ++ cloneHelpSSE

cloneBasicOpts :: DarcsOption a
                  (Maybe String
                   -> CloneKind
                   -> [O.MatchFlag]
                   -> Maybe Bool
                   -> O.SetScriptsExecutable
                   -> O.WithWorkingDir
                   -> a)
cloneBasicOpts = O.reponame
               ^ O.partial
               ^ O.matchOneContext
               ^ O.setDefault
               ^ O.setScriptsExecutable
               ^ O.useWorkingDir

cloneAdvancedOpts :: DarcsOption a (Bool -> O.WithPatchIndex -> O.NetworkOptions -> a)
cloneAdvancedOpts = O.usePacks ^ O.patchIndex ^ O.network

cloneOpts :: DarcsOption a
             (Maybe String
              -> CloneKind
              -> [O.MatchFlag]
              -> Maybe Bool
              -> O.SetScriptsExecutable
              -> O.WithWorkingDir
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> Bool
              -> O.WithPatchIndex
              -> O.NetworkOptions
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)
cloneOpts = cloneBasicOpts `withStdOpts` cloneAdvancedOpts

clone :: DarcsCommand [DarcsFlag]
clone = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "clone"
    , commandHelp = cloneHelp
    , commandDescription = cloneDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<REPOSITORY>", "[<DIRECTORY>]"]
    , commandCommand = cloneCmd
    , commandPrereq = validContextFile
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc cloneAdvancedOpts
    , commandBasicOptions = odesc cloneBasicOpts
    , commandDefaults = defaultFlags cloneOpts
    , commandCheckOptions = ocheck cloneOpts
    , commandParseOptions = onormalise cloneOpts
    }

get :: DarcsCommand [DarcsFlag]
get = commandAlias "get" Nothing clone

putDescription :: String
putDescription = "Deprecated command, replaced by clone."

putHelp :: String
putHelp = unlines
 [ "This command is deprecated."
 , ""
 , "To clone the current repository to a ssh destination,"
 , "use the syntax `darcs clone . user@server:path` ."
 ]

put :: DarcsCommand [DarcsFlag]
put = commandStub "put" putHelp putDescription clone

cloneCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
cloneCmd fps opts [inrepodir, outname] = cloneCmd fps (NewRepo outname:opts) [inrepodir]
cloneCmd _ opts [inrepodir] = do
  debugMessage "Starting work on clone..."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir
  unless (Quiet `elem` opts) $ showMotd repodir
  rfsource <- identifyRepoFormat repodir
  debugMessage $ "Found the format of "++repodir++"..."

  -- there's no fundamental reason for banning gets of repositories with
  -- rebase in progress, but it seems a bit dubious to actually copy the
  -- rebase state, and removing it is a bit of work since the current
  -- implementation just copies the inventory file
  when (formatHas RebaseInProgress rfsource) $
      fail "Can't clone a repository with a rebase in progress"

  unless (formatHas HashedInventory rfsource) $ putInfo opts $
       text "***********************************************************************"
    $$ text "  _______   Sorry for the wait! The repository you are cloning is"
    $$ text " |       |  using the DEPRECATED 'old-fashioned' format. I'm doing a"
    $$ text " | O   O |  hashed copy instead, but this may take a while."
    $$ text " |  ___  |"
    $$ text " | /   \\ |  We recommend that the maintainer upgrade the remote copy"
    $$ text " |_______|  as well. See http://wiki.darcs.net/OF for more information."
    $$ text ""
    $$ text "***********************************************************************"

  case cloneToSSH opts of
    Just repo -> do
      withTempDir "clone" $ \_ -> do
         putInfo opts $ text "Creating local clone..."
         currentDir <- getCurrentDirectory
         cloneRepository repodir "local" (verbosity opts) (useCache opts)
                         CompleteClone (umask opts) (remoteDarcs opts)
                         (setScriptsExecutable opts)
                         (remoteRepos opts) (NoSetDefault True)
                         (toMatchFlags $ map convertUpToToOne opts)
                         rfsource
                         (withWorkingDir opts)
                         (runPatchIndex opts)
                         (usePacks opts)
                         (not $ null [p | UpToPattern p <- opts] ) -- --to-match given
                         YesForgetParent
         setCurrentDirectory currentDir
         (scp, args) <- getSSH SCP
         putInfo opts $ text "Transferring clone by SCP..."
         r <- exec scp (args ++ ["-r", "local", repo]) (AsIs,AsIs,AsIs)
         when (r /= ExitSuccess) $ fail $ "Problem during SCP transfer."
         putInfo opts $ text "Cloning and transferring successful."
    Nothing -> do
      mysimplename <- makeRepoName True opts repodir
      cloneRepository repodir mysimplename (verbosity opts) (useCache opts)
                  (cloneKind opts) (umask opts) (remoteDarcs opts)
                  (setScriptsExecutable opts)
                  (remoteRepos opts) (setDefault True opts)
                  (toMatchFlags $ map convertUpToToOne opts)
                  rfsource
                  (withWorkingDir opts)
                  (runPatchIndex opts)
                  (usePacks opts)
                  (not $ null [p | UpToPattern p <- opts] ) -- --to-match given
                  NoForgetParent
      putInfo opts $ text "Finished cloning."

cloneCmd _ _ _ = fail "You must provide 'clone' with either one or two arguments."

cloneToSSH :: [DarcsFlag] -> Maybe String
cloneToSSH fs = case parseFlags O.reponame fs of
  Nothing -> Nothing
  Just r -> if isSshUrl r then Just r else Nothing

makeRepoName :: Bool -> [DarcsFlag] -> FilePath -> IO String
makeRepoName talkative dfs d =
 case [ n | NewRepo n <- dfs] of
  (n:_) ->
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
  [] ->
    case dropWhile (=='.') $ reverse $
         takeWhile (\c -> c /= '/' && c /= ':') $
         dropWhile (=='/') $ reverse d of
      ""   -> getUniqueRepositoryName talkative "anonymous_repo"
      base@('/':_) -> getUniqueRepositoryName talkative base -- Absolute
      base -> do -- Relative
              cwd <- getCurrentDirectory
              getUniqueRepositoryName talkative (cwd ++ "/" ++ base)

cloneHelpTag :: String
cloneHelpTag =
 unlines
  [ "It is often desirable to make a copy of a repository that excludes"
  , "some patches.  For example, if releases are tagged then `darcs clone"
  , "--tag .` would make a copy of the repository as at the latest release."
  , ""
  , "An untagged repository state can still be identified unambiguously by"
  , "a context file, as generated by `darcs log --context`.  Given the"
  , "name of such a file, the `--context` option will create a repository"
  , "that includes only the patches from that context.  When a user reports"
  , "a bug in an unreleased version of your project, the recommended way to"
  , "find out exactly what version they were running is to have them" 
  , "include a context file in the bug report."
  , ""
  , "You can also make a copy of an untagged state using the `--to-patch` or"
  , "`--to-match` options, which exclude patches *after* the first matching"
  , "patch.  Because these options treat the set of patches as an ordered"
  , "sequence, you may get different results after reordering with `darcs"
  , "optimize`, so tagging is preferred."
  , ""
  ]

cloneHelpSSE :: String
cloneHelpSSE =
    unlines
    [ "The `--set-scripts-executable` option causes scripts to be made"
    , "executable in the working tree. A script is any file that starts"
    , "with a shebang (\"#!\")."
    ]

validContextFile :: [DarcsFlag] -> IO (Either String ())
validContextFile opts =
   case getContext opts of
     Nothing -> return $ Right ()
     Just ctxAbsolutePath -> do
         let ctxFilePath = toFilePath ctxAbsolutePath
         exists <- doesFileExist ctxFilePath
         if exists
             then do
                (ps :: PatchSet DummyPatch Origin wX) <-
                    scanContextFile ctxFilePath
                (ps `seq` return $ Right ()) `catch` \(_ :: SomeException) ->
                    return . Left $ "File " ++ ctxFilePath
                                    ++ " is not a valid context file"
             else return . Left $
                 "Context file " ++ ctxFilePath ++ " does not exist"

-- | 'getContext' takes a list of flags and returns the context
-- specified by @Context c@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--context=FILE@
getContext :: [DarcsFlag] -> Maybe AbsolutePath
getContext fs = listToMaybe [ f | O.Context f <- toMatchFlags fs ]

-- The 'clone' command takes --to-patch and --to-match as arguments,
-- but internally wants to handle them as if they were --patch and --match
-- TODO: remove this when we get rid of directly looking at [DarcsFlag]
-- for this command.
convertUpToToOne :: DarcsFlag -> DarcsFlag
convertUpToToOne (UpToPattern p) = OnePattern p
convertUpToToOne (UpToPatch p) = OnePatch p
convertUpToToOne f = f
