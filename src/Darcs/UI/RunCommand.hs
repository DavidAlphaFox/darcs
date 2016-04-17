-- Copyright (C) 2002,2003,2005 David Roundy
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
-- | This is the actual heavy lifter code, which is responsible for parsing the
-- arguments and then running the command itself.
module Darcs.UI.RunCommand ( runTheCommand ) where

import Prelude hiding ( (^) )
import Data.Functor ((<$>))
import Data.List ( intercalate )
import Control.Monad ( unless, when )
import System.Console.GetOpt( ArgOrder( Permute, RequireOrder ),
                              OptDescr( Option ),
                              getOpt )
import System.Exit ( ExitCode ( ExitSuccess ), exitWith )

import Darcs.UI.Options ( DarcsOption, (^), odesc, oparse, parseFlags, optDescr )
import Darcs.UI.Options.All
    ( stdCmdActions, StdCmdAction(..)
    , anyVerbosity, verbosity, Verbosity(..), network, NetworkOptions(..)
    , preHook, postHook )

import Darcs.UI.Defaults ( applyDefaults )
import Darcs.UI.External ( viewDoc )
import Darcs.UI.Flags ( DarcsFlag (NewRepo), toMatchFlags, fixRemoteRepos )
import Darcs.UI.Commands
    ( CommandArgs( CommandOnly, SuperCommandOnly, SuperCommandSub )
    , CommandControl
    , DarcsCommand
    , commandName
    , commandCommand
    , commandPrereq
    , commandExtraArgHelp
    , commandExtraArgs
    , commandArgdefaults
    , commandGetArgPossibilities
    , commandOptions
    , commandParseOptions
    , wrappedCommandName
    , disambiguateCommands
    , getCommandHelp
    , getCommandMiniHelp
    , getSubcommands
    , extractCommands
    , superName
    , subusage
    , formatPath
    )
import Darcs.UI.Commands.GZCRCs ( doCRCWarnings )
import Darcs.UI.Commands.Clone ( makeRepoName, cloneToSSH )

import Darcs.Patch.Match ( checkMatchSyntax )
import Darcs.Repository.Prefs ( getGlobal, getPreflist )
import Darcs.Repository.Test ( runPosthook, runPrehook )
import Darcs.Util.AtExit ( atexit )
import Darcs.Util.Download ( setDebugHTTP, disableHTTPPipelining )
import Darcs.Util.Global ( setDebugMode, setTimingsMode )
import Darcs.Util.Path ( AbsolutePath, getCurrentDirectory, toPath, ioAbsoluteOrRemote, makeAbsolute )
import Darcs.Util.Printer ( text )
import Darcs.Util.Progress ( setProgressMode )
import Darcs.Util.Text ( chompTrailingNewline )
-- 执行相应的命令
runTheCommand :: [CommandControl] -> String -> [String] -> IO ()
runTheCommand commandControlList cmd args =
  either fail rtc $ disambiguateCommands commandControlList cmd args
 where
  rtc (CommandOnly c,       as) = runCommand Nothing c as
  rtc (SuperCommandOnly c,  as) = runRawSupercommand c as
  rtc (SuperCommandSub c s, as) = runCommand (Just c) s as

runCommand :: Maybe (DarcsCommand pf1) -> DarcsCommand pf2 -> [String] -> IO ()
runCommand _ _ args -- Check for "dangerous" typoes...
    | "-all" `elem` args = -- -all indicates --all --look-for-adds!
        fail "Are you sure you didn't mean --all rather than -all?"
runCommand msuper cmd args = do
  old_wd <- getCurrentDirectory
  let options = commandOptions old_wd cmd
  case fixupMsgs $ getOpt Permute options args of
    (cmdline_flags,orig_extra,getopt_errs) -> do
      -- FIXME This code is highly order-dependent because of hidden state: the
      -- current directory. Like almost all Repository functions, getGlobal and
      -- getPreflist assume that the cwd is the base of our work repo (if any).
      -- This is supposed to be ensured by commandPrereq. Which means we must
      -- first call commandPrereq, then getGlobal and getPreflist, and then we
      -- must use the (saved) original working directory to resolve possibly
      -- relative paths to absolute paths.
      prereq_errors <- commandPrereq cmd cmdline_flags
      user_defs <- getGlobal   "defaults"
      repo_defs <- getPreflist "defaults"
      let (flags,flag_errors) =
            applyDefaults (fmap commandName msuper) cmd old_wd user_defs repo_defs cmdline_flags
      case parseFlags stdCmdActions flags of
        Just Help -> viewDoc $ text $ getCommandHelp msuper cmd
        Just ListOptions -> do
          setProgressMode False
          file_args <- commandGetArgPossibilities cmd
          putStrLn $ intercalate "\n" $ getOptionsOptions options : file_args
        Just Disable ->
          fail $ "Command "++commandName cmd++" disabled with --disable option!"
        Nothing -> case prereq_errors of
          Left complaint -> fail $
            "Unable to " ++ formatPath ("darcs " ++ superName msuper ++ commandName cmd) ++
            " here.\n\n" ++ complaint
          Right () -> case getopt_errs ++ flag_errors of
            [] -> do
              extra <- commandArgdefaults cmd flags old_wd orig_extra
              case extraArgumentsError extra cmd msuper of
                Nothing     -> runWithHooks cmd old_wd flags extra
                Just msg    -> fail msg
            es -> fail (intercalate "\n" es)

fixupMsgs :: (a, b, [String]) -> (a, b, [String])
fixupMsgs (fs,as,es) = (fs,as,map (("command line: "++).chompTrailingNewline) es)

withHookOpts :: DarcsOption a (t2 -> t3 -> t4 -> t1)
                      -> (t2 -> t3 -> t4 -> t -> t1) -> [DarcsFlag] -> t -> a
withHookOpts opts runHook flags path = oparse opts runHook' flags where
  runHook' mcmd ask verb = runHook mcmd ask verb path

runWithHooks :: DarcsCommand pf
             -> AbsolutePath -> [DarcsFlag] -> [String] -> IO ()
runWithHooks cmd old_wd flags extra = do
   -- NOTE: we must get the cwd again because commandPrereq has the side-effect of changing it.
   new_wd <- getCurrentDirectory
   checkMatchSyntax $ toMatchFlags flags
   -- set any global variables
   oparse (anyVerbosity ^ network) setGlobalVariables flags
   -- actually run the command and its hooks
   preHookExitCode <- withHookOpts (preHook ^ verbosity) runPrehook flags new_wd
   if preHookExitCode /= ExitSuccess
      then exitWith preHookExitCode
      else do fixedFlags <- fixRemoteRepos old_wd flags
              phDir <- getPosthookDir new_wd cmd fixedFlags extra
              let parsedFlags = commandParseOptions cmd fixedFlags
              commandCommand cmd (new_wd, old_wd) parsedFlags extra
              postHookExitCode <- withHookOpts (postHook ^ verbosity) runPosthook flags phDir
              exitWith postHookExitCode

setGlobalVariables :: Bool -> Bool -> Verbosity -> Bool -> NetworkOptions -> IO ()
setGlobalVariables debug debugHttp verb timings net = do
  when timings setTimingsMode
  when debug setDebugMode
  when debugHttp setDebugHTTP
  when (verb == Quiet) $ setProgressMode False
  when (noHttpPipelining net) disableHTTPPipelining
  unless (verb == Quiet) $ atexit $ doCRCWarnings (verb == Verbose)

-- | Returns the working directory for the posthook. For most commands, the
-- first parameter is returned. For the \'get\' command, the path of the newly
-- created repository is returned if it is not an ssh url.
getPosthookDir :: AbsolutePath -> DarcsCommand pf -> [DarcsFlag] -> [String] -> IO AbsolutePath
getPosthookDir new_wd cmd flags extra | commandName cmd `elem` ["get","clone"] = do
    case extra of
      [inrepodir, outname] -> getPosthookDir new_wd cmd (NewRepo outname:flags) [inrepodir]
      [inrepodir] ->
        case cloneToSSH flags of
         Nothing -> do
          repodir <- toPath <$> ioAbsoluteOrRemote inrepodir
          reponame <- makeRepoName False flags repodir
          return $ makeAbsolute new_wd reponame
         _ -> return new_wd
      _ -> fail "You must provide 'clone' with either one or two arguments."
getPosthookDir new_wd _ _ _ = return new_wd


-- | Checks if the number of extra arguments matches the number of extra
-- arguments supported by the command as specified in `commandExtraArgs`.
-- Extra arguments are arguments that follow the command but aren't
-- considered a flag. In `darcs push xyz`, xyz would be an extra argument.
extraArgumentsError :: [String]             -- extra commands provided by user
                    -> DarcsCommand pf1
                    -> Maybe (DarcsCommand pf2)
                    -> Maybe String
extraArgumentsError extra cmd msuper
    | extraArgsCmd < 0 = Nothing
    | extraArgsInput > extraArgsCmd = Just badArg
    | extraArgsInput < extraArgsCmd = Just missingArg
    | otherwise = Nothing
        where
            extraArgsInput = length extra
            extraArgsCmd = commandExtraArgs cmd
            badArg     = "Bad argument: `" ++ unwords extra ++
                         "'\n" ++ getCommandMiniHelp msuper cmd
            missingArg = "Missing argument:  " ++ nthArg (length extra + 1) ++
                         "\n" ++ getCommandMiniHelp msuper cmd
            nthArg n       = nthOf n (commandExtraArgHelp cmd)
            nthOf 1 (h:_)  = h
            nthOf n (_:hs) = nthOf (n-1) hs
            nthOf _ []     = "UNDOCUMENTED"

getOptionsOptions :: [OptDescr DarcsFlag] -> String
getOptionsOptions = intercalate "\n" . concatMap goo
 where
  goo (Option _ os _ _) = map ("--"++) os

runRawSupercommand :: DarcsCommand pf -> [String] -> IO ()
runRawSupercommand super [] =
    fail $ "Command '"++ commandName super ++"' requires a subcommand!\n\n"
             ++ subusage super
runRawSupercommand super args = do
  cwd <- getCurrentDirectory
  case fixupMsgs $ getOpt RequireOrder (map (optDescr cwd) (odesc stdCmdActions)) args of
    -- note: we do not apply defaults here
    (flags,_,getopt_errs) -> case parseFlags stdCmdActions flags of
      Just Help ->
        viewDoc $ text $ getCommandHelp Nothing super
      Just ListOptions -> do
        putStrLn "--help"
        mapM_ (putStrLn . wrappedCommandName) (extractCommands $ getSubcommands super)
      Just Disable -> do
        fail $ "Command " ++ commandName super ++
               " disabled with --disable option!"
      Nothing -> fail $ case getopt_errs of
        [] -> "Invalid subcommand!\n\n" ++ subusage super
        _ -> intercalate "\n" getopt_errs
