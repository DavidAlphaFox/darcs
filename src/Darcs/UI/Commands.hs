--  Copyright (C) 2002,2003,2005 David Roundy
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
module Darcs.UI.Commands
    ( CommandControl ( CommandData, HiddenCommand, GroupName )
    , DarcsCommand ( .. )
    , WrappedCommand(..)
    , wrappedCommandName
    , commandAlias
    , commandStub
    , commandOptions
    , commandAlloptions
    , withStdOpts
    , disambiguateCommands
    , CommandArgs(..)
    , getCommandHelp
    , getCommandMiniHelp
    , getSubcommands
    , usage
    , usageHelper
    , subusage
    , extractCommands
    , extractAllCommands
    , normalCommand
    , hiddenCommand
    , commandGroup
    , superName
    , nodefaults
    , putInfo
    , putVerbose
    , putWarning
    , putVerboseWarning
    , abortRun
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , setEnvDarcsFiles
    , formatPath
    , defaultRepo
    , amInHashedRepository
    , amInRepository
    , amNotInRepository
    , findRepository
    ) where

import Prelude hiding ( (^) )
import Control.Monad ( when, unless )
import Data.List ( sort, isPrefixOf )
import Data.Maybe ( catMaybes )
import Storage.Hashed.Tree ( Tree )
import System.Console.GetOpt ( OptDescr )
import System.Exit ( exitSuccess )
import System.IO ( stderr )
#ifndef WIN32
import System.Posix.Env ( setEnv )

import Darcs.Patch ( listTouchedFiles )
import qualified Darcs.Patch ( summary )
#endif
import Darcs.Patch ( RepoPatch, xmlSummary, Patchy )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Info ( toXml )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, hopefullyM )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )

import qualified Darcs.Repository as R ( amInHashedRepository, amInRepository
                                       , amNotInRepository, findRepository )
import Darcs.Repository.Prefs ( defaultrepo )

import Darcs.UI.Options ( DarcsOption, DarcsOptDescr, (^), optDescr, odesc, parseFlags )
import Darcs.UI.Options.All
    ( StdCmdAction, stdCmdActions, anyVerbosity, UseCache, useCache, hooks
    , Verbosity(..), verbosity, Summary(..), DryRun(..), dryRun, XmlOutput(..) )

import Darcs.UI.Flags ( remoteRepos, workRepo, DarcsFlag )
import Darcs.UI.PrintPatch ( showFriendly )
import Darcs.UI.Usage ( usageInfo )

import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, putDocLn, hPutDocLn, text, (<+>), errorDoc
                          , vsep, insertBeforeLastline, prefix, ($$), vcat
#ifndef WIN32
                          , renderString
#endif
                          , RenderMode(..)
                          )
#ifndef WIN32
import Darcs.Util.Progress ( beginTedious, endTedious, tediousSize
                           , finishedOneIO )
#endif
import Darcs.Util.Text ( chompTrailingNewline )

extractCommands :: [CommandControl] -> [WrappedCommand]
extractCommands ccl = [ cmd | CommandData cmd <- ccl ]

extractHiddenCommands :: [CommandControl] -> [WrappedCommand]
extractHiddenCommands ccl = [ cmd | HiddenCommand cmd <- ccl ]

extractAllCommands :: [CommandControl] -> [WrappedCommand]
extractAllCommands ccl = concatMap flatten (extractCommands ccl ++ extractHiddenCommands ccl)
    where flatten c@(WrappedCommand (DarcsCommand {})) = [c]
          flatten c@(WrappedCommand (SuperCommand { commandSubCommands = scs })) = c : extractAllCommands scs

-- |A 'WrappedCommand' is a 'DarcsCommand' where the options type has been hidden
data WrappedCommand where
    WrappedCommand :: DarcsCommand parsedFlags -> WrappedCommand

normalCommand :: DarcsCommand parsedFlags -> CommandControl
normalCommand c = CommandData (WrappedCommand c)

hiddenCommand :: DarcsCommand parsedFlags -> CommandControl
hiddenCommand c = HiddenCommand (WrappedCommand c)

commandGroup :: String -> CommandControl
commandGroup = GroupName

wrappedCommandName :: WrappedCommand -> String
wrappedCommandName (WrappedCommand c) = commandName c

wrappedCommandDescription :: WrappedCommand -> String
wrappedCommandDescription (WrappedCommand c) = commandDescription c

data CommandControl
  = CommandData WrappedCommand
  | HiddenCommand WrappedCommand
  | GroupName String

-- |A 'DarcsCommand' represents a command like add, record etc.
-- The 'parsedFlags' type represents the options that are
-- passed to the command's implementation
data DarcsCommand parsedFlags =
      DarcsCommand
          { commandProgramName -- programs that use libdarcs can change the name here
          , commandName
          , commandHelp
          , commandDescription :: String
          , commandExtraArgs :: Int
          , commandExtraArgHelp :: [String]
          , commandCommand :: -- First 'AbsolutePath' is the repository path,
                              -- second one is the path where darcs was executed.
                              (AbsolutePath, AbsolutePath)
                           -> parsedFlags -> [String] -> IO ()
          , commandPrereq :: [DarcsFlag] -> IO (Either String ())
          , commandGetArgPossibilities :: IO [String]
          , commandArgdefaults :: [DarcsFlag] -> AbsolutePath -> [String]
                               -> IO [String]
          , commandBasicOptions :: [DarcsOptDescr DarcsFlag]
          , commandAdvancedOptions :: [DarcsOptDescr DarcsFlag]
          , commandDefaults :: [DarcsFlag]
          , commandCheckOptions :: [DarcsFlag] -> [String]
          , commandParseOptions :: [DarcsFlag] -> parsedFlags
          }
    | SuperCommand
          { commandProgramName
          , commandName
          , commandHelp
          , commandDescription :: String
          , commandPrereq :: [DarcsFlag] -> IO (Either String ())
          , commandSubCommands :: [CommandControl]
          }

withStdOpts :: DarcsOption (Maybe StdCmdAction -> Bool -> Bool -> Verbosity -> Bool -> b) c
            -> DarcsOption (UseCache -> Maybe String -> Bool -> Maybe String -> Bool -> a) b
            -> DarcsOption a c
withStdOpts basicOpts advancedOpts =
  basicOpts ^ stdCmdActions ^ anyVerbosity ^ advancedOpts ^ useCache ^ hooks

commandAlloptions :: DarcsCommand pf -> ([DarcsOptDescr DarcsFlag], [DarcsOptDescr DarcsFlag])
commandAlloptions DarcsCommand { commandBasicOptions = opts1
                               , commandAdvancedOptions = opts2 } =
    ( opts1 ++ odesc stdCmdActions
    , odesc anyVerbosity ++ opts2 ++ odesc useCache ++ odesc hooks )
commandAlloptions SuperCommand { } = (odesc stdCmdActions, [])

--  Obtain options suitable as input to System.Console.Getopt, including the
--  --disable option (which is not listed explicitly in the DarcsCommand
--  definitions).
commandOptions :: AbsolutePath -> DarcsCommand pf -> [OptDescr DarcsFlag]
commandOptions cwd = map (optDescr cwd) . uncurry (++) . commandAlloptions

nodefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
nodefaults _ _ = return

getSubcommands :: DarcsCommand pf -> [CommandControl]
getSubcommands c@(SuperCommand {}) = commandSubCommands c
getSubcommands _ = []

commandAlias :: String -> Maybe (DarcsCommand pf) -> DarcsCommand pf -> DarcsCommand pf
commandAlias n msuper c =
    c { commandName = n
      , commandDescription = "Alias for `" ++ commandProgramName c ++ " "
                             ++ cmdName ++ "'."
      , commandHelp = "The `" ++ commandProgramName c ++ " " ++ n
                      ++ "' command is an alias for " ++ "`"
                      ++ commandProgramName c ++ " " ++ cmdName ++ "'.\n"
                      ++ commandHelp c
      }
  where
    cmdName = unwords . map commandName . maybe id (:) msuper $ [ c ]

commandStub :: String -> String -> String -> DarcsCommand pf -> DarcsCommand pf
commandStub n h d c = c { commandName = n
                        , commandHelp = h
                        , commandDescription = d
                        , commandCommand = \_ _ _ -> putStr h
                        }

usage :: [CommandControl] -> String
usage cs = unlines
    [ "Usage: darcs COMMAND ..."
    , ""
    , "Commands:"
    , usageHelper cs
    , "Use 'darcs COMMAND --help' for help on a single command."
    , "Use 'darcs --version' to see the darcs version number."
    , "Use 'darcs --exact-version' to see a detailed darcs version."
    , "Use 'darcs help patterns' for help on patch matching."
    , "Use 'darcs help environment' for help on environment variables."
    , "Use 'darcs help manpage' to display help in the manpage format."
    , "Use 'darcs help markdown' to display help in the markdown format."
    , ""
    , "Check bug reports at http://bugs.darcs.net/"
    ]

subusage :: DarcsCommand pf -> String
subusage super = usageInfo header (odesc stdCmdActions) ++ superHelp
  where
    header = unlines [ unwords [ "Usage:"
                               , commandProgramName super
                               , commandName super
                               , "SUBCOMMAND ..."
                               ]
                     , ""
                     , commandDescription super
                     , ""
                     , "Subcommands:"
                     , usageHelper (getSubcommands super)
                     , "Options:"
                     ]
    superHelp = '\n' : commandHelp super

usageHelper :: [CommandControl] -> String
usageHelper xs = usageHelper' (maximum $ 15 : (catMaybes $ map f xs)) xs
   where
         -- returns length of necessary tabbing this command
         f (CommandData c) = Just ((+2) . length . wrappedCommandName $ c)
         f _ = Nothing

usageHelper' :: Int -> [CommandControl] -> String
usageHelper' _ [] = ""
usageHelper' x (HiddenCommand _ : cs) = usageHelper' x cs
usageHelper' x (CommandData c : cs) = "  " ++ padSpaces (wrappedCommandName c) x
                                        ++ chompTrailingNewline (wrappedCommandDescription c)
                                        ++ "\n" ++ usageHelper' x cs
usageHelper' x (GroupName n : cs) = "\n" ++ n ++ "\n" ++ usageHelper' x cs

padSpaces :: String -> Int -> String
padSpaces s n = s ++ replicate (n - length s) ' '

superName :: Maybe (DarcsCommand pf) -> String
superName Nothing  = ""
superName (Just x) = commandName x ++ " "

getCommandMiniHelp :: Maybe (DarcsCommand pf1) -> DarcsCommand pf2 -> String
getCommandMiniHelp msuper cmd = unlines
    [ getCommandHelpCore msuper cmd
    , ""
    , unwords [ "See"
              , commandProgramName cmd
              , "help"
              , maybe "" ((++ " ") . commandName) msuper ++ commandName cmd
              , "for details."
              ]
    ]

getCommandHelp :: Maybe (DarcsCommand pf1) -> DarcsCommand pf2 -> String
getCommandHelp msuper cmd = basicHelp ++ advancedHelp ++ cmdHelp
  where
    basicHelp = unlines (reverse basicR)

    advancedHelp = if null advanced
                       then ""
                       else '\n' :
                            unlines ("Advanced options:" : reverse advancedR)

    cmdHelp =  '\n' : commandHelp cmd

    -- we could just call usageInfo twice, but then the advanced
    -- options might not line up with the basic ones (no short switches)
    (advancedR, basicR) = splitAt (length advanced) . reverse . lines $
                              combinedUsage

    combinedUsage = let header = getCommandHelpCore msuper cmd ++ subcommands
                                     ++ "\n\nOptions:"
                    in usageInfo header (basic ++ advanced)

    (basic, advanced) = commandAlloptions cmd

    subcommands = case msuper of
                      Nothing -> case getSubcommands cmd of
                                     [] -> []
                                     s  -> "\n\nSubcommands:\n"
                                           ++ usageHelper s
                      -- we don't want to list subcommands if we're already
                      -- specifying them
                      Just _  -> ""

getCommandHelpCore :: Maybe (DarcsCommand pf1) -> DarcsCommand pf2 -> String
getCommandHelpCore msuper cmd =
    unwords [ "Usage:"
            , commandProgramName cmd
            , superName msuper ++ commandName cmd
            , "[OPTION]..."
            , unwords args_help
            ]
    ++ "\n" ++ commandDescription cmd
  where
    args_help = case cmd of
                    (DarcsCommand {}) -> commandExtraArgHelp cmd
                    _ -> []

data CommandArgs where
    CommandOnly :: DarcsCommand parsedFlags -> CommandArgs
    SuperCommandOnly :: DarcsCommand parsedFlags -> CommandArgs
    SuperCommandSub :: DarcsCommand parsedFlags1 ->  DarcsCommand parsedFlags2 -> CommandArgs

-- Parses a darcs command line with potentially abbreviated commands
disambiguateCommands :: [CommandControl] -> String -> [String]
                     -> Either String (CommandArgs, [String])
disambiguateCommands allcs cmd args = do
    WrappedCommand c <- extract cmd allcs
    case (getSubcommands c, args) of
        ([], _) -> return (CommandOnly c, args)
        (_, []) -> return (SuperCommandOnly c, args)
        (subcs, a : as) -> case extract a subcs of
                               Left _ -> return (SuperCommandOnly c, args)
                               Right (WrappedCommand sc) -> return (SuperCommandSub c sc, as)

extract :: String -> [CommandControl] -> Either String WrappedCommand
extract cmd cs = case potentials of
    []  -> Left $ "No such command '" ++ cmd ++ "'\n"
    [c] -> Right c
    cs' -> Left $ unlines [ "Ambiguous command..."
                          , ""
                          , "The command '" ++ cmd ++ "' could mean one of:"
                          , unwords . sort . map wrappedCommandName $ cs'
                          ]
  where
    potentials = [c | c <- extractCommands cs, cmd `isPrefixOf` wrappedCommandName c]
                 ++ [h | h <- extractHiddenCommands cs, cmd == wrappedCommandName h]

amVerbose :: [DarcsFlag] -> Bool
amVerbose = (== Verbose) . parseFlags verbosity

amQuiet :: [DarcsFlag] -> Bool
amQuiet = (== Quiet) . parseFlags verbosity

putVerbose :: [DarcsFlag] -> Doc -> IO ()
putVerbose flags = when (amVerbose flags) . putDocLn

putInfo :: [DarcsFlag] -> Doc -> IO ()
putInfo flags = unless (amQuiet flags) . putDocLn

putWarning :: [DarcsFlag] -> Doc -> IO ()
putWarning flags = unless (amQuiet flags) . hPutDocLn Encode stderr

putVerboseWarning :: [DarcsFlag] -> Doc -> IO ()
putVerboseWarning flags = when (amVerbose flags) . hPutDocLn Encode stderr

abortRun :: [DarcsFlag] -> Doc -> IO ()
abortRun flags msg = if parseFlags dryRun flags == YesDryRun
                        then putInfo flags $ text "NOTE:" <+> msg
                        else errorDoc msg

-- | @'printDryRunMessageAndExit' action flags patches@ prints a string
-- representing the action that would be taken if the @--dry-run@ option had
-- not been passed to darcs. Then darcs exits successfully.  @action@ is the
-- name of the action being taken, like @\"push\"@ @flags@ is the list of flags
-- which were sent to darcs @patches@ is the sequence of patches which would be
-- touched by @action@.
printDryRunMessageAndExit :: (RepoPatch p, ApplyState p ~ Tree)
                          => String
                          -> Verbosity -> Summary -> DryRun -> XmlOutput
                          -> Bool -- interactive
                          -> FL (PatchInfoAnd p) wX wY
                          -> IO ()
printDryRunMessageAndExit action v s d x interactive patches = do
    when (d == YesDryRun) $ do
        putInfoX . text $ unwords [ "Would"
                                  , action
                                  , "the following changes:"
                                  ]
        putDocLn put_mode
        putInfoX $ text ""
        putInfoX $ text "Making no changes: this is a dry run."
        exitSuccess
    when (not interactive && s == YesSummary) $ do
        putInfoX . text $ unwords [ "Will"
                                  , action
                                  , "the following changes:"
                                  ]
        putDocLn put_mode
  where
    put_mode = if x == YesXml
                   then text "<patches>" $$
                        vcat (mapFL (indent . xml_info s) patches) $$
                        text "</patches>"
                   else vsep $ mapFL (showFriendly v s) patches

    putInfoX = if x == YesXml then const (return ()) else putDocLn

    xml_info YesSummary = xml_with_summary
    xml_info NoSummary  = toXml . info

    xml_with_summary hp
        | Just p <- hopefullyM hp = insertBeforeLastline (toXml $ info hp)
                                        (indent $ xmlSummary p)
    xml_with_summary hp = toXml (info hp)

    indent = prefix "    "

-- | Set the DARCS_PATCHES and DARCS_PATCHES_XML environment variables with
-- info about the given patches, for use in post-hooks.
setEnvDarcsPatches :: (RepoPatch p, ApplyState p ~ Tree)
                   => FL (PatchInfoAnd p) wX wY -> IO ()
#ifndef WIN32
setEnvDarcsPatches ps = do
    let k = "Defining set of chosen patches"
    beginTedious k
    tediousSize k 3
    finishedOneIO k "DARCS_PATCHES"
    setEnvCautiously "DARCS_PATCHES" (renderString Encode $ Darcs.Patch.summary ps)
    finishedOneIO k "DARCS_PATCHES_XML"
    setEnvCautiously "DARCS_PATCHES_XML" . renderString Encode $
        text "<patches>" $$
        vcat (mapFL (toXml . info) ps) $$
        text "</patches>"
    finishedOneIO k "DARCS_FILES"
    setEnvCautiously "DARCS_FILES" $ unlines (listTouchedFiles ps)
    endTedious k

-- | Set some environment variable to the given value, unless said value is
-- longer than 10K characters, in which case do nothing.
setEnvCautiously :: String -> String -> IO ()
setEnvCautiously e v
    | toobig (10 * 1024) v = return ()
    | otherwise = setEnv e v True
  where
    toobig :: Int -> [a] -> Bool
    toobig 0 _ = True
    toobig _ [] = False
    toobig n (_ : xs) = toobig (n - 1) xs
#else
setEnvDarcsPatches _ = return ()
#endif

-- | Set the DARCS_FILES environment variable to the files touched by the
-- given patch, one per line, for use in post-hooks.
setEnvDarcsFiles :: (PatchInspect p, Patchy p) => p wX wY -> IO ()
#ifndef WIN32
setEnvDarcsFiles ps =
    setEnvCautiously "DARCS_FILES" $ unlines (listTouchedFiles ps)
#else
setEnvDarcsFiles _ = return ()
#endif

-- | Format a path for screen output, so that the user sees where the path
-- begins and ends. Could (should?) also warn about unprintable characters
-- here.
formatPath :: String -> String
formatPath path = "\"" ++ quote path ++ "\""
  where
    quote "" = ""
    quote (c:cs) = if c `elem` ['\\', '"']
                       then '\\' : c : quote cs
                       else c : quote cs

defaultRepo :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
defaultRepo fs = defaultrepo (remoteRepos fs)

amInHashedRepository :: [DarcsFlag] -> IO (Either String ())
amInHashedRepository fs = R.amInHashedRepository (workRepo fs)

amInRepository :: [DarcsFlag] -> IO (Either String ())
amInRepository fs = R.amInRepository (workRepo fs)

amNotInRepository :: [DarcsFlag] -> IO (Either String ())
amNotInRepository fs = R.amNotInRepository (workRepo fs)

findRepository :: [DarcsFlag] -> IO (Either String ())
findRepository fs = R.findRepository (workRepo fs)
