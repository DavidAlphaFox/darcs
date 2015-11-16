--  Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Commands.Help (
 helpCmd,
 commandControlList, environmentHelp,          -- these are for preproc.hs
 printVersion,
 listAvailableCommands ) where

import Darcs.UI.Flags
    ( DarcsFlag
    , environmentHelpEmail
    , environmentHelpSendmail
    )
import Darcs.UI.Options.Markdown ( optionsMarkdown )
import Darcs.UI.Commands
    ( CommandArgs(..)
    , CommandControl(..)
    , normalCommand
    , DarcsCommand(..), withStdOpts
    , WrappedCommand(..)
    , wrappedCommandName
    , disambiguateCommands
    , extractCommands
    , getCommandHelp
    , nodefaults
    , usage
    )
import Darcs.UI.External ( viewDoc )
import Darcs.Repository.Lock ( environmentHelpTmpdir, environmentHelpKeepTmpdir
                             , environmentHelpLocks )
import Darcs.Patch.Match ( helpOnMatchers )
import Darcs.Repository.Prefs ( boringFileHelp, binariesFileHelp, environmentHelpHome )
import Darcs.Repository.Ssh ( environmentHelpSsh, environmentHelpScp, environmentHelpSshPort )
import Darcs.Repository.External ( environmentHelpProtocols )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Path ( AbsolutePath )
import Control.Arrow ( (***) )
import Data.Char ( isAlphaNum, toLower, toUpper )
import Data.Either ( partitionEithers )
import Data.List ( groupBy, isPrefixOf, intercalate, nub )
import Darcs.Util.English ( andClauses )
import Darcs.Util.Printer (text, vcat, vsep, ($$), empty)
import Darcs.Util.Printer.Color ( environmentHelpColor, environmentHelpEscape, environmentHelpEscapeWhite )
import System.Exit ( exitSuccess )
import Version ( version )
import Darcs.Util.Download ( environmentHelpProxy, environmentHelpProxyPassword )
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.UI.Options ( DarcsOption, defaultFlags, ocheck, onormalise, oid )
import qualified Darcs.UI.Options.All as O ( StdCmdAction, Verbosity, UseCache )
import qualified Darcs.UI.TheCommands as TheCommands

helpDescription :: String
helpDescription = "Display help about darcs and darcs commands."

helpHelp :: String
helpHelp =
 "Without arguments, `darcs help` prints a categorized list of darcs\n" ++
 "commands and a short description of each one.  With an extra argument,\n" ++
 "`darcs help foo` prints detailed help about the darcs command foo.\n"

argPossibilities :: [String]
argPossibilities = map wrappedCommandName $ extractCommands commandControlList

helpOpts :: DarcsOption a
            (Maybe O.StdCmdAction
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
helpOpts = withStdOpts oid oid

help :: DarcsCommand [DarcsFlag]
help = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "help"
    , commandHelp = helpHelp
    , commandDescription = helpDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  "]
    , commandCommand = \ x y z -> helpCmd x y z >> exitSuccess
    , commandPrereq = \_ -> return $ Right ()
    , commandGetArgPossibilities = return argPossibilities
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = []
    , commandDefaults = defaultFlags helpOpts
    , commandCheckOptions = ocheck helpOpts
    , commandParseOptions = onormalise helpOpts
    }

helpCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
helpCmd _ _ ["manpage"] = putStr $ unlines manpageLines
helpCmd _ _ ["markdown"] = putStr $ unlines markdownLines
helpCmd _ _ ["patterns"] = viewDoc $ text $ unlines helpOnMatchers
helpCmd _ _ ("environment":vs_) =
    viewDoc $ header $$
              vsep (map render known) $$
              footer
  where
    header | null known = empty
           | otherwise = text "Environment Variables" $$
                         text "====================="

    footer | null unknown = empty
           | otherwise = text "" $$
                         text ("Unknown environment variables: "
                               ++ intercalate ", " unknown)

    render (ks, ds) = text (andClauses ks ++ ":") $$
                      vcat [ text ("  " ++ d) | d <- ds ]

    (unknown, known) = case map (map toUpper) vs_ of
                           [] -> ([], environmentHelp)
                           vs -> (nub *** (nub . concat)) . partitionEithers $
                                     map doLookup vs

    -- v is not known if it doesn't appear in the list of aliases of any
    -- of the environment var help descriptions.
    doLookup v = case filter ((v `elem`) . fst) environmentHelp of
                     [] -> Left v
                     es -> Right es

helpCmd _ _ [] = viewDoc $ text $ usage commandControlList

helpCmd _ _ (cmd:args) =
    let disambiguated = disambiguateCommands commandControlList cmd args
    in case disambiguated of
         Left err -> fail err
         Right (cmds,_) ->
             let msg = case cmds of
                         CommandOnly c       -> getCommandHelp Nothing  c
                         SuperCommandOnly c  -> getCommandHelp Nothing  c
                         SuperCommandSub c s -> getCommandHelp (Just c) s
             in viewDoc $ text msg

listAvailableCommands :: IO ()
listAvailableCommands =
    do here <- getCurrentDirectory
       is_valid <- mapM
                   (\(WrappedCommand c)-> withCurrentDirectory here $ commandPrereq c [])
                   (extractCommands commandControlList)
       putStr $ unlines $ map (wrappedCommandName . fst) $
                filter (isRight.snd) $
                zip (extractCommands commandControlList) is_valid
       putStrLn "--help"
       putStrLn "--version"
       putStrLn "--exact-version"
       putStrLn "--overview"
    where isRight (Right _) = True
          isRight _ = False

printVersion :: IO ()
printVersion = putStrLn $ "darcs version " ++ version

-- avoiding a module import cycle between Help and TheCommands
commandControlList :: [CommandControl]
commandControlList =
  normalCommand help : TheCommands.commandControlList

-- FIXME: the "grouping" comments below should made subsections in the
-- manpage, as we already do for DarcsCommand groups. --twb, 2009

-- | Help on each environment variable in which Darcs is interested.
environmentHelp :: [([String], [String])]
environmentHelp = [
 -- General-purpose
 environmentHelpHome,
 environmentHelpEditor,
 environmentHelpPager,
 environmentHelpColor,
 environmentHelpEscapeWhite,
 environmentHelpEscape,
 environmentHelpTmpdir,
 environmentHelpKeepTmpdir,
 environmentHelpEmail,
 environmentHelpSendmail,
 environmentHelpLocks,
 -- Remote Repositories
 environmentHelpSsh,
 environmentHelpScp,
 environmentHelpSshPort,
 environmentHelpProxy,
 environmentHelpProxyPassword,
 environmentHelpProtocols,
 environmentHelpTimeout]

-- | This module is responsible for emitting a darcs "man-page", a
-- reference document used widely on Unix-like systems.  Manpages are
-- primarily used as a quick reference, or "memory jogger", so the
-- output should be terser than the user manual.
--
-- Before modifying the output, please be sure to read the man(7) and
-- man-pages(7) manpages, as these respectively describe the relevant
-- syntax and conventions.

-- | The lines of the manpage to be printed.
manpageLines :: [String]
manpageLines = [
 ".TH DARCS 1 \"" ++ version ++ "\"",
 ".SH NAME",
 "darcs \\- an advanced revision control system",
 ".SH SYNOPSIS",
 ".B darcs", ".I command", ".RI < arguments |[ options ]>...",
 "",
 "Where the", ".I commands", "and their respective", ".I arguments", "are",
 "",
 unlines synopsis,
 ".SH DESCRIPTION",
 -- FIXME: this is copy-and-pasted from darcs.cabal, so
 -- it'll get out of date as people forget to maintain
 -- both in sync.
 "Darcs is a free, open source revision control",
 "system. It is:",
 ".TP 3", "\\(bu",
 "Distributed: Every user has access to the full",
 "command set, removing boundaries between server and",
 "client or committer and non\\(hycommitters.",
 ".TP", "\\(bu",
 "Interactive: Darcs is easy to learn and efficient to",
 "use because it asks you questions in response to",
 "simple commands, giving you choices in your work",
 "flow. You can choose to record one change in a file,",
 "while ignoring another. As you update from upstream,",
 "you can review each patch name, even the full `diff'",
 "for interesting patches.",
 ".TP", "\\(bu",
 "Smart: Originally developed by physicist David",
 "Roundy, darcs is based on a unique algebra of",
 "patches.",
 "This smartness lets you respond to changing demands",
 "in ways that would otherwise not be possible. Learn",
 "more about spontaneous branches with darcs.",
 ".SH OPTIONS",
 "Different options are accepted by different Darcs commands.",
 "Each command's most important options are listed in the",
 ".B COMMANDS",
 "section.  For a full list of all options accepted by",
 "a particular command, run `darcs", ".I command", "\\-\\-help'.",
 ".SS " ++ escape (unlines helpOnMatchers), -- FIXME: this is a kludge.
 ".SH COMMANDS",
 unlines commands,
 unlines environment,
 ".SH FILES",
 ".SS \"_darcs/prefs/binaries\"",
 escape $ unlines binariesFileHelp,
 ".SS \"_darcs/prefs/boring\"",
 escape $ unlines boringFileHelp,
 ".SH BUGS",
 "At http://bugs.darcs.net/ you can find a list of known",
 "bugs in Darcs.  Unknown bugs can be reported at that",
 "site (after creating an account) or by emailing the",
 "report to bugs@darcs.net.",
 -- ".SH EXAMPLE",
 -- FIXME:
 -- new project: init, rec -la;
 -- track upstream project: clone, pull -a;
 -- contribute to project: add, rec, push/send.
 ".SH SEE ALSO",
 "The Darcs website provides a lot of additional information.",
 "It can be found at http://darcs.net/",
 ".SH LICENSE",
 "Darcs is free software; you can redistribute it and/or modify",
 "it under the terms of the GNU General Public License as published by",
 "the Free Software Foundation; either version 2, or (at your option)",
 "any later version." ]
    where
      -- | A synopsis line for each command.  Uses 'foldl' because it is
      -- necessary to avoid blank lines from Hidden_commands, as groff
      -- translates them into annoying vertical padding (unlike TeX).
      synopsis :: [String]
      synopsis = foldl iter [] commandControlList
          where iter :: [String] -> CommandControl -> [String]
                iter acc (GroupName _) = acc
                iter acc (HiddenCommand _) = acc
                iter acc (CommandData (WrappedCommand c@SuperCommand {})) =
                    acc ++ concatMap
                            (render (commandName c ++ " "))
                            (extractCommands (commandSubCommands c))
                iter acc (CommandData c) = acc ++ render "" c
                render :: String -> WrappedCommand -> [String]
                render prefix (WrappedCommand c) =
                    [".B darcs " ++ prefix ++ commandName c] ++
                    map mangle_args (commandExtraArgHelp c) ++
                    -- In the output, we want each command to be on its own
                    -- line, but we don't want blank lines between them.
                    -- AFAICT this can only be achieved with the .br
                    -- directive, which is probably a GNUism.
                    [".br"]

      -- | As 'synopsis', but make each group a subsection (.SS), and
      -- include the help text for each command.
      commands :: [String]
      commands = foldl iter [] commandControlList
          where iter :: [String] -> CommandControl -> [String]
                iter acc (GroupName x) = acc ++ [".SS \"" ++ x ++ "\""]
                iter acc (HiddenCommand _) = acc
                iter acc (CommandData (WrappedCommand c@SuperCommand {})) =
                    acc ++ concatMap
                            (render (commandName c ++ " "))
                            (extractCommands (commandSubCommands c))
                iter acc (CommandData c) = acc ++ render "" c
                render :: String -> WrappedCommand -> [String]
                render prefix (WrappedCommand c) =
                    [".B darcs " ++ prefix ++ commandName c] ++
                    map mangle_args (commandExtraArgHelp c) ++
                    [".RS 4", escape $ commandHelp c, ".RE"]

      -- | Now I'm showing off: mangle the extra arguments of Darcs commands
      -- so as to use the ideal format for manpages, italic words and roman
      -- punctuation.
      mangle_args :: String -> String
      mangle_args s =
          ".RI " ++ unwords (map show (groupBy cmp $ map toLower $ gank s))
              where cmp x y = not $ xor (isAlphaNum x) (isAlphaNum y)
                    xor x y = (x && not y) || (y && not x)
                    gank (' ':'o':'r':' ':xs) = '|' : gank xs
                    gank (x:xs) = x : gank xs
                    gank [] = []

      environment :: [String]
      environment = ".SH ENVIRONMENT" : concat
                    [(".SS \"" ++ andClauses ks ++ "\"") : map escape ds
                     | (ks, ds) <- environmentHelp]

      escape :: String -> String
      escape = minus . bs       -- Order is important
        where
          minus      = replace "-"     "\\-"
          bs         = replace "\\"    "\\\\"

          replace :: Eq a => [a] -> [a] -> [a] -> [a]
          replace _ _ [] = []
          replace find repl s =
              if find `isPrefixOf` s
                  then repl ++ replace find repl (drop (length find) s)
                  else head s : replace find repl (tail s)

markdownLines :: [String]
markdownLines =
 [ "Darcs " ++ version, ""
 , "# Commands", ""
 , unlines commands
 , "# Environment variables"
 , "", unlines environment
 , "# Patterns"
 , "", unlines helpOnMatchers  ]
   where
      environment :: [String]
      environment = intercalate [""]
                     [ renderEnv ks ds | (ks, ds) <- environmentHelp ]
        where
          renderEnv k d = ("## " ++ (intercalate ", " k)) : "" : d
      commands :: [String]
      commands = foldl iter [] commandControlList
      iter :: [String] -> CommandControl -> [String]
      iter acc (GroupName x) = acc ++ ["## " ++ x, ""]
      iter acc (HiddenCommand _) = acc
      iter acc (CommandData (WrappedCommand c@SuperCommand {})) =
          acc ++ concatMap
                  (render (commandName c ++ " "))
                  (extractCommands (commandSubCommands c))
      iter acc (CommandData c) = acc ++ render "" c
      render :: String -> WrappedCommand -> [String]
      render prefix (WrappedCommand c) =
          [ "### " ++ prefix ++ commandName c
          , "", "darcs " ++ prefix ++ commandName c ++ " [OPTION]... " ++
          unwords (commandExtraArgHelp c)
          , "", commandDescription c
          , "", commandHelp c
          , "Options:", optionsMarkdown $ commandBasicOptions c
          , if null opts2 then ""
             else unlines ["Advanced Options:", optionsMarkdown opts2]
          ]
       where opts2 = commandAdvancedOptions c

environmentHelpEditor :: ([String], [String])
environmentHelpEditor = (["DARCS_EDITOR", "DARCSEDITOR", "VISUAL", "EDITOR"],[
 "To edit a patch description of email comment, Darcs will invoke an",
 "external editor.  Your preferred editor can be set as any of the",
 "environment variables $DARCS_EDITOR, $DARCSEDITOR, $VISUAL or $EDITOR.",
 "If none of these are set, vi(1) is used.  If vi crashes or is not",
 "found in your PATH, emacs, emacs -nw, nano and (on Windows) edit are",
 "each tried in turn."])

environmentHelpPager :: ([String], [String])
environmentHelpPager = (["DARCS_PAGER", "PAGER"],[
 "Darcs will sometimes invoke a pager if it deems output to be too long",
 "to fit onscreen.  Darcs will use the pager specified by $DARCS_PAGER",
 "or $PAGER.  If neither are set, `less` will be used."])

environmentHelpTimeout :: ([String], [String])
environmentHelpTimeout = (["DARCS_CONNECTION_TIMEOUT"],[
 "Set the maximum time in seconds that darcs allows and connection to",
 "take. If the variable is not specified the default are 30 seconds. This",
 "option only works with curl."])

-- | There are two environment variables that we do not document:
-- - DARCS_USE_ISPRINT: deprecated, use DARCS_DONT_ESCAPE_ISPRINT.
-- - DARCS_TESTING_PREFS_DIR: used by the test suite to tell darcs
--                            where to find its configuration files.
