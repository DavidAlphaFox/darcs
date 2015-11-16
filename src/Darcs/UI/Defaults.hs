module Darcs.UI.Defaults ( applyDefaults ) where

import Control.Monad.Writer
import Data.Char ( isSpace )
import Data.Functor.Compose ( Compose(..) )
import Data.List ( nub, intercalate )
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import System.Console.GetOpt
import Text.Regex.Applicative
    ( (<$>), (<*>), (*>), (<|>)
    , match, pure, many, some
    , psym, anySym, string )

import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( DarcsOptDescr )

import Darcs.UI.Commands
    ( DarcsCommand(..), commandAlloptions, extractAllCommands
    , WrappedCommand(..)
    )
import Darcs.UI.TheCommands ( commandControlList )
import Darcs.Util.Path ( AbsolutePath )

-- | Apply defaults from all sources to a list of 'DarcsFlag's (e.g. from the
-- command line), given the command (and possibly super command) name, and a
-- list of all options for the command.
-- 
-- Sources for defaults are
-- 
--  * the builtin (hard-coded) defaults,
-- 
--  * the defaults file in the user's configuration, and
-- 
--  * the defaults file in the current repository.
-- 
-- Note that the pseudo command @ALL@ is allowed in defaults files to specify
-- that an option should be the default for all commands to which it applies.
-- 
-- The order of precedence for conflicting options (i.e. those belonging to
-- same group of mutually exclusive options) is from less specific to more
-- specific. In other words, options from the command line override all
-- defaults, per-repo defaults override per-user defaults, which in turn
-- override the built-in defaults. Inside the options from a defaults file,
-- options for the given command override options for the @ALL@ pseudo command.
-- 
-- Conflicting options at the same level of precedence are not allowed.
--
-- Errors encountered during processing of command line or defaults flags
-- are formatted and added as (separate) strings to the list of error messages
-- that are returned together with the resulting flag list.
applyDefaults :: Maybe String
              -> DarcsCommand pf
              -> AbsolutePath
              -> [String]
              -> [String]
              -> [DarcsFlag]
              -> ([DarcsFlag], [String])
applyDefaults msuper cmd cwd user repo flags = runWriter $ do
    cl_flags  <- runChecks "Command line" check_opts flags
    user_defs <- get_flags "User defaults" user
    repo_defs <- get_flags "Repo defaults" repo
    return $ cl_flags ++ repo_defs ++ user_defs ++ builtin_defs
  where
    cmd_name = mkCmdName msuper (commandName cmd)
    builtin_defs = commandDefaults cmd
    check_opts = commandCheckOptions cmd
    opts = uncurry (++) $ commandAlloptions cmd
    get_flags source = parseDefaults source cwd cmd_name opts check_opts

-- | Name of a normal command, or name of super and sub command.
data CmdName = NormalCmd String | SuperCmd String String

-- | Make a 'CmdName' from a possible super command name and a sub command name.
mkCmdName :: Maybe String -> String -> CmdName
mkCmdName Nothing cmd = NormalCmd cmd
mkCmdName (Just super) sub = SuperCmd super sub

-- | Turn a 'CmdName' into a 'String'. For a 'SuperCmd' concatenate with a space in between.
showCmdName :: CmdName -> String
showCmdName (SuperCmd super sub) = unwords [super,sub]
showCmdName (NormalCmd name) = name

runChecks :: String -> ([DarcsFlag] -> [String]) -> [DarcsFlag] -> Writer [String] [DarcsFlag]
runChecks source check fs = case check fs of
  [] -> return fs
  es -> do
    tell [intercalate "\n" $ map ((source++": ")++) es]
    return fs

-- | Parse a list of lines from a defaults file, returning a list of 'DarcsFlag',
-- given the current working directory, the command name, and a list of 'DarcsOption'
-- for the command.
--
-- In the result, defaults for the given command come first, then come defaults
-- for @ALL@ commands.
--
-- We check that matching options actually exist.
--
--  * lines matching the command name: the option must exist in the command's
--    option map.
--
--  * lines matching @ALL@: there must be at least *some* darcs command with
--    that option.
--
-- It is debatable whether these checks are useful. On the one hand they can help
-- detect typos in defaults files. On the other hand they make it difficult to
-- use different versions of darcs in parallel: a default for an option that is
-- only available in a later version will make the earlier version produce an
-- error. Maybe reduce this to a warning?
parseDefaults :: String
              -> AbsolutePath
              -> CmdName
              -> [DarcsOptDescr DarcsFlag]
              -> ([DarcsFlag] -> [String])
              -> [String]
              -> Writer [String] [DarcsFlag]
parseDefaults source cwd cmd opts check_opts def_lines = do
    cmd_flags <- flags_for (M.keys opt_map) cmd_defs >>=
      runChecks (source++" for command '"++showCmdName cmd++"'") check_opts
    all_flags <- flags_for allOptionSwitches all_defs >>=
      runChecks (source++" for ALL commands") check_opts
    return $ cmd_flags ++ all_flags
  where
    opt_map = optionMap opts
    cmd_defs = parseDefaultsLines cmd def_lines
    all_defs = parseDefaultsLines (NormalCmd "ALL") def_lines
    to_flag all_switches (switch,arg) =
      if switch `notElem` all_switches then do
        tell [source++": command '"++showCmdName cmd
             ++"' has no option '"++switch++"'."]
        return Nothing
      else
        mapErrors ((source++" for command '"++showCmdName cmd++"':"):)
          $ defaultToFlag cwd opt_map (switch,arg)
    -- the catMaybes filters out options that are not defined
    -- for this command
    flags_for all_switches = fmap catMaybes . mapM (to_flag all_switches)
    mapErrors f = mapWriter (\(r, es) -> (r, if null es then [] else f es))

-- | Result of parsing a defaults line: switch and argument(s).
type Default = (String, String)

-- | Extract 'Default's from lines of a defaults file that match the given 'CmdName'.
-- 
-- The syntax is
--
-- @
--  supercmd subcmd [--]switch [args...]
-- @
--
-- for (super) commands with a sub command, and
--
-- @
--  cmd default [--]default [args...]
-- @
--
-- for normal commands (including the @ALL@ pseudo command).
parseDefaultsLines :: CmdName -> [String] -> [Default]
parseDefaultsLines cmd = catMaybes . map matchLine
  where
    matchLine = match $ (,) <$> (match_cmd cmd *> spaces *> opt_dashes *> word) <*> rest
    match_cmd (NormalCmd name) = string name
    match_cmd (SuperCmd super sub) = string super *> spaces *> string sub
    opt_dashes = string "--" <|> pure ""
    word = some $ psym (not.isSpace)
    spaces = some $ psym isSpace
    rest = spaces *> many anySym <|> pure ""

{- $note
This definition is a bit simpler, and doesn't need Text.Regex.Applicative,
but it has two disadvantages over the one above:

 * Flag arguments are split and joined again with words/unwords, which means
   that whitespace inside an argument is not preserved literally.

 * It is less easily extendable with new syntax.

> parseDefaultsLines :: CmdName -> [String] -> [(String, String)]
> parseDefaultsLines name entries = case name of
>     SuperCmd super sub -> [ mk_def d as | (s:c:d:as) <- map words entries, s == super, c == sub ]
>     NormalCmd cmd ->      [ mk_def d as | (c:d:as) <- map words entries, c == cmd ]
>   where
>     mk_def d as = (drop_dashes d, unwords as)
>     drop_dashes ('-':'-':switch) = switch
>     drop_dashes switch = switch
-}

-- | Search an option list for a switch. If found, apply the flag constructor
-- from the option to the arg, if any. The first parameter is the current working
-- directory, which, depending on the option type, may be needed to create a flag
-- from an argument.
-- 
-- Fails if (default has argument /= corresponding option has argument).
defaultToFlag :: AbsolutePath
              -> OptionMap
              -> Default
              -> Writer [String] (Maybe DarcsFlag)
defaultToFlag cwd opts (switch, arg) = case M.lookup switch opts of
    -- This case is not impossible! A default flag defined for ALL commands
    -- is not necessarily defined for the concrete command in question.
    Nothing -> return Nothing
    Just opt -> flag_from $ getArgDescr $ getCompose opt
  where
    getArgDescr (Option _ _ a _) = a
    flag_from (NoArg mkFlag) = do
      if not (null arg) then do
        tell ["'"++switch++"' takes no argument, but '"++arg++"' argument given."]
        return Nothing
      else
        return $ Just $ mkFlag cwd
    flag_from (OptArg mkFlag _) =
      return $ Just $ mkFlag (if null arg then Nothing else Just arg) cwd
    flag_from (ReqArg mkFlag _) = do
      if null arg then do
        tell ["'"++switch++"' requires an argument, but no "++"argument given."]
        return Nothing
      else
        return $ Just $ mkFlag arg cwd

-- | Get all the longSwitches from a list of options.
optionSwitches :: [DarcsOptDescr DarcsFlag] -> [String]
optionSwitches = concatMap sel where
  sel (Compose (Option _ switches _ _)) = switches

-- | A finite map from long switches to 'DarcsOptDescr's.
type OptionMap = M.Map String (DarcsOptDescr DarcsFlag)

-- | Build an 'OptionMap' from a list of 'DarcsOption's.
optionMap :: [DarcsOptDescr DarcsFlag] -> OptionMap
optionMap = M.fromList . concatMap sel where
  add_option opt switch = (switch, opt)
  sel o@(Compose (Option _ switches _ _)) = map (add_option o) switches

-- | List of option switches of all commands (except help but that has no options).
allOptionSwitches :: [String]
allOptionSwitches = nub $ optionSwitches $
  concatMap (\(WrappedCommand c) -> uncurry (++) . commandAlloptions $ c) $
            extractAllCommands commandControlList
