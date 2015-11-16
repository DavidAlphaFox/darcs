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

{-# LANGUAGE CPP, PatternGuards #-}

module Darcs.UI.Commands.Record
    ( record
    , commit
    , getLog -- used by amend and tag, too
    , recordConfig, RecordConfig(..) -- needed for darcsden
    ) where

import Prelude hiding ( (^), catch )

import Control.Applicative ( (<$>) )
import Control.Exception ( handleJust, catch, IOException )
import Control.Monad ( when, unless, void )
import System.IO ( stdin )
import Data.List ( sort, isPrefixOf )
import Data.Char ( ord )
import System.Exit ( exitFailure, exitSuccess, ExitCode(..) )
import System.Directory ( removeFile )
import qualified Data.ByteString as B ( hPut )

import Darcs.Repository.Lock
    ( readLocaleFile
    , writeLocaleFile
    , appendToFile
    )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , tentativelyAddPatch
    , finalizeRepositoryChanges
    , invalidateIndex
    , unrecordedChangesWithPatches
    , readRecorded
    , listRegisteredFiles
    )
import Darcs.Patch
    ( RepoPatch, Patchy, PrimOf, PrimPatch
    , namepatch, summaryFL, adddeps, fromPrims )
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), nullFL )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Split ( primSplitter )
import Darcs.UI.External ( editFile )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContextPrim
    , runSelection
    , askAboutDepends
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Path ( FilePathLike, SubPath, toFilePath, AbsolutePath )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , nodefaults
    , commandStub
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Util ( announceFiles, filterExistingFiles,
                                testTentativeAndMaybeExit )
import Darcs.UI.Flags
    ( DarcsFlag
    , fileHelpAuthor
    , getAuthor
    , getDate
    , diffOpts
    , fixSubPaths
    )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, oparse, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking (..), DryRun(NoDryRun)  )
import Darcs.Repository.Util ( getMovesPs, getReplaces )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Prompt ( askUser, promptYorn )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Global ( darcsLastMessage )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Util.Printer ( putDocLn, hPutDocLn, text, ($$), prefixLines, RenderMode(..) )
import Darcs.Util.ByteString ( encodeLocale )
import qualified Darcs.Util.Ratified as Ratified ( hGetContents )
import Storage.Hashed.Tree( Tree )

recordDescription :: String
recordDescription = "Create a patch from unrecorded changes."

recordHelp :: String
recordHelp =
 "The `darcs record` command is used to create a patch from changes in\n" ++
 "the working tree.  If you specify a set of files and directories,\n" ++
 "changes to other files will be skipped.\n" ++
 "\n" ++ recordHelp' ++
 "\n" ++ recordHelp''

recordBasicOpts :: DarcsOption a
                   (Maybe String
                    -> Maybe String
                    -> O.TestChanges
                    -> Maybe Bool
                    -> Bool
                    -> Bool
                    -> Maybe O.AskLongComment
                    -> O.LookFor
                    -> Maybe String
                    -> O.WithContext
                    -> O.DiffAlgorithm
                    -> a)
recordBasicOpts
    = O.patchname
    ^ O.author
    ^ O.testChanges
    ^ O.interactive
    ^ O.pipe
    ^ O.askdeps
    ^ O.askLongComment
    ^ O.lookfor
    ^ O.workingRepoDir
    ^ O.withContext
    ^ O.diffAlgorithm

recordAdvancedOpts :: DarcsOption a
                      (O.Logfile -> O.Compression -> O.UseIndex -> O.UMask -> O.SetScriptsExecutable -> a)
recordAdvancedOpts = O.logfile ^ O.compress ^ O.useIndex ^ O.umask ^ O.setScriptsExecutable

recordOpts :: DarcsOption a
              (Maybe String
               -> Maybe String
               -> O.TestChanges
               -> Maybe Bool
               -> Bool
               -> Bool
               -> Maybe O.AskLongComment
               -> O.LookFor
               -> Maybe String
               -> O.WithContext
               -> O.DiffAlgorithm
               -> Maybe O.StdCmdAction
               -> Bool
               -> Bool
               -> O.Verbosity
               -> Bool
               -> O.Logfile
               -> O.Compression
               -> O.UseIndex
               -> O.UMask
               -> O.SetScriptsExecutable
               -> O.UseCache
               -> Maybe String
               -> Bool
               -> Maybe String
               -> Bool
               -> a)
recordOpts = recordBasicOpts `withStdOpts` recordAdvancedOpts

data RecordConfig = RecordConfig
    { patchname :: Maybe String
    , author :: Maybe String
    , testChanges :: O.TestChanges
    , interactive :: Maybe Bool
    , pipe :: Bool
    , askDeps :: Bool
    , askLongComment :: Maybe O.AskLongComment
    , lookfor :: O.LookFor
    , _workingRepoDir :: Maybe String
    , withContext :: O.WithContext
    , diffAlgorithm :: O.DiffAlgorithm
    , verbosity :: O.Verbosity
    , logfile :: O.Logfile
    , compress :: O.Compression
    , useIndex :: O.UseIndex
    , umask :: O.UMask
    , sse :: O.SetScriptsExecutable
    , useCache :: O.UseCache
    }

recordConfig :: [DarcsFlag] -> RecordConfig
recordConfig = oparse (recordBasicOpts ^ O.verbosity ^ recordAdvancedOpts ^ O.useCache) RecordConfig

record :: DarcsCommand RecordConfig
record = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "record"
    , commandHelp = recordHelp
    , commandDescription = recordDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = recordCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc recordAdvancedOpts
    , commandBasicOptions = odesc recordBasicOpts
    , commandDefaults = defaultFlags recordOpts
    , commandCheckOptions = ocheck recordOpts
    , commandParseOptions = recordConfig
}

commitDescription :: String
commitDescription = "Redirect the user to record, push or send."

commitHelp :: String
commitHelp =
 "This command does not do anything.\n"++
 "If you want to save changes locally, use the `darcs record` command.\n"++
 "If you want to save a recorded patch to another repository, use the\n"++
 "`darcs push` or `darcs send` commands instead.\n"

commit :: DarcsCommand RecordConfig
commit = commandStub "commit" commitHelp commitDescription record

recordCmd :: (AbsolutePath, AbsolutePath) -> RecordConfig -> [String] -> IO ()
recordCmd fps cfg args = do
    checkNameIsNotOption (patchname cfg) (isInteractive True cfg)
    withRepoLock NoDryRun (useCache cfg) YesUpdateWorking (umask cfg) $ RepoJob $ \(repository :: Repository p wR wU wR) -> do
      files <- if null args then return Nothing
          else Just . sort <$> fixSubPaths fps args
      when (files == Just []) $ fail "No valid arguments were given."
      announceFiles files "Recording changes in"
      existing_files <- maybe (return Nothing)
          (fmap Just . filterExistingFiles repository (O.adds (lookfor cfg))) files
      when (existing_files == Just []) $
         fail "None of the files you specified exist!"
      debugMessage "About to get the unrecorded changes."
      Sealed replacePs <- if O.replaces (lookfor cfg) == O.YesLookForReplaces
        then getReplaces (diffingOpts cfg) repository files
        else return (Sealed NilFL)
      movesPs <- if O.moves (lookfor cfg) == O.YesLookForMoves
          then getMovesPs repository files
          else return NilFL
      changes <- unrecordedChangesWithPatches (diffingOpts cfg) repository files
                   movesPs (unsafeCoerceP replacePs :: FL (PrimOf p) wR wR)
      debugMessage "I've got unrecorded changes."
      case changes of
          NilFL | not (askDeps cfg) -> do
              -- We need to grab any input waiting for us, since we
              -- might break scripts expecting to send it to us; we
              -- don't care what that input is, though.
              void (getDate (pipe cfg))
              putStrLn "No changes!"
              exitFailure
          _ -> doRecord repository cfg existing_files changes

-- | Check user specified patch name is not accidentally a command line flag
checkNameIsNotOption :: Maybe String -> Bool -> IO ()
checkNameIsNotOption Nothing     _      = return ()
checkNameIsNotOption _           False  = return ()
checkNameIsNotOption (Just name) True   =
    when (length name == 1 || (length name == 2 && head name == '-')) $ do
        confirmed <- promptYorn $ "You specified " ++ show name ++ " as the patch name. Is that really what you want?"
        unless confirmed $ putStrLn "Okay, aborting the record." >> exitFailure

doRecord :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
         => Repository p wR wU wR -> RecordConfig -> Maybe [SubPath] -> FL (PrimOf p) wR wX -> IO ()
doRecord repository cfg files ps = do
    date <- getDate (pipe cfg)
    my_author <- getAuthor (author cfg) (pipe cfg)
    debugMessage "I'm slurping the repository."
    debugMessage "About to select changes..."
    pristine <- readRecorded repository
    (chs :> _ ) <- runSelection (selectChanges ps) $
                  selectionContextPrim First "record" (patchSelOpts cfg) (Just primSplitter)
                                       (map toFilePath <$> files)
                                       (Just pristine)
    when (is_empty_but_not_askdeps chs) $
              do putStrLn "Ok, if you don't want to record anything, that's fine!"
                 exitSuccess
    handleJust onlySuccessfulExits (\_ -> return ()) $
             do deps <- if askDeps cfg
                        then askAboutDepends repository chs (patchSelOpts cfg) []
                        else return []
                when (askDeps cfg) $ debugMessage "I've asked about dependencies."
                if nullFL chs && null deps
                  then putStrLn "Ok, if you don't want to record anything, that's fine!"
                  else do setEnvDarcsFiles chs
                          (name, my_log, logf) <- getLog (patchname cfg) (pipe cfg) (logfile cfg) (askLongComment cfg) Nothing chs
                          debugMessage ("Patch name as received from getLog: " ++ show (map ord name))
                          doActualRecord repository cfg name date my_author my_log logf deps chs
    where is_empty_but_not_askdeps l
              | askDeps cfg = False
                                      -- a "partial tag" patch; see below.
              | otherwise = nullFL l

doActualRecord :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository p wR wU wR
               -> RecordConfig
               -> String -> String -> String
               -> [String] -> Maybe String
               -> [PatchInfo] -> FL (PrimOf p) wR wX -> IO ()
doActualRecord repository cfg name date my_author my_log logf deps chs =
              do debugMessage "Writing the patch file..."
                 mypatch <- namepatch date name my_author my_log $
                            fromPrims $ progressFL "Writing changes:" chs
                 let pia = n2pia $ adddeps mypatch deps
                 -- We don't care about the returned updated repository
                 _ <- tentativelyAddPatch repository (compress cfg) (verbosity cfg) YesUpdateWorking
                           $ pia
                 invalidateIndex repository
                 debugMessage "Applying to pristine..."
                 testTentativeAndMaybeExit repository
                      (verbosity cfg)
                      (testChanges cfg)
                      (sse cfg)
                      (isInteractive True cfg)
                      ("you have a bad patch: '" ++ name ++ "'")
                      "record it" (Just failuremessage)
                 finalizeRepositoryChanges repository YesUpdateWorking (compress cfg)
                                    `clarifyErrors` failuremessage
                 debugMessage "Syncing timestamps..."
                 removeLogFile logf
                 unless (verbosity cfg == O.Quiet) $
                     putDocLn $ text $ "Finished recording patch '" ++ name ++ "'"
                 setEnvDarcsPatches (pia :>: NilFL)
    where
        removeLogFile :: Maybe String -> IO ()
        removeLogFile Nothing = return ()
        removeLogFile (Just lf) | lf == darcsLastMessage = return ()
                                | otherwise              = removeFile lf
        failuremessage = "Failed to record patch '"++name++"'" ++
                           case logf of Just lf -> "\nLogfile left in "++lf++"."
                                        Nothing -> ""

recordHelp' :: String
recordHelp' = unlines
 [ "Every patch has a name, an optional description, an author and a date."
 , ""
 , "Darcs will launch a text editor (see `darcs help environment`) after the"
 , "interactive selection, to let you enter the patch name (first line) and"
 , "the patch description (subsequent lines)."
 , ""
 , "The patch name should be a short sentence that concisely describes the"
 , "patch, such as \"Add error handling to main event loop.\"  You can"
 , "supply it in advance with the `-m` option, in which case no text editor"
 , "is launched, unless you use the `--edit-long-comment` option."
 , ""
 , "The patch description is an optional block of free-form text.  It is"
 , "used to supply additional information that doesn't fit in the patch"
 , "name.  For example, it might include a rationale of WHY the change was"
 , "necessary."
 , ""
 , "A technical difference between patch name and patch description, is"
 , "that matching with the flag `-p` is only done on patch names."
 , ""
 , "Finally, the `--logfile` option allows you to supply a file that"
 , "already contains the patch name and patch description.  This is"
 , "useful if a previous record failed and left a `darcs-record-0` file."
 , ""
 , unlines fileHelpAuthor
 , ""
 , "If you want to manually define any extra dependencies for your patch,"
 , "you can use the `--ask-deps` flag, and darcs will ask you for the patch's"
 , "dependencies.  Some dependencies may be automatically inferred from the"
 , "patch's content and cannot be removed. "
 , "A patch with specific dependencies can be empty."
 , ""
 , "The patch date is generated automatically.  It can only be spoofed by"
 , "using the `--pipe` option."
 , ""
 , "If you run record with the `--pipe` option, you will be prompted for"
 , "the patch date, author, and the long comment. The long comment will extend"
 , "until the end of file or stdin is reached (ctrl-D on Unixy systems, ctrl-Z"
 , "on systems running a Microsoft OS)."
 , ""
 , "This interface is intended for scripting darcs, in particular for writing"
 , "repository conversion scripts.  The prompts are intended mostly as a useful"
 , "guide (since scripts won't need them), to help you understand the format in"
 , "which to provide the input. Here's an example of what the `--pipe`"
 , "prompts look like:"
 , ""
 , "    What is the date? Mon Nov 15 13:38:01 EST 2004"
 , "    Who is the author? David Roundy"
 , "    What is the log? One or more comment lines"
 ]

data PName = FlagPatchName String | PriorPatchName String | NoPatchName

-- | Get the patch name and long description from one of
--
--  * the configuration (flags, defaults, hard-coded)
--
--  * an existing log file
--
--  * stdin (e.g. a pipe)
--
--  * a text editor
--
-- It ensures the patch name is not empty nor starts with the prefix TAG.
--
-- The last result component is a possible path to a temporary file that should be removed later.
getLog :: forall prim wX wY . (Patchy prim, PrimPatch prim)
       => Maybe String                          -- ^ patchname option
       -> Bool                                  -- ^ pipe option
       -> O.Logfile                             -- ^ logfile option
       -> Maybe O.AskLongComment                -- ^ askLongComment option
       -> Maybe (String, [String])              -- ^ possibly an existing patch name and long description
       -> FL prim wX wY                         -- ^ changes to record
       -> IO (String, [String], Maybe String)   -- ^ patch name, long description and possibly the path
                                                --   to the temporary file that should be removed later
getLog m_name has_pipe log_file ask_long m_old chs = go has_pipe log_file ask_long where
  go True _ _ = do
      p <- case patchname_specified of
             FlagPatchName p  -> return p
             PriorPatchName p -> return p
             NoPatchName      -> prompt_patchname False
      putStrLn "What is the log?"
      thelog <- lines `fmap` Ratified.hGetContents stdin
      return (p, thelog, Nothing)
  go _ (O.Logfile { O._logfile = Just f }) _ = do
      mlp <- lines `fmap` readLocaleFile f `catch` (\(_ :: IOException) -> return [])
      firstname <- case (patchname_specified, mlp) of
                     (FlagPatchName  p, []) -> return p
                     (_, p:_)               -> if badName p
                                                 then prompt_patchname True
                                                 else return p -- logfile trumps prior!
                     (PriorPatchName p, []) -> return p
                     (NoPatchName, [])      -> prompt_patchname True
      append_info f firstname
      when (ask_long == Just O.YesEditLongComment) (void $ editFile f)
      (name, thelog) <- read_long_comment f firstname
      return (name, thelog, if O._rmlogfile log_file then Just $ toFilePath f else Nothing)
  go _ _ (Just O.YesEditLongComment) =
      case patchname_specified of
          FlagPatchName  p  -> actually_get_log p
          PriorPatchName p  -> actually_get_log p
          NoPatchName       -> actually_get_log ""
  go _ _ (Just O.NoEditLongComment) =
      case patchname_specified of
          FlagPatchName  p  -> return (p, default_log, Nothing) -- record (or amend) -m
          PriorPatchName p  -> return (p, default_log, Nothing) -- amend
          NoPatchName       -> do p <- prompt_patchname True -- record
                                  return (p, [], Nothing)
  go _ _ (Just O.PromptLongComment) =
      case patchname_specified of
          FlagPatchName p   -> prompt_long_comment p -- record (or amend) -m
          PriorPatchName p  -> prompt_long_comment p
          NoPatchName       -> prompt_patchname True >>= prompt_long_comment
  go _ _ Nothing =
      case patchname_specified of
          FlagPatchName  p  -> return (p, default_log, Nothing)  -- record (or amend) -m
          PriorPatchName "" -> actually_get_log ""
          PriorPatchName p  -> return (p, default_log, Nothing)
          NoPatchName       -> actually_get_log ""

  patchname_specified = case (m_name, m_old) of
                          (Just name, _) | badName name -> NoPatchName
                                         | otherwise    -> FlagPatchName name
                          (Nothing,   Just (name,_))    -> PriorPatchName name
                          (Nothing,   Nothing)          -> NoPatchName

  badName "" = True
  badName n  = "TAG" `isPrefixOf` n

  default_log = case m_old of
                  Nothing    -> []
                  Just (_,l) -> l

  prompt_patchname retry =
    do n <- askUser "What is the patch name? "
       if badName n
          then if retry then prompt_patchname retry
                        else fail "Bad patch name!"
          else return n

  prompt_long_comment oldname =
    do y <- promptYorn "Do you want to add a long comment?"
       if y then actually_get_log oldname
            else return (oldname, [], Nothing)

  actually_get_log p = do let logf = darcsLastMessage
                          -- TODO: make sure encoding used for logf is the same everywhere
                          -- probably should be locale because the editor will assume it
                          writeLocaleFile logf $ unlines $ p : default_log
                          append_info logf p
                          _ <- editFile logf
                          (name,long) <- read_long_comment logf p
                          if badName name
                            then do putStrLn "WARNING: empty or incorrect patch name!"
                                    pn <- prompt_patchname True
                                    return (pn, long, Nothing)
                            else return (name,long,Just logf)

  read_long_comment :: FilePathLike p => p -> String -> IO (String, [String])
  read_long_comment f oldname =
      do f' <- readLocaleFile f
         let t = filter (not.("#" `isPrefixOf`)) $ (lines.filter (/='\r')) f'
         case t of []     -> return (oldname, [])
                   (n:ls) -> return (n, ls)

  append_info f oldname =
      do fc <- readLocaleFile f
         appendToFile f $ \h ->
             do case fc of
                  _ | null (lines fc) -> B.hPut h (encodeLocale (oldname ++ "\n"))
                    | last fc /= '\n' -> B.hPut h (encodeLocale "\n")
                    | otherwise       -> return ()
                hPutDocLn Encode h
                     $ text "# Please enter the patch name in the first line, and"
                    $$ text "# optionally, a long description in the following lines."
                    $$ text "#"
                    $$ text "# Lines starting with '#' will be ignored."
                    $$ text "#"
                    $$ text "#"
                    $$ text "# This patch contains the following changes:"
                    $$ text "#"
                    $$ prefixLines (text "#") (summaryFL chs)

onlySuccessfulExits :: ExitCode -> Maybe ()
onlySuccessfulExits ExitSuccess = Just ()
onlySuccessfulExits _ = Nothing

recordHelp'' :: String
recordHelp'' =
 "If a test command has been defined with `darcs setpref`, attempting to\n" ++
 "record a patch will cause the test command to be run in a clean copy\n" ++
 "of the working tree (that is, including only recorded changes).  If\n" ++
 "the test fails, you will be offered to abort the record operation.\n" ++
 "\n" ++
 "The `--set-scripts-executable` option causes scripts to be made\n" ++
 "executable in the clean copy of the working tree, prior to running the\n" ++
 "test.  See `darcs clone` for an explanation of the script heuristic.\n" ++
 "\n" ++
 "If your test command is tediously slow (e.g. `make all`) and you are\n" ++
 "recording several patches in a row, you may wish to use `--no-test` to\n" ++
 "skip all but the final test.\n" ++
 "\n" ++
 "To see some context (unchanged lines) around each change, use the\n" ++
 "`--unified` option.\n"

patchSelOpts :: RecordConfig -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = verbosity cfg
    , S.matchFlags = []
    , S.diffAlgorithm = diffAlgorithm cfg
    , S.interactive = isInteractive True cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext cfg
    }

diffingOpts :: RecordConfig -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts cfg = diffOpts (useIndex cfg) (O.adds (lookfor cfg)) False (diffAlgorithm cfg)

isInteractive :: Bool -> RecordConfig -> Bool
isInteractive def = maybe def id . interactive
