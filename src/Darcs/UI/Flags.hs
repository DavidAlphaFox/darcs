-- Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Flags
    ( -- TODO we want to stop exporting the constructors of DarcsFlag
      -- from here. First need to change all the relevant code over to the
      -- using helpers from this module instead.
      F.DarcsFlag( .. )
    , compression
    , remoteDarcs
    , reorder
    , minimize
    , editDescription
    , diffingOpts
    , diffOpts
    , externalMerge
    , wantGuiPause
    , isInteractive
    , maxCount
    , willRemoveLogFile
    , isUnified
    , doHappyForwarding
    , includeBoring
    , doAllowCaseOnly
    , doAllowWindowsReserved
    , doReverse
    , usePacks
    , showChangesOnlyToFiles
    , removeFromAmended
    , toMatchFlags
    , verbosity
    , useCache
    , umask
    , dryRun
    , lookForAdds
    , lookForMoves
    , lookForReplaces
    , diffAlgorithm
    , runTest
    , testChanges
    , setScriptsExecutable
    , withWorkingDir
    , leaveTestDir
    , remoteRepos
    , setDefault
    , cloneKind
    , workRepo
    , allowConflicts
    , runPatchIndex
    , useIndex
    , hasSummary
    , hasXmlOutput
    , selectDeps
    , hasAuthor
    , hasLogfile
    , patchFormat

    , fixRemoteRepos
    , fixUrl
    , fixSubPaths
    , maybeFixSubPaths
    , getRepourl
    , getAuthor
    , promptAuthor
    , getEasyAuthor
    , getSendmailCmd
    , fileHelpAuthor
    , environmentHelpEmail
    , getSubject
    , getCharset
    , getInReplyTo
    , getCc
    , environmentHelpSendmail
    , siblings
    , getOutput
    , getDate
    , getReply
    , applyAs
    ) where

import Prelude hiding ( (^) )

import Data.List ( nub, intercalate )
import Data.Maybe
    ( isJust
    , maybeToList
    , isNothing
    , catMaybes
    )
import Control.Monad ( unless )
import Control.Applicative( (<$>) )
import System.Directory ( doesDirectoryExist, createDirectory )
import System.FilePath.Posix ( (</>) )

import qualified Darcs.Patch.Match as MF ( MatchFlag(..) )
import Darcs.UI.External
    ( catchall )
import qualified Darcs.UI.Options.Flags as F ( DarcsFlag( .. ) )
import Darcs.UI.Options.Core
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Environment ( maybeGetEnv )
import Darcs.Util.Exception ( firstJustIO )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Prompt
    ( askUser
    , askUserListItem
    )
import Darcs.Repository.Lock ( writeLocaleFile )
import Darcs.Repository.Prefs
    ( getPreflist
    , getGlobal
    , globalPrefsDirDoc
    , globalPrefsDir
    )
import Darcs.Util.ByteString ( decodeString )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.IsoDate ( getIsoDateTime, cleanLocalDate )
import Darcs.Util.Path
    ( AbsolutePath
    , AbsolutePathOrStd
    , SubPath
    , toFilePath
    , makeSubPathOf
    , ioAbsolute
    , makeAbsoluteOrStd
    )
import Darcs.Util.Printer ( putDocLn, text, ($$) )
import Darcs.Util.URL ( isValidLocalPath )

type Config = [F.DarcsFlag]

compression :: Config -> O.Compression
compression = parseFlags O.compress

remoteDarcs :: Config -> O.RemoteDarcs
remoteDarcs = O.remoteDarcs . parseFlags O.network

reorder :: Config -> O.Reorder
reorder = parseFlags O.reorder

minimize :: Config -> Bool
minimize = parseFlags O.minimize

editDescription :: Config -> Bool
editDescription = parseFlags O.editDescription

-- | Non-trivial interaction between options.
diffOpts :: O.UseIndex -> O.LookForAdds -> Bool -> O.DiffAlgorithm -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffOpts use_index look_for_adds include_boring diff_alg =
    (use_index, scanKnown look_for_adds include_boring, diff_alg)
  where
    scanKnown O.NoLookForAdds _ = O.ScanKnown
    scanKnown O.YesLookForAdds False = O.ScanAll
    scanKnown O.YesLookForAdds True = O.ScanBoring

diffingOpts :: Config -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts flags = diffOpts (useIndex flags) (lookForAdds flags) False (diffAlgorithm flags)

externalMerge :: Config -> O.ExternalMerge
externalMerge = parseFlags O.useExternalMerge

-- | This will become dis-entangled as soon as we inline these functions.
wantGuiPause :: Config -> O.WantGuiPause
wantGuiPause fs = if (hasDiffCmd fs || hasExternalMerge fs) && hasPause fs then O.YesWantGuiPause else O.NoWantGuiPause
  where
    hasDiffCmd = isJust . O._diffCmd . parseFlags O.extDiff
    hasExternalMerge = (/= O.NoExternalMerge) . parseFlags O.useExternalMerge
    hasPause = (== O.YesWantGuiPause) . parseFlags O.pauseForGui

-- | Non-trivial interaction between options. Explicit @-i@ or @-a@ dominates,
-- else @--count@, @--xml@, or @--dry-run@ imply @-a@, else use the def argument.
isInteractive :: Bool -> Config -> Bool
isInteractive def = oparse (O.dryRunXml ^ O.changesFormat ^ O.interactive) decide
  where
    decide :: O.DryRun -> O.XmlOutput -> Maybe O.ChangesFormat -> Maybe Bool -> Bool
    decide _           _        _                     (Just True)  = True
    decide _           _        _                     (Just False) = False
    decide _           _        (Just O.CountPatches) Nothing      = False
    decide _           O.YesXml _                     Nothing      = False
    decide O.YesDryRun _        _                     Nothing      = False
    decide _           _        _                     Nothing      = def

maxCount :: Config -> Maybe Int
maxCount = parseFlags O.matchMaxcount

willRemoveLogFile :: Config -> Bool
willRemoveLogFile = O._rmlogfile . parseFlags O.logfile

isUnified :: Config -> O.WithContext
isUnified = parseFlags O.withContext

doHappyForwarding :: Config -> Bool
doHappyForwarding = parseFlags O.happyForwarding

includeBoring :: Config -> Bool
includeBoring = parseFlags O.includeBoring

doAllowCaseOnly :: Config -> Bool
doAllowCaseOnly = parseFlags O.allowCaseDifferingFilenames

doAllowWindowsReserved :: Config -> Bool
doAllowWindowsReserved = parseFlags O.allowWindowsReservedFilenames

doReverse :: Config -> Bool
doReverse = parseFlags O.changesReverse

usePacks :: Config -> Bool
usePacks = parseFlags O.usePacks

showChangesOnlyToFiles :: Config -> Bool
showChangesOnlyToFiles = parseFlags O.onlyToFiles

removeFromAmended :: Config -> Bool
removeFromAmended = parseFlags O.amendUnrecord

toMatchFlags :: Config -> [MF.MatchFlag]
toMatchFlags = parseFlags O.matchAny

verbosity :: Config -> O.Verbosity
verbosity = parseFlags O.verbosity

useCache :: Config -> O.UseCache
useCache = parseFlags O.useCache

umask :: Config -> O.UMask
umask = parseFlags O.umask

dryRun :: Config -> O.DryRun
dryRun = parseFlags O.dryRun

runPatchIndex :: Config -> O.WithPatchIndex
runPatchIndex = parseFlags O.patchIndex

lookForAdds :: Config -> O.LookForAdds
lookForAdds = O.adds . parseFlags O.lookfor

lookForReplaces :: Config -> O.LookForReplaces
lookForReplaces = O.replaces . parseFlags O.lookfor

diffAlgorithm :: Config -> O.DiffAlgorithm
diffAlgorithm = parseFlags O.diffAlgorithm

lookForMoves :: Config -> O.LookForMoves
lookForMoves = O.moves . parseFlags O.lookfor

runTest :: Config -> O.RunTest
runTest = parseFlags O.test

testChanges :: Config -> O.TestChanges
testChanges = parseFlags O.testChanges

setScriptsExecutable :: Config -> O.SetScriptsExecutable
setScriptsExecutable = parseFlags O.setScriptsExecutable

withWorkingDir :: Config -> O.WithWorkingDir
withWorkingDir = parseFlags O.useWorkingDir

leaveTestDir :: Config -> O.LeaveTestDir
leaveTestDir = parseFlags O.leaveTestDir

remoteRepos :: Config -> O.RemoteRepos
remoteRepos = parseFlags O.remoteRepos

setDefault :: Bool -> Config -> O.SetDefault
setDefault defYes = maybe def noDef . parseFlags O.setDefault where
  def = if defYes then O.YesSetDefault False else O.NoSetDefault False
  noDef yes = if yes then O.YesSetDefault True else O.NoSetDefault True

cloneKind :: Config -> O.CloneKind
cloneKind = parseFlags O.partial

workRepo :: Config -> O.WorkRepo
workRepo = parseFlags O.workRepo

allowConflicts :: Config -> O.AllowConflicts
allowConflicts = maybe O.NoAllowConflicts id . parseFlags (O.conflicts O.NoAllowConflicts)

-- | Ugly. The alternative is to put the remoteRepos accessor into the IO monad,
-- which is hardly better.
fixRemoteRepos :: AbsolutePath -> Config -> IO Config
fixRemoteRepos d = mapM fixRemoteRepo where
  fixRemoteRepo (F.RemoteRepo p) = F.RemoteRepo `fmap` fixUrl d p
  fixRemoteRepo f = return f

-- | 'fixUrl' takes a String that may be a file path or a URL.
-- It returns either the URL, or an absolute version of the path.
fixUrl :: AbsolutePath -> String -> IO String
fixUrl d f = if isValidLocalPath f
                then toFilePath `fmap` withCurrentDirectory d (ioAbsolute f)
                else return f

-- | @maybeFixSubPaths files@ tries to turn the file paths in its argument into
-- @SubPath@s.
--
-- When converting a relative path to an absolute one, this function first tries
-- to interpret the relative path with respect to the current working directory.
-- If that fails, it tries to interpret it with respect to the repository
-- directory. Only when that fails does it put a @Nothing@ in the result at the
-- position of the path that cannot be converted.
--
-- It is intended for validating file arguments to darcs commands.
maybeFixSubPaths :: (AbsolutePath, AbsolutePath) -> [FilePath] -> IO [Maybe SubPath]
maybeFixSubPaths (r, o) fs = withCurrentDirectory o $ do
  fixedFs <- mapM fixit fs
  let bads = snd . unzip . filter (isNothing . fst) $ zip fixedFs fs
  unless (null bads) . putStrLn $ "Ignoring non-repository paths: " ++
    intercalate ", " bads
  return fixedFs
 where
    fixit p = do ap <- ioAbsolute p
                 case makeSubPathOf r ap of
                   Just sp -> return $ Just sp
                   Nothing -> withCurrentDirectory r $ do
                     absolutePathByRepodir <- ioAbsolute p
                     return $ makeSubPathOf r absolutePathByRepodir

-- | @fixSubPaths files@ returns the @SubPath@s for the paths in @files@ that
-- are inside the repository, preserving their order. Paths in @files@ that are
-- outside the repository directory are not in the result.
--
-- When converting a relative path to an absolute one, this function first tries
-- to interpret the relative path with respect to the current working directory.
-- If that fails, it tries to interpret it with respect to the repository
-- directory. Only when that fails does it omit the path from the result.
--
-- It is intended for validating file arguments to darcs commands.
fixSubPaths :: (AbsolutePath, AbsolutePath) -> [FilePath] -> IO [SubPath]
fixSubPaths fps fs = nub . catMaybes <$> maybeFixSubPaths fps
    (filter (not . null) fs)

-- | 'getRepourl' takes a list of flags and returns the url of the
-- repository specified by @Repodir \"directory\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--repodir=DIRECTORY@
getRepourl :: Config -> Maybe String
getRepourl fs = case parseFlags O.possiblyRemoteRepo fs of
  Nothing -> Nothing
  Just d -> if not (isValidLocalPath d) then Just d else Nothing

fileHelpAuthor :: [String]
fileHelpAuthor = [
 "Each patch is attributed to its author, usually by email address (for",
 "example, `Fred Bloggs <fred@example.net>`).  Darcs looks in several",
 "places for this author string: the `--author` option, the files",
 "`_darcs/prefs/author` (in the repository) and `" ++ globalPrefsDirDoc ++ "author` (in your",
 "home directory), and the environment variables `$DARCS_EMAIL` and",
 "`$EMAIL`.  If none of those exist, Darcs will prompt you for an author",
 "string and write it to `" ++ globalPrefsDirDoc ++ "author`.  Note that if you have more",
 "than one email address, you can put them all in `" ++ globalPrefsDirDoc ++ "author`,",
 "one author per line.  Darcs will still prompt you for an author, but it",
 "allows you to select from the list, or to type in an alternative."
 ]

environmentHelpEmail :: ([String], [String])
environmentHelpEmail = (["DARCS_EMAIL","EMAIL"], fileHelpAuthor)

-- | 'getAuthor' takes a list of flags and returns the author of the
-- change specified by @Author \"Leo Tolstoy\"@ in that list of flags, if any.
-- Otherwise, if @Pipe@ is present, asks the user who is the author and
-- returns the answer. If neither are present, try to guess the author,
-- from repository or global preference files or environment variables,
-- and if it's not possible, ask the user.
getAuthor :: Maybe String -> Bool -> IO String
getAuthor (Just author) _ = return author
getAuthor Nothing pipe = if pipe then askUser "Who is the author? " else promptAuthor True False

-- | 'promptAuthor' try to guess the author, from repository or
-- global preference files or environment variables, and
-- if it's not possible or alwaysAsk parameter is true, ask the user.
-- If store parameter is true, the new author is added into
-- @_darcs/prefs@.
promptAuthor :: Bool -- Store the new author
             -> Bool -- Author selection even if already stored
             -> IO String
promptAuthor store alwaysAsk = do
  as <- getEasyAuthor
  case as of
    [a] -> if alwaysAsk then
             askForAuthor (fancyPrompt as) (fancyPrompt as)
           else return a
    []  -> askForAuthor shortPrompt longPrompt
    _   -> askForAuthor (fancyPrompt as) (fancyPrompt as)
 where
  shortPrompt = askUser "What is your email address? "
  longPrompt  = askUser "What is your email address (e.g. Fred Bloggs <fred@example.net>)? "
  fancyPrompt xs =
    do putDocLn $ text "" $$
                  text "You have saved the following email addresses to your global settings:"
       str <- askUserListItem "Please select an email address for this repository: " (xs ++ ["Other"])
       if str == "Other"
          then longPrompt
          else return str
  askForAuthor askfn1 askfn2 = do
      aminrepo <- doesDirectoryExist (darcsdir++"/prefs")
      if aminrepo && store then do
          putDocLn $
            text "Each patch is attributed to its author, usually by email address (for" $$
            text "example, `Fred Bloggs <fred@example.net>').  Darcs could not determine" $$
            text "your email address, so you will be prompted for it." $$
            text "" $$
            text ("Your address will be stored in " ++ globalPrefsDirDoc ++ "author") $$
            text "It will be used for all patches you record in ALL repositories." $$
            text ("If you move that file to " ++ darcsdir </> "prefs" </> "author, it will") $$
            text "be used for patches recorded in this repository only."
          add <- askfn1
          maybeprefsdir <- globalPrefsDir
          prefsdir <- case maybeprefsdir of
            Nothing -> do
                    putStrLn "WARNING: Global preference directory could not be found."
                    return $ darcsdir </> "prefs"
            Just dir -> do exists <- doesDirectoryExist dir
                           unless exists $ createDirectory dir
                           return dir
          writeLocaleFile (prefsdir </> "author") $
                          unlines ["# " ++ line | line <- fileHelpAuthor] ++ "\n" ++ add
          return add
        else askfn2

-- | 'getEasyAuthor' tries to get the author name first from the repository preferences,
-- then from global preferences, then from environment variables.  Returns @[]@
-- if it could not get it.  Note that it may only return multiple possibilities when
-- reading from global preferences
getEasyAuthor :: IO [String]
getEasyAuthor =
  firstNotNullIO [ (take 1 . nonblank) `fmap` getPreflist "author"
                 , nonblank    `fmap` getGlobal "author"
                 , maybeToList `fmap` maybeGetEnv "DARCS_EMAIL"
                 , maybeToList `fmap` maybeGetEnv "EMAIL"
                 ] >>= mapM decodeString
 where
  nonblank = filter (not . null)
  -- this could perhaps be simplified with Control.Monad
  -- but note that we do NOT want to concatenate the results
  firstNotNullIO [] = return []
  firstNotNullIO (e:es) = do
    v <- e `catchall` return []
    if null v then firstNotNullIO es else return v

getDate :: Bool -> IO String
getDate hasPipe = if hasPipe then cleanLocalDate =<< askUser "What is the date? "
                  else getIsoDateTime

environmentHelpSendmail :: ([String], [String])
environmentHelpSendmail = (["SENDMAIL"], [
 "On Unix, the `darcs send` command relies on sendmail(8).  The",
 "`--sendmail-command` or $SENDMAIL environment variable can be used to",
 "provide an explicit path to this program; otherwise the standard",
 "locations /usr/sbin/sendmail and /usr/lib/sendmail will be tried."])
-- FIXME: mention the following also:
-- * sendmail(8) is not sendmail-specific;
-- * nowadays, desktops often have no MTA or an unconfigured MTA --
--   which is awful, because it accepts mail but doesn't relay it;
-- * in this case, can be a sendmail(8)-emulating wrapper on top of an
--   MUA that sends mail directly to a smarthost; and
-- * on a multi-user system without an MTA and on which you haven't
--   got root, can be msmtp.

-- |'getSendmailCmd' takes a list of flags and returns the sendmail command
-- to be used by @darcs send@. Looks for a command specified by
-- @SendmailCmd \"command\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--sendmail-command=COMMAND@
-- Alternatively the user can set @$S@@ENDMAIL@ which will be used as a fallback if present.
getSendmailCmd :: Config -> IO String
getSendmailCmd fs = case parseFlags O.sendmailCmd fs of
  Just cmd -> return cmd
  Nothing -> fmap (maybe "" id) $ firstJustIO [ maybeGetEnv "SENDMAIL" ]

-- | Accessor for output option
getOutput :: Config -> FilePath -> Maybe AbsolutePathOrStd
getOutput fs fp = fmap go (parseFlags O.output fs) where
  go (O.Output ap)         = ap
  go (O.OutputAutoName ap) = makeAbsoluteOrStd ap fp

getCharset :: Config -> Maybe String
getCharset = parseFlags O.charset

-- |'getSubject' takes a list of flags and returns the subject of the mail
-- to be sent by @darcs send@. Looks for a subject specified by
-- @Subject \"subject\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--subject=SUBJECT@
getSubject :: Config -> Maybe String
getSubject = O._subject . parseFlags O.headerFields

-- |'getCc' takes a list of flags and returns the addresses to send a copy of
-- the patch bundle to when using @darcs send@.
-- looks for a cc address specified by @Cc \"address\"@ in that list of flags.
-- Returns the addresses as a comma separated string.
getCc :: Config -> String
getCc = intercalate " , " . O._cc . parseFlags O.headerFields

getInReplyTo :: Config -> Maybe String
getInReplyTo = O._inReplyTo . parseFlags O.headerFields

getReply :: Config -> Maybe String
getReply = parseFlags O.reply

-- | 'flagsToSiblings' collects the contents of all @Sibling@ flags in a list of flags.
siblings :: Config -> [AbsolutePath]
siblings = parseFlags O.siblings

useIndex :: Config -> O.UseIndex
useIndex = parseFlags O.useIndex

hasSummary :: O.Summary -> Config -> O.Summary
hasSummary def = maybe def id . parseFlags O.summary

hasXmlOutput :: Config -> O.XmlOutput
hasXmlOutput = parseFlags O.xmloutput

selectDeps :: Config -> O.SelectDeps
selectDeps = parseFlags O.selectDeps

hasLogfile :: Config -> Maybe AbsolutePath
hasLogfile = O._logfile . parseFlags O.logfile

hasAuthor :: Config -> Maybe String
hasAuthor = parseFlags O.author

patchFormat :: Config -> O.PatchFormat
patchFormat = parseFlags O.patchFormat

applyAs :: Config -> Maybe String
applyAs = parseFlags O.applyAs
