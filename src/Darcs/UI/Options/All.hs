{-# LANGUAGE RecordWildCards #-}
{- | All the concrete options.

Notes:

  * The term \"option\" refers to a flag or combination of flags that
    together form a part of a command's configuration. Ideally, options
    should be orthogonal to each other, so we can freely combine them.

  * A primitive (indivisible) option has an associate value type.

  * An option named \"xyzActions\" represents a set of flags that act as
    mutually exclusive sub-commands. They typically have a dedicated value
    type named \"XyzAction\".

  * This module is probably best imported qualified. This is in contrast to
    the current practice of using subtly differing names to avoid name
    clashes for closely related items. For instance, the data constructors
    for an option's value type and the corresponding data constructors in
    'F.DarcsFlag' may coincide. This is also why we import "Darcs.UI.Flags"
    qualified here.

  * When the new options system is finally in place, no code other than the
    one for constructing options should directly refer to 'F.DarcsFlag'
    constructors.

-}
module Darcs.UI.Options.All
    ( DarcsOption

    -- root
    , RootAction (..)
    , rootActions

    -- all commands
    , StdCmdAction (..)
    , stdCmdActions
    , debug
    , Verbosity (..) -- re-export
    , verbosity
    , timings
    , anyVerbosity
    , preHook
    , postHook
    , hooks
    , UseCache (..) -- re-export
    , useCache

    -- interactivity
    , XmlOutput (..)
    , xmloutput
    , DryRun (..) -- re-export
    , dryRun
    , dryRunXml
    , interactive
    , pipe
    , WantGuiPause (..) -- re-export
    , pauseForGui
    , askdeps

    -- patch selection
    , module Darcs.UI.Options.Matching -- re-export
    , SelectDeps (..)
    , selectDeps
    , changesReverse
    , matchMaxcount

    -- local or remote repo(s)
    , WorkRepo (..) -- re-export
    , workRepo
    , workingRepoDir
    , RemoteRepos (..) -- re-export
    , remoteRepos
    , possiblyRemoteRepo
    , reponame
    , notInRemote
    , notInRemoteFlagName
    , RepoCombinator (..)
    , repoCombinator
    , allowUnrelatedRepos
    , justThisRepo
    , WithWorkingDir (..) -- re-export
    , useWorkingDir
    , SetDefault (..) -- re-export
    , setDefault

    -- patch meta-data
    , patchname
    , author
    , AskLongComment (..)
    , askLongComment
    , keepDate
    , Logfile (..)
    , logfile

    -- looking for changes
    , LookFor (..)
    , LookForAdds (..) -- re-export
    , LookForMoves (..) -- re-export
    , LookForReplaces (..) -- re-export
    , lookfor

    -- files to consider
    , UseIndex (..) -- re-export
    , ScanKnown (..) -- re-export
    , diffing
    , includeBoring
    , allowProblematicFilenames
    , allowCaseDifferingFilenames
    , allowWindowsReservedFilenames
    , onlyToFiles
    , useIndex
    , recursive

    -- differences
    , DiffAlgorithm (..) -- re-export
    , diffAlgorithm
    , WithContext (..)
    , withContext
    , unidiff
    , ExternalDiff (..)
    , extDiff

    -- tests
    , TestChanges (..)
    , testChanges
    , RunTest (..) -- re-export
    , test
    , LeaveTestDir (..) -- re-export
    , leaveTestDir

    -- mail related
    , HeaderFields (..)
    , headerFields
    , sendToContext
    , sendmail
    , sendmailCmd
    , charset
    , editDescription
    , ccApply
    , reply
    , happyForwarding

    -- patch bundles
    , applyAs
    , Sign (..)
    , sign
    , Verify (..)
    , verify

    -- merging patches
    , AllowConflicts (..) -- re-export
    , conflicts
    , ExternalMerge (..) -- re-export
    , useExternalMerge

    -- optimizations
    , Compression (..) -- re-export
    , compress
    , usePacks
    , WithPatchIndex (..) -- re-export
    , patchIndex
    , patchIndexYes
    , Reorder (..) -- re-export
    , reorder
    , minimize
    , storeInMemory

    -- miscellaneous
    , Output (..)
    , output
    , Summary (..)
    , summary
    , RemoteDarcs (..) -- re-export
    , NetworkOptions (..)
    , network
    , UMask (..) -- re-export
    , umask
    , SetScriptsExecutable (..) -- re-export
    , setScriptsExecutable
    , restrictPaths

    -- command specific

    -- amend
    , amendUnrecord
    , selectAuthor

    -- annotate
    , humanReadable
    , machineReadable

    -- clone
    , CloneKind (..)
    , partial

    -- dist
    , distname
    , distzip

    -- convert import/export, init
    , marks
    , readMarks
    , writeMarks
    , PatchFormat (..)
    , patchFormat
    , hashed

    -- log
    , ChangesFormat (..)
    , changesFormat

    -- replace
    , tokens
    , forceReplace

    -- test
    , TestStrategy (..)
    , testStrategy

    -- show files/index
    , files
    , directories
    , pending
    , nullFlag

    -- gzcrcs
    , GzcrcsAction (..)
    , gzcrcsActions

    -- optimize
    , siblings
    , reorderPatches
    , optimizePatchIndex
    ) where

import Prelude hiding ( (^) )
import Data.Char ( isDigit )
import Data.List ( intercalate )
import Data.Maybe ( listToMaybe )

import Darcs.Repository.Flags
    ( Compression (..)
    , RemoteDarcs (..)
    , Reorder (..)
    , Verbosity (..)
    , UseCache (..)
    , UMask (..)
    , DryRun (..)
    , LookForAdds (..)
    , LookForMoves (..)
    , LookForReplaces (..)
    , DiffAlgorithm (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , UseIndex (..)
    , ScanKnown (..)
    , CloneKind (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , AllowConflicts (..)
    , WantGuiPause (..)
    , WithPatchIndex (..)
    , WithWorkingDir (..)
    )

import qualified Darcs.UI.Options.Flags as F ( DarcsFlag(..) )
import Darcs.UI.Options.Core
import Darcs.UI.Options.Iso
import Darcs.UI.Options.Util
import Darcs.UI.Options.Matching

-- * Type instantiations

-- | 'DarcsOption' instantiates the first two type parameters of 'OptSpec' to
-- what we need in darcs. The first parameter is instantiated to
-- The flag type is instantiate to 'Flag'.
type DarcsOption = OptSpec DarcsOptDescr Flag

type RawDarcsOption = forall v. v -> RawOptSpec Flag v

-- * Root command

-- | Options for darcs iself that act like sub-commands.
data RootAction = RootHelp | Version | ExactVersion | ListCommands deriving (Eq, Show)

rootActions :: PrimDarcsOption (Maybe RootAction)
rootActions = withDefault Nothing
  [ RawNoArg ['h'] ["help", "overview"] F.Help (Just RootHelp)
    "show a brief description of all darcs commands and top-level options"
  , RawNoArg ['v'] ["version"] F.Version  (Just Version) "show the darcs version"
  , RawNoArg [] ["exact-version"] F.ExactVersion (Just ExactVersion)
    "show the exact darcs version"
    -- the switch --commands is here for compatibility only
  , RawNoArg [] ["commands","list-options"] F.ListCommands (Just ListCommands)
    "show plain list of available options and commands, for auto-completion"
  ]

-- * Common to all commands

-- ** Standard command actions

data StdCmdAction = Help | ListOptions | Disable deriving (Eq, Show)

stdCmdActions :: PrimDarcsOption (Maybe StdCmdAction)
stdCmdActions = withDefault Nothing
  [ RawNoArg [] ["help"] F.Help (Just Help)
    "show a brief description of the command and its options"
  , RawNoArg [] ["list-options"] F.ListOptions (Just ListOptions)
    "show plain list of available options and commands, for auto-completion"
  , RawNoArg [] ["disable"] F.Disable (Just Disable) "disable this command" ]

-- ** Verbosity related

debug :: PrimDarcsOption Bool
debug = singleNoArg [] ["debug"] F.Debug "give only debug output"

debugHttp :: PrimDarcsOption Bool
debugHttp = singleNoArg [] ["debug-http"] F.DebugHTTP "debug output from libcurl"

verbosity :: PrimDarcsOption Verbosity
verbosity = withDefault NormalVerbosity
  [ RawNoArg ['q'] ["quiet"] F.Quiet Quiet "suppress informational output"
  , RawNoArg [] ["standard-verbosity"] F.NormalVerbosity NormalVerbosity
      "neither verbose nor quiet output"
  , RawNoArg ['v'] ["verbose"] F.Verbose Verbose "give verbose output" ]

timings :: PrimDarcsOption Bool
timings = singleNoArg [] ["timings"] F.Timings "provide debugging timings information"

anyVerbosity :: DarcsOption a (Bool -> Bool -> Verbosity -> Bool -> a)
anyVerbosity = debug ^ debugHttp ^ verbosity ^ timings where

-- ** Hooks

hooks :: DarcsOption a (Maybe String -> Bool -> Maybe String -> Bool -> a)
hooks = preHook ^ postHook

preHook :: DarcsOption a (Maybe String -> Bool -> a)
preHook = prehookCmd ^ hookPrompt "prehook" F.AskPrehook F.RunPrehook

postHook :: DarcsOption a (Maybe String -> Bool -> a)
postHook = posthookCmd ^ hookPrompt "posthook" F.AskPosthook F.RunPosthook

prehookCmd :: PrimDarcsOption (Maybe String)
prehookCmd = withDefault Nothing
    [ RawStrArg [] ["prehook"] F.PrehookCmd unF Just unV
      "COMMAND" "specify command to run before this darcs command"
    , RawNoArg [] ["no-prehook"] F.NoPrehook Nothing
      "don't run prehook command" ]
  where unF f = [ s | F.PrehookCmd s <- [f] ]
        unV v = [ s | Just s <- [v] ]

posthookCmd :: PrimDarcsOption (Maybe String)
posthookCmd = withDefault Nothing
    [ RawStrArg [] ["posthook"] F.PosthookCmd unF Just unV "COMMAND"
      "specify command to run after this darcs command"
    , RawNoArg [] ["no-posthook"] F.NoPosthook Nothing
      "don't run posthook command" ]
  where unF f = [ s | F.PosthookCmd s <- [f] ]
        unV v = [ s | Just s <- [v] ]

hookPrompt :: String -> Flag -> Flag -> PrimDarcsOption Bool
hookPrompt name fask frun = withDefault False
  [ RawNoArg [] ["prompt-"++name] fask True
    ("prompt before running "++name)
  , RawNoArg [] ["run-"++name] frun False
    ("run "++name++" command without prompting") ]

-- ** Misc

useCache :: PrimDarcsOption UseCache
useCache = (imap . cps) (Iso fw bw) $ singleNoArg [] ["no-cache"] F.NoCache "don't use patch caches"
  where
    fw True = NoUseCache
    fw False = YesUseCache
    bw NoUseCache = True
    bw YesUseCache = False

-- * Interactivity related

{- TODO: these options interact (no pun intended) in complex ways that are
very hard to figure out for users as well as maintainers. I think the only
solution here is a more radical (and probably incompatible) re-design
involving all interactivity related options. That is beyond the goals of
this sub-project (which is already large enough).
-}

data XmlOutput = NoXml | YesXml deriving (Eq, Show)

xmloutput :: PrimDarcsOption XmlOutput
xmloutput = withDefault NoXml [__xmloutput YesXml]

__xmloutput :: RawDarcsOption
__xmloutput val = RawNoArg [] ["xml-output"] F.XMLOutput val "generate XML formatted output"

-- | NOTE: I'd rather work to have no uses of dryRunNoxml, so that any time
-- --dry-run is a possibility, automated users can examine the results more
-- easily with --xml.
--
-- See also issue2397.
dryRun :: PrimDarcsOption DryRun
dryRun = (imap . cps) (Iso fw bw) $ singleNoArg [] ["dry-run"] F.DryRun "don't actually take the action"
  where
    fw True = YesDryRun
    fw False = NoDryRun
    bw YesDryRun = True
    bw NoDryRun = False

dryRunXml :: DarcsOption a (DryRun -> XmlOutput -> a)
dryRunXml = dryRun ^ xmloutput

__dryrun :: RawDarcsOption
__dryrun val = RawNoArg [] ["dry-run"] F.DryRun val "don't actually take the action"

pipe :: PrimDarcsOption Bool
pipe = singleNoArg [] ["pipe"] F.Pipe "ask user interactively for the patch metadata"

interactive :: PrimDarcsOption (Maybe Bool)
interactive = withDefault Nothing
  [ RawNoArg ['a'] ["all","no-interactive"] F.All (Just False) "answer yes to all patches"
  , RawNoArg ['i'] ["interactive"] F.Interactive (Just True) "prompt user interactively" ]

pauseForGui :: PrimDarcsOption WantGuiPause
pauseForGui = withDefault YesWantGuiPause
  [ RawNoArg [] ["pause-for-gui"] F.PauseForGui YesWantGuiPause
    "pause for an external diff or merge command to finish"
  , RawNoArg [] ["no-pause-for-gui"] F.NoPauseForGui NoWantGuiPause
    "return immediately after external diff or merge command finishes" ]

askdeps :: PrimDarcsOption Bool
askdeps = withDefault False
  [ RawNoArg [] ["ask-deps"] F.AskDeps True "manually select dependencies"
  , RawNoArg [] ["no-ask-deps"] F.NoAskDeps False "automatically select dependencies" ]

-- * Patch selection related

data SelectDeps = NoDeps | AutoDeps | PromptDeps deriving (Eq, Show)

selectDeps :: PrimDarcsOption SelectDeps
selectDeps = withDefault PromptDeps
  [ RawNoArg [] ["no-deps"] F.DontGrabDeps NoDeps
    "don't automatically fulfill dependencies"
  , RawNoArg [] ["auto-deps","dont-prompt-for-dependencies"] F.DontPromptForDependencies AutoDeps
    "don't ask about patches that are depended on by matched patches (with --match or --patch)"
  , RawNoArg [] ["prompt-deps","prompt-for-dependencies"] F.PromptForDependencies PromptDeps
    "prompt about patches that are depended on by matched patches" ]

changesReverse :: PrimDarcsOption Bool
changesReverse = withDefault False
  [ RawNoArg [] ["reverse"] F.Reverse True "show/consider changes in reverse order"
  , RawNoArg [] ["no-reverse"] F.Forward False "show/consider changes in the usual order" ]

-- | TODO: Returning @-1@ if the argument cannot be parsed as an integer is
-- not something I expected to find in a Haskell program. Instead, the flag
-- should take either a plain 'String' argument (leaving it to a later stage
-- to parse the 'String' to an 'Int'), or else a @'Maybe' 'Int'@, taking
-- the possibility of a failed parse into account.
matchMaxcount :: PrimDarcsOption (Maybe Int)
matchMaxcount = OptSpec {..} where
  ounparse k (Just n) = k [ F.MaxCount n ]
  ounparse k Nothing = k []
  oparse k fs = k $ listToMaybe [ s | F.MaxCount s <- fs ]
  ocheck fs = case [ "--max-count="++show n | F.MaxCount n <- fs ] of
    cfs@(_:_:_) -> ["conflicting flags: " ++ intercalate ", " cfs]
    _ -> []
  odesc = [ strArg [] ["max-count"] (F.MaxCount . toInt) "NUMBER"
    "return only NUMBER results" ]
  toInt s = if not (null s) && all isDigit s then read s else (-1)

-- * Local or remote repo

workRepo :: PrimDarcsOption WorkRepo
workRepo = imap (Iso fw bw) $ workingRepoDir ^ possiblyRemoteRepo where
  fw k (WorkRepoDir s)         = k (Just s) Nothing
  fw k (WorkRepoPossibleURL s) = k Nothing  (Just s)
  fw k WorkRepoCurrentDir      = k Nothing  Nothing
  bw k (Just s) _              = k (WorkRepoDir s)
  bw k Nothing  (Just s)       = k (WorkRepoPossibleURL s)
  bw k Nothing  Nothing        = k WorkRepoCurrentDir

workingRepoDir :: PrimDarcsOption (Maybe String)
workingRepoDir = singleStrArg [] ["repodir"] F.WorkRepoDir arg "DIRECTORY"
    "specify the repository directory in which to run"
  where arg (F.WorkRepoDir s) = Just s
        arg _ = Nothing

-- | @--repodir@ is there for compatibility, should be removed eventually
--
-- IMHO the whole option can disappear; it overlaps with using an extra (non-option)
-- argument, which is how e.g. @darcs get@ is usually invoked.
reponame :: PrimDarcsOption (Maybe String)
reponame = singleStrArg [] ["repo-name","repodir"] F.NewRepo arg "DIRECTORY" "path of output directory"
  where arg (F.NewRepo s) = Just s; arg _ = Nothing

possiblyRemoteRepo :: PrimDarcsOption (Maybe String)
possiblyRemoteRepo = singleStrArg [] ["repo"] F.WorkRepoUrl arg "URL"
    "specify the repository URL"
  where arg (F.WorkRepoUrl s) = Just s
        arg _ = Nothing

remoteRepos :: PrimDarcsOption RemoteRepos
remoteRepos = (imap . cps) (Iso fw bw) $ multiStrArg [] ["remote-repo"] F.RemoteRepo mkV "URL"
    "specify the remote repository URL to work with"
  where mkV fs = [ s | F.RemoteRepo s <- fs ]
        fw ss = RemoteRepos ss
        bw (RemoteRepos ss) = ss

notInRemoteFlagName :: String
notInRemoteFlagName = "not-in-remote"

notInRemote :: PrimDarcsOption [Maybe String]
notInRemote =
    multiOptStrArg [] [notInRemoteFlagName] F.NotInRemote args "URL/PATH" $
        "select all patches not in the default push/pull repository or at "
        ++ "location URL/PATH"
  where
    args fs = [s | F.NotInRemote s <- fs]

data RepoCombinator = Intersection | Union | Complement deriving (Eq, Show)

repoCombinator :: PrimDarcsOption RepoCombinator
repoCombinator = withDefault Union
  [ RawNoArg [] ["intersection"] F.Intersection Intersection
    "take intersection of all repositories"
  , RawNoArg [] ["union"] F.Union Union
    "take union of all repositories"
  , RawNoArg [] ["complement"] F.Complement Complement
    "take complement of repositories (in order listed)" ]

allowUnrelatedRepos :: PrimDarcsOption Bool
allowUnrelatedRepos = singleNoArg [] ["ignore-unrelated-repos"] F.AllowUnrelatedRepos
  "do not check if repositories are unrelated"

justThisRepo :: PrimDarcsOption Bool
justThisRepo = singleNoArg [] ["just-this-repo"] F.JustThisRepo
  "Limit the check or repair to the current repo"

-- | convert, clone, init
useWorkingDir :: PrimDarcsOption WithWorkingDir
useWorkingDir = withDefault WithWorkingDir
  [ RawNoArg [] ["with-working-dir"] F.UseWorkingDir WithWorkingDir
    "Create a working directory (normal repository)"
  , RawNoArg [] ["no-working-dir"] F.UseNoWorkingDir NoWorkingDir
    "Do not create a working directory (bare repository)" ]

setDefault :: PrimDarcsOption (Maybe Bool)
setDefault = withDefault Nothing
  [ RawNoArg [] ["set-default"] F.SetDefault (Just True) "set default repository"
  , RawNoArg [] ["no-set-default"] F.NoSetDefault (Just False) "don't set default repository" ]

-- * Specifying patch meta-data

patchname :: PrimDarcsOption (Maybe String)
patchname = singleStrArg ['m'] ["name"] F.PatchName arg "PATCHNAME"
    "name of patch"
  where arg (F.PatchName s) = Just s
        arg _ = Nothing

author :: PrimDarcsOption (Maybe String)
author = singleStrArg ['A'] ["author"] F.Author arg
    "EMAIL" "specify author id"
  where arg (F.Author s) = Just s
        arg _ = Nothing

data AskLongComment = NoEditLongComment | YesEditLongComment | PromptLongComment
  deriving (Eq, Show)

-- TODO: fix non-default behavior
askLongComment :: PrimDarcsOption (Maybe AskLongComment)
askLongComment = withDefault Nothing
  [ RawNoArg [] ["edit-long-comment"] F.EditLongComment (Just YesEditLongComment)
    "edit the long comment by default"
  , RawNoArg [] ["skip-long-comment"] F.NoEditLongComment (Just NoEditLongComment)
    "don't give a long comment"
  , RawNoArg [] ["prompt-long-comment"] F.PromptLongComment (Just PromptLongComment)
    "prompt for whether to edit the long comment" ]

keepDate :: PrimDarcsOption Bool
keepDate = withDefault False
  [ RawNoArg [] ["keep-date"] F.KeepDate True
   "keep the date of the original patch"
  , RawNoArg [] ["no-keep-date"] F.NoKeepDate False
   "use the current date for the amended patch" ]

-- record, send
data Logfile = Logfile
  { _logfile :: Maybe AbsolutePath
  , _rmlogfile :: Bool
  }

logfile :: PrimDarcsOption Logfile
logfile = imap (Iso fw bw) (__logfile ^ __rmlogfile) where
  fw k (Logfile x y) = k x y
  bw k x y = k (Logfile x y)

__logfile :: PrimDarcsOption (Maybe AbsolutePath)
__logfile = singleAbsPathArg [] ["logfile"] F.LogFile arg "FILE"
    "give patch name and comment in file"
  where arg (F.LogFile s) = Just s
        arg _ = Nothing

__rmlogfile :: PrimDarcsOption Bool
__rmlogfile = withDefault False
  [ RawNoArg [] ["delete-logfile"] F.RmLogFile True
    "delete the logfile when done"
  , RawNoArg [] ["no-delete-logfile"] F.DontRmLogFile False
    "keep the logfile when done" ]

-- * Looking for changes

data LookFor = LookFor
  { adds :: LookForAdds
  , replaces :: LookForReplaces
  , moves :: LookForMoves
  }

lookfor :: PrimDarcsOption LookFor
lookfor = imap (Iso fw bw) (lookforadds ^ lookforreplaces ^ lookformoves) where
  fw k (LookFor a r m) = k a r m
  bw k a r m = k (LookFor a r m)

lookforadds :: PrimDarcsOption LookForAdds
lookforadds = withDefault NoLookForAdds
  [ RawNoArg ['l'] ["look-for-adds"] F.LookForAdds YesLookForAdds
    "look for (non-boring) files that could be added"
  , RawNoArg [] ["dont-look-for-adds","no-look-for-adds"] F.NoLookForAdds NoLookForAdds
    "don't look for any files that could be added" ]

lookforreplaces :: PrimDarcsOption LookForReplaces
lookforreplaces = withDefault NoLookForReplaces
  [ RawNoArg [] ["look-for-replaces"] F.LookForReplaces YesLookForReplaces
    "look for replaces that could be marked"
  , RawNoArg [] ["dont-look-for-replaces","no-look-for-replaces"]
    F.NoLookForReplaces NoLookForReplaces
    "don't look for any replaces" ]

lookformoves :: PrimDarcsOption LookForMoves
lookformoves = withDefault NoLookForMoves
  [ RawNoArg [] ["look-for-moves"] F.LookForMoves YesLookForMoves
   "look for files that may be moved/renamed"
  , RawNoArg [] ["dont-look-for-moves","no-look-for-moves"]
    F.NoLookForMoves NoLookForMoves
   "don't look for any files that could be moved/renamed" ]

-- * Files to consider

diffing :: PrimDarcsOption (UseIndex, ScanKnown, DiffAlgorithm)
diffing = imap (Iso curry3 uncurry3) $ useIndex ^ scanKnown ^ diffAlgorithm
  where
    uncurry3 k x y z = k (x,y,z)
    curry3 k (x,y,z) = k x y z

useIndex :: PrimDarcsOption UseIndex
useIndex = (imap . cps) (Iso fw bw) ignoreTimes where
  fw False = UseIndex
  fw True = IgnoreIndex
  bw UseIndex = False
  bw IgnoreIndex = True

scanKnown :: PrimDarcsOption ScanKnown
scanKnown = imap (Iso fw bw) $ lookforadds ^ includeBoring where
  fw k ScanKnown = k NoLookForAdds False
  fw k ScanAll = k YesLookForAdds False
  fw k ScanBoring = k YesLookForAdds True
  bw k NoLookForAdds _ = k ScanKnown
  bw k YesLookForAdds False = k ScanAll
  bw k YesLookForAdds True = k ScanBoring

includeBoring :: PrimDarcsOption Bool
includeBoring = withDefault False
  [ RawNoArg [] ["boring"] F.Boring True "don't skip boring files"
  , RawNoArg [] ["no-boring"] F.SkipBoring False "skip boring files" ]

allowProblematicFilenames :: DarcsOption a (Bool -> Bool -> a)
allowProblematicFilenames = allowCaseDifferingFilenames ^ allowWindowsReservedFilenames

allowCaseDifferingFilenames :: PrimDarcsOption Bool
allowCaseDifferingFilenames = withDefault False
  [ RawNoArg [] ["case-ok"] F.AllowCaseOnly True
    "don't refuse to add files differing only in case"
  , RawNoArg [] ["no-case-ok"] F.DontAllowCaseOnly False
    "refuse to add files whose name differ only in case" ]

allowWindowsReservedFilenames :: PrimDarcsOption Bool
allowWindowsReservedFilenames = withDefault False
  [ RawNoArg [] ["reserved-ok"] F.AllowWindowsReserved True
    "don't refuse to add files with Windows-reserved names"
  , RawNoArg [] ["no-reserved-ok"] F.DontAllowWindowsReserved False
    "refuse to add files with Windows-reserved names" ]

-- | TODO: see issue2395
onlyToFiles :: PrimDarcsOption Bool
onlyToFiles = withDefault False
  [ RawNoArg [] ["only-to-files"] F.OnlyChangesToFiles True
    "show only changes to specified files"
  , RawNoArg [] ["no-only-to-files"] F.ChangesToAllFiles False
    "show changes to all files" ]

ignoreTimes :: PrimDarcsOption Bool
ignoreTimes = withDefault False
  [ RawNoArg [] ["ignore-times"] F.IgnoreTimes True
    "don't trust the file modification times"
  , RawNoArg [] ["no-ignore-times"] F.DontIgnoreTimes False
    "trust modification times to find modified files" ]

recursive :: PrimDarcsOption Bool
recursive = withDefault False
  [ RawNoArg ['r'] ["recursive"] F.Recursive True "recurse into subdirectories"
  , RawNoArg [] ["not-recursive","no-recursive"] F.NoRecursive False ("don't recurse into subdirectories") ]

-- * Differences

diffAlgorithm :: PrimDarcsOption DiffAlgorithm
diffAlgorithm = withDefault PatienceDiff
  [ RawNoArg [] ["myers"] F.UseMyersDiff MyersDiff
    "use myers diff algorithm"
  , RawNoArg [] ["patience"] F.UsePatienceDiff PatienceDiff
    "use patience diff algorithm" ]

data WithContext = NoContext | YesContext deriving (Eq, Show)

withContext :: PrimDarcsOption WithContext
withContext = (imap . cps) (Iso fw bw) $ withDefault False
  [ RawNoArg ['u'] ["unified"] F.Unified True
    "output changes in a darcs-specific format similar to diff -u"
  , RawNoArg  [] ["no-unified"] F.NonUnified False
    "output changes in darcs' usual format" ]
  where fw False = NoContext
        fw True = YesContext
        bw NoContext = False
        bw YesContext = True

unidiff :: PrimDarcsOption Bool
unidiff = withDefault True
  [ RawNoArg ['u'] ["unified"] F.Unified True "pass -u option to diff"
  , RawNoArg  [] ["no-unified"] F.NonUnified False "output patch in diff's dumb format" ]

data ExternalDiff = ExternalDiff { _diffCmd :: Maybe String, _diffOpts :: [String] } deriving (Eq, Show)

extDiff :: PrimDarcsOption ExternalDiff
extDiff = imap (Iso fw bw) $ extDiffCmd ^ extDiffOpts where
  fw k (ExternalDiff cmd opts) = k cmd opts
  bw k cmd opts = k (ExternalDiff cmd opts)

extDiffCmd :: PrimDarcsOption (Maybe String)
extDiffCmd = singleStrArg [] ["diff-command"] F.DiffCmd arg "COMMAND"
    "specify diff command (ignores --diff-opts)"
  where arg (F.DiffCmd s) = Just s
        arg _ = Nothing

extDiffOpts :: PrimDarcsOption [String]
extDiffOpts = multiStrArg [] ["diff-opts"] F.DiffFlags mkV "OPTIONS"
    "options to pass to diff"
  where mkV fs = [ s | F.DiffFlags s <- fs ]

-- * Runnign tests

data TestChanges = NoTestChanges | YesTestChanges LeaveTestDir deriving (Eq)

testChanges :: PrimDarcsOption TestChanges
testChanges = imap (Iso fw bw) $ test ^ leaveTestDir where
  fw k NoTestChanges = k NoRunTest {- undefined -} YesLeaveTestDir
  fw k (YesTestChanges ltd) = k YesRunTest ltd
  bw k NoRunTest _ = k NoTestChanges
  bw k YesRunTest ltd = k (YesTestChanges ltd)

test :: PrimDarcsOption RunTest
test = withDefault NoRunTest
  [ RawNoArg [] ["test"] F.Test YesRunTest "run the test script"
  , RawNoArg [] ["no-test"] F.NoTest NoRunTest "don't run the test script" ]

leaveTestDir :: PrimDarcsOption LeaveTestDir
leaveTestDir = withDefault YesLeaveTestDir
  [ RawNoArg [] ["leave-test-directory"]
    F.LeaveTestDir YesLeaveTestDir "don't remove the test directory"
  , RawNoArg [] ["remove-test-directory"]
    F.NoLeaveTestDir NoLeaveTestDir "remove the test directory" ]

-- * Mail related

data HeaderFields = HeaderFields
  { _to, _cc :: [String]
  , _from, _subject, _inReplyTo :: Maybe String
  }

headerFields :: PrimDarcsOption HeaderFields
headerFields = imap (Iso fw bw) $ to ^ cc ^ from ^ subject ^ inReplyTo where
  fw k (HeaderFields t f c s i) = k t f c s i
  bw k t f c s i = k (HeaderFields t f c s i)

from :: PrimDarcsOption (Maybe String)
from = singleStrArg [] ["from"] F.Author arg
    "EMAIL" "specify email address"
  where arg (F.Author s) = Just s
        arg _ = Nothing

to :: PrimDarcsOption [String]
to = multiStrArg [] ["to"] F.Target mkV "EMAIL" "specify destination email"
  where mkV fs = [ s | F.Target s <- fs ]

cc :: PrimDarcsOption [String]
cc = multiStrArg [] ["cc"] F.Cc mkV "EMAIL" "mail results to additional EMAIL(s)"
  where mkV fs = [ s | F.Cc s <- fs ]

subject :: PrimDarcsOption (Maybe String)
subject = singleStrArg [] ["subject"] F.Subject arg
    "SUBJECT" "specify mail subject"
  where arg (F.Subject s) = Just s
        arg _ = Nothing

inReplyTo :: PrimDarcsOption (Maybe String)
inReplyTo = singleStrArg [] ["in-reply-to"] F.InReplyTo arg
    "EMAIL" "specify in-reply-to header"
  where arg (F.InReplyTo s) = Just s
        arg _ = Nothing

sendToContext :: PrimDarcsOption (Maybe AbsolutePath)
sendToContext = singleAbsPathArg [] ["context"] F.Context arg "FILENAME"
    "send to context stored in FILENAME"
  where arg (F.Context s) = Just s
        arg _ = Nothing

-- TODO: do something about the nonsensical case (False, Just s)
--
-- Some of the tests actually do this (pass --sendmail-command without
-- passing --mail) and it's unclear if it's deliberate or just a historical
-- accident after the issue2204 changes. We should untangle that and
-- perhaps turn this into a single option with an optional argument.
-- The other question to resolve is the interaction with the 'output'
-- options to darcs send.
sendmailIso :: Iso (Bool -> Maybe String -> a) ((Bool, Maybe String) -> a)
sendmailIso = Iso uncurry curry

sendmail :: PrimDarcsOption (Bool, Maybe String)
sendmail = imap sendmailIso $ mail ^ sendmailCmd

mail :: PrimDarcsOption Bool
mail = singleNoArg [] ["mail"] F.Mail "send patch using sendmail"

sendmailCmd :: PrimDarcsOption (Maybe String)
sendmailCmd = singleStrArg [] ["sendmail-command"] F.SendmailCmd arg "COMMAND"
    "specify sendmail command"
  where arg (F.SendmailCmd s) = Just s
        arg _ = Nothing

minimize :: PrimDarcsOption Bool
minimize = withDefault True
  [ RawNoArg [] ["minimize"] F.Minimize True "minimize context of patch bundle"
  , RawNoArg [] ["no-minimize"] F.NoMinimize False ("don't minimize context of patch bundle") ]

charset :: PrimDarcsOption (Maybe String)
charset = singleStrArg [] ["charset"] F.Charset arg
    "CHARSET" "specify mail charset"
  where arg (F.Charset s) = Just s
        arg _ = Nothing

editDescription :: PrimDarcsOption Bool
editDescription = withDefault True
  [ RawNoArg [] ["edit-description"] F.EditDescription True
    "edit the patch bundle description"
  , RawNoArg [] ["dont-edit-description","no-edit-description"] F.NoEditDescription False
    "don't edit the patch bundle description" ]

-- TODO: turn these two into a combined option

ccApply :: PrimDarcsOption (Maybe String)
ccApply = singleStrArg [] ["cc"] F.Cc arg
    "EMAIL" "mail results to additional EMAIL(s). Requires --reply"
  where arg (F.Cc s) = Just s
        arg _ = Nothing

reply :: PrimDarcsOption (Maybe String)
reply = singleStrArg [] ["reply"] F.Reply arg "FROM"
    "reply to email-based patch using FROM address"
  where arg (F.Reply s) = Just s
        arg _ = Nothing

happyForwarding :: PrimDarcsOption Bool
happyForwarding = withDefault False
  [ RawNoArg [] ["happy-forwarding"] F.HappyForwarding True
    "forward unsigned messages without extra header"
  , RawNoArg [] ["no-happy-forwarding"] F.NoHappyForwarding False
    "don't forward unsigned messages without extra header" ]

-- * Patch bundle related

applyAs :: PrimDarcsOption (Maybe String)
applyAs = withDefault Nothing
  [ RawStrArg [] ["apply-as"] F.ApplyAs unF Just unV "USERNAME"
    "apply patch as another user using sudo"
  , RawNoArg [] ["no-apply-as"] F.NonApply Nothing
    "don't use sudo to apply as another user" ]
  where
    unF f = [ s | F.ApplyAs s <- [f] ]
    unV x = [ s | Just s <- [x] ]

data Sign = NoSign | Sign | SignAs String | SignSSL String deriving (Eq, Show)

sign :: PrimDarcsOption Sign
sign = withDefault NoSign
  [ RawNoArg [] ["sign"] F.Sign Sign "sign the patch with your gpg key"
  , RawStrArg [] ["sign-as"] F.SignAs unFSignAs SignAs unSignAs "KEYID"
    "sign the patch with a given keyid"
  , RawStrArg [] ["sign-ssl"] F.SignSSL  unFSignSSL SignSSL unSignSSL "IDFILE"
    "sign the patch using openssl with a given private key"
  , RawNoArg [] ["dont-sign","no-sign"] F.NoSign NoSign "don't sign the patch" ]
  where unFSignAs f = [ s | F.SignAs s <- [f] ]
        unSignAs v = [ s | SignAs s <- [v] ]
        unFSignSSL f = [ s | F.SignSSL s <- [f] ]
        unSignSSL v = [ s | SignSSL s <- [v] ]

data Verify = NoVerify | VerifyKeyring AbsolutePath | VerifySSL AbsolutePath deriving (Eq, Show)

verify :: PrimDarcsOption Verify
verify = withDefault NoVerify
  [ RawAbsPathArg [] ["verify"] F.Verify unFKeyring VerifyKeyring unVKeyring "PUBRING"
    "verify that the patch was signed by a key in PUBRING"
  , RawAbsPathArg [] ["verify-ssl"] F.VerifySSL unFSSL VerifySSL unVSSL "KEYS"
    "verify using openSSL with authorized keys from file KEYS"
  , RawNoArg [] ["no-verify"] F.NonVerify NoVerify
    "don't verify patch signature" ]
  where
    unFKeyring f = [ s | F.Verify s <- [f] ]
    unVKeyring x = [ s | VerifyKeyring s <- [x] ]
    unFSSL f = [ s | F.VerifySSL s <- [f] ]
    unVSSL x = [ s | VerifySSL s <- [x] ]

-- * Merging patches

-- applyConflictOptions = conflicts NoAllowConflicts
-- pullConflictOptions = conflicts YesAllowConflictsAndMark

conflicts :: AllowConflicts -> PrimDarcsOption (Maybe AllowConflicts)
conflicts def = withDefault (Just def)
  [ RawNoArg [] ["mark-conflicts"]
      F.MarkConflicts (Just YesAllowConflictsAndMark) "mark conflicts"
  , RawNoArg [] ["allow-conflicts"]
      F.AllowConflicts (Just YesAllowConflicts) "allow conflicts, but don't mark them"
--   , RawNoArg [] ["no-resolve-conflicts"]
--       NoAllowConflicts "equivalent to --dont-allow-conflicts, for backwards compatibility"
  , RawNoArg [] ["dont-allow-conflicts","no-allow-conflicts","no-resolve-conflicts"]
      F.NoAllowConflicts (Just NoAllowConflicts) "fail if there are patches that would create conflicts"
  , RawNoArg [] ["skip-conflicts"]
      F.SkipConflicts Nothing "filter out any patches that would create conflicts" ]

-- Technically not an isomorphism, see 'sendmailIso'.
useExternalMerge :: PrimDarcsOption ExternalMerge
useExternalMerge = imap (Iso fw bw) $ singleStrArg [] ["external-merge"] F.ExternalMerge arg
    "COMMAND" "use external tool to merge conflicts"
  where
    arg (F.ExternalMerge s) = Just s
    arg _ = Nothing
    bw k (Just s) = k (YesExternalMerge s)
    bw k Nothing = k NoExternalMerge
    fw k (YesExternalMerge s) = k (Just s)
    fw k NoExternalMerge = k Nothing

-- * Optimizations

compress :: PrimDarcsOption Compression
compress = withDefault GzipCompression
  [ RawNoArg [] ["compress"] F.Compress GzipCompression "compress patch data"
  , RawNoArg [] ["dont-compress","no-compress"] F.NoCompress NoCompression "don't compress patch data" ]

usePacks :: PrimDarcsOption Bool
usePacks = withDefault True
  [ RawNoArg [] ["packs"] F.Packs True "use repository packs"
  , RawNoArg [] ["no-packs"] F.NoPacks False "don't use repository packs" ]

-- for init, clone and convert: patch index disabled by default
patchIndex :: PrimDarcsOption WithPatchIndex
patchIndex = withDefault NoPatchIndex [__patchIndex YesPatchIndex, __noPatchIndex NoPatchIndex]

-- for log and annotate: patch index enabled by default
patchIndexYes :: PrimDarcsOption WithPatchIndex
patchIndexYes = withDefault YesPatchIndex [__patchIndex YesPatchIndex, __noPatchIndex NoPatchIndex]

__patchIndex, __noPatchIndex :: RawDarcsOption
__patchIndex val = RawNoArg [] ["with-patch-index"] F.PatchIndexFlag val "build patch index"
__noPatchIndex val = RawNoArg [] ["no-patch-index"] F.NoPatchIndexFlag val "don't build patch index"

-- diff, dist
storeInMemory :: PrimDarcsOption Bool
storeInMemory = withDefault False
  [ RawNoArg [] ["store-in-memory"] F.StoreInMemory True
    "do patch application in memory rather than on disk"
  , RawNoArg [] ["no-store-in-memory"] F.ApplyOnDisk False
    "do patch application on disk" ]

-- * Output

data Output = Output AbsolutePathOrStd
            | OutputAutoName AbsolutePath
            deriving (Eq, Show)

output :: PrimDarcsOption (Maybe Output)
output = withDefault Nothing
    [ RawAbsPathOrStdArg ['o'] ["output"]
      F.Output unOutputF (Just . Output) unOutput
      "FILE" "specify output filename"
    , RawOptAbsPathArg ['O'] ["output-auto-name"]
      F.OutputAutoName unOutputAutoNameF (Just . OutputAutoName) unOutputAutoName
      "." "DIRECTORY"
      "output to automatically named file in DIRECTORY, default: current directory"
    ]
  where
    unOutputF f = [ p | F.Output p <- [f] ]
    unOutput (Just (Output p)) = [p]
    unOutput _ = []
    unOutputAutoNameF f = [ p | F.OutputAutoName p <- [f] ]
    unOutputAutoName (Just (OutputAutoName p)) = [p]
    unOutputAutoName _ = []

-- * Miscellaneous

data Summary = NoSummary | YesSummary deriving (Eq, Show)

summary :: PrimDarcsOption (Maybe Summary)
summary = withDefault Nothing
  [ RawNoArg ['s'] ["summary"] F.Summary (Just YesSummary) "summarize changes"
  , RawNoArg [] ["no-summary"] F.NoSummary (Just NoSummary) "don't summarize changes" ]

-- | TODO: reconsider this grouping of options
data NetworkOptions = NetworkOptions
  { noHttpPipelining :: Bool
  , remoteDarcs :: RemoteDarcs }

networkIso :: Iso (Bool -> Maybe String -> a) (NetworkOptions -> a)
networkIso = Iso fw bw where
  fw k (NetworkOptions x (RemoteDarcs y)) = k x (Just y)
  fw k (NetworkOptions x DefaultRemoteDarcs) = k x Nothing
  bw k x (Just y) = k (NetworkOptions x (RemoteDarcs y))
  bw k x Nothing = k (NetworkOptions x DefaultRemoteDarcs)

network :: PrimDarcsOption NetworkOptions
network = imap networkIso
  $ singleNoArg [] ["no-http-pipelining"] F.NoHTTPPipelining "disable HTTP pipelining"
  ^ singleStrArg [] ["remote-darcs"] F.RemoteDarcsOpt arg "COMMAND"
    "name of the darcs executable on the remote server"
  where arg (F.RemoteDarcsOpt s) = Just s
        arg _ = Nothing

umask :: PrimDarcsOption UMask
umask = (imap . cps) (Iso fw bw) $ singleStrArg [] ["umask"] F.UMask arg "UMASK"
    "specify umask to use when writing"
  where
    arg (F.UMask s) = Just s
    arg _ = Nothing
    fw (Just s) = YesUMask s
    fw Nothing = NoUMask
    bw (YesUMask s) = Just s
    bw NoUMask = Nothing

setScriptsExecutable :: PrimDarcsOption SetScriptsExecutable
setScriptsExecutable = withDefault NoSetScriptsExecutable
  [ RawNoArg [] ["set-scripts-executable"] F.SetScriptsExecutable YesSetScriptsExecutable
    "make scripts executable"
  , RawNoArg [] ["dont-set-scripts-executable","no-set-scripts-executable"]
    F.DontSetScriptsExecutable NoSetScriptsExecutable "don't make scripts executable" ]

restrictPaths :: PrimDarcsOption Bool
restrictPaths = withDefault True
  [ RawNoArg [] ["restrict-paths"] F.RestrictPaths True
    "don't allow darcs to touch external files or repo metadata"
  , RawNoArg [] ["dont-restrict-paths","no-restrict-paths"]
    F.DontRestrictPaths False
    "allow darcs to modify any file or directory (unsafe)" ]

-- * Specific to a single command

-- ** amend

amendUnrecord :: PrimDarcsOption Bool
amendUnrecord = withDefault False
  [ RawNoArg [] ["unrecord"] F.AmendUnrecord True "remove changes from the patch"
  , RawNoArg [] ["record"] F.NoAmendUnrecord False "add more changes to the patch" ]

selectAuthor :: PrimDarcsOption Bool
selectAuthor = singleNoArg [] ["select-author"] F.SelectAuthor
  "select author id from a menu"

-- ** annotate

-- | TODO: These should be mutually exclusive, but are they? The code is almost inscrutable.
humanReadable :: PrimDarcsOption Bool
humanReadable = withDefault False [__humanReadable True]

__humanReadable :: RawDarcsOption
__humanReadable val = RawNoArg [] ["human-readable"] F.HumanReadable val "give human-readable output"

-- | See above.
machineReadable :: PrimDarcsOption Bool
machineReadable = singleNoArg [] ["machine-readable"] F.MachineReadable "give machine-readable output"

-- ** clone

partial :: PrimDarcsOption CloneKind
partial = withDefault NormalClone
  [ RawNoArg [] ["lazy"] F.Lazy LazyClone "get patch files only as needed"
  , RawNoArg [] ["complete"] F.Complete CompleteClone "get a complete copy of the repository" ]

-- ** convert import/export

marks :: DarcsOption a (Maybe String -> Maybe String -> a)
marks = readMarks ^ writeMarks

readMarks :: PrimDarcsOption (Maybe String)
readMarks = singleStrArg [] ["read-marks"] F.ReadMarks arg
    "FILE" "continue conversion, previously checkpointed by --write-marks"
  where arg (F.ReadMarks s) = Just s
        arg _ = Nothing

writeMarks :: PrimDarcsOption (Maybe String)
writeMarks = singleStrArg [] ["write-marks"] F.WriteMarks arg
    "FILE" "checkpoint conversion to continue it later"
  where arg (F.WriteMarks s) = Just s
        arg _ = Nothing

-- | Deprecated flag, still present to output an error message.
hashed :: PrimDarcsOption ()
hashed = deprecated
  [ "All repositories are now \"hashed\", so this option was removed."
  , "Use --darcs-1 to get the effect that --hashed had previously." ] $
  [ RawNoArg [] ["hashed"] F.Hashed () "deprecated, use --darcs-1 instead" ]

data PatchFormat = PatchFormat1 | PatchFormat2 deriving (Eq, Show)

patchFormat :: PrimDarcsOption PatchFormat
patchFormat = withDefault PatchFormat2
  [ RawNoArg [] ["darcs-2"] F.UseFormat2 PatchFormat2
    "Standard darcs patch format"
  , RawNoArg [] ["darcs-1"] F.UseFormat1 PatchFormat1
    "Older patch format (for compatibility)"]

-- ** dist

distname :: PrimDarcsOption (Maybe String)
distname = singleStrArg ['d'] ["dist-name"] F.DistName arg "DISTNAME" "name of version"
  where arg (F.DistName s) = Just s
        arg _ = Nothing

distzip :: PrimDarcsOption Bool
distzip = singleNoArg [] ["zip"] F.DistZip "generate zip archive instead of gzip'ed tar"

-- ** log

data ChangesFormat = HumanReadable | GenContext | GenXml | NumberPatches | CountPatches deriving (Eq, Show)

changesFormat :: PrimDarcsOption (Maybe ChangesFormat)
changesFormat = withDefault Nothing
  [ RawNoArg [] ["context"] F.GenContext (Just GenContext) "give output suitable for get --context"
  , __xmloutput (Just GenXml)
  , __humanReadable (Just HumanReadable)
  , RawNoArg [] ["number"] F.NumberPatches (Just NumberPatches) "number the changes"
  , RawNoArg [] ["count"] F.Count (Just CountPatches) "output count of changes" ]

-- ** replace

tokens :: PrimDarcsOption (Maybe String)
tokens = singleStrArg [] ["token-chars"] F.Toks arg "\"[CHARS]\""
    "define token to contain these characters"
  where arg (F.Toks s) = Just s; arg _ = Nothing

forceReplace :: PrimDarcsOption Bool
forceReplace = withDefault False
  [ RawNoArg ['f'] ["force"] F.ForceReplace True
    "proceed with replace even if 'new' token already exists"
  , RawNoArg [] ["no-force"] F.NonForce False
    "don't force the replace if it looks scary" ]

-- ** test

data TestStrategy = Once | Linear | Backoff | Bisect deriving (Eq, Show)

testStrategy :: PrimDarcsOption TestStrategy
testStrategy = withDefault Once
  [ RawNoArg [] ["once"] F.Once Once "run test on current version only"
  , RawNoArg [] ["linear"] F.Linear Linear "locate the most recent version lacking an error"
  , RawNoArg [] ["backoff"] F.Backoff Backoff "exponential backoff search"
  , RawNoArg [] ["bisect"] F.Bisect Bisect "binary instead of linear search" ]

-- ** show files/index

files :: PrimDarcsOption Bool
files = withDefault True
  [ RawNoArg [] ["files"] F.Files True "include files in output"
  , RawNoArg [] ["no-files"] F.NoFiles False "don't include files in output" ]

directories :: PrimDarcsOption Bool
directories = withDefault True
  [ RawNoArg [] ["directories"] F.Directories True "include directories in output"
  , RawNoArg [] ["no-directories"] F.NoDirectories False "don't include directories in output" ]

pending :: PrimDarcsOption Bool
pending = withDefault True
  [ RawNoArg [] ["pending"] F.Pending True "reflect pending patches in output"
  , RawNoArg [] ["no-pending"] F.NoPending False "only included recorded patches in output" ]

-- "null" is already taken
nullFlag :: PrimDarcsOption Bool
nullFlag = singleNoArg ['0'] ["null"] F.NullFlag "separate file names by NUL characters"

-- ** gzcrcs

data GzcrcsAction = GzcrcsCheck | GzcrcsRepair deriving (Eq, Show)

gzcrcsActions :: PrimDarcsOption (Maybe GzcrcsAction)
gzcrcsActions = withDefault Nothing
  [ RawNoArg [] ["check"] F.Check (Just GzcrcsCheck) "Specify checking mode"
  , RawNoArg [] ["repair"] F.Repair (Just GzcrcsRepair) "Specify repair mode" ]

-- ** optimize

siblings :: PrimDarcsOption [AbsolutePath]
siblings = multiAbsPathArg [] ["sibling"] F.Sibling mkV "URL" "specify a sibling directory"
  where mkV fs = [ s | F.Sibling s <- fs ]

reorderPatches :: PrimDarcsOption Bool
reorderPatches = singleNoArg [] ["reorder-patches"] F.Reorder "reorder the patches in the repository"

reorder :: PrimDarcsOption Reorder
reorder = withDefault NoReorder
  [ RawNoArg [] ["reorder-patches"] F.Reorder Reorder
    "reorder the patches in the repository"
  , RawNoArg [] ["no-reorder-patches"] F.NoReorder NoReorder
    "don't reorder the patches in the repository" ]

optimizePatchIndex :: PrimDarcsOption (Maybe WithPatchIndex)
optimizePatchIndex = withDefault Nothing
  [ __patchIndex (Just YesPatchIndex)
  , __noPatchIndex (Just NoPatchIndex) ]
