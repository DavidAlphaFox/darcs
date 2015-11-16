-- |This module should only be imported by Darcs.UI.Options.*
-- and by 'Darcs.UI.Flags'. Other modules needing access to 'DarcsFlag'
-- should import 'Darcs.UI.Flags'
module Darcs.UI.Options.Flags ( DarcsFlag(..) ) where

import Darcs.Util.Path ( AbsolutePath, AbsolutePathOrStd )

-- | The 'DarcsFlag' type is a list of all flags that can ever be
-- passed to darcs, or to one of its commands.
data DarcsFlag = Version | ExactVersion | ListCommands
               | Help | ListOptions | NoTest | Test
               | OnlyChangesToFiles | ChangesToAllFiles
               | LeaveTestDir | NoLeaveTestDir
               | Timings | Debug | DebugHTTP
               | Verbose | NormalVerbosity | Quiet
               | Target String | Cc String
               | Output AbsolutePathOrStd | OutputAutoName AbsolutePath | Mail
               | Subject String | InReplyTo String | Charset String
               | SendmailCmd String | Author String | SelectAuthor | PatchName String
               | OnePatch String | SeveralPatch String
               | OneHash String
               | AfterPatch String | UpToPatch String
               | AfterHash String | UpToHash String
               | TagName String | LastN Int | MaxCount Int | PatchIndexRange Int Int
               | NumberPatches
               | OneTag String | AfterTag String | UpToTag String
               | GenContext | Context AbsolutePath | Count
               | LogFile AbsolutePath | RmLogFile | DontRmLogFile
               | DistName String | DistZip | All
               | Recursive | NoRecursive
               | Minimize | NoMinimize
               | Reorder | NoReorder
               | RestrictPaths | DontRestrictPaths
               | AskDeps | NoAskDeps | IgnoreTimes | DontIgnoreTimes
               | LookForAdds | NoLookForAdds
               | LookForMoves | NoLookForMoves
               | LookForReplaces | NoLookForReplaces
               | UseMyersDiff | UsePatienceDiff
               | Intersection | Union | Complement
               | Sign | SignAs String | NoSign | SignSSL String
               | HappyForwarding | NoHappyForwarding
               | Verify AbsolutePath | VerifySSL AbsolutePath
               | RemoteDarcsOpt String
               | EditDescription | NoEditDescription
               | Toks String
               | EditLongComment | NoEditLongComment | PromptLongComment
               | KeepDate | NoKeepDate
               | AllowConflicts | MarkConflicts | NoAllowConflicts
               | SkipConflicts
               | Boring | SkipBoring
               | AllowCaseOnly | DontAllowCaseOnly
               | AllowWindowsReserved | DontAllowWindowsReserved
               | DontGrabDeps | DontPromptForDependencies | PromptForDependencies
               | Compress | NoCompress | UnCompress
               | WorkRepoDir String | WorkRepoUrl String | RemoteRepo String
               | NewRepo String
               | NotInRemote (Maybe String)
               | Reply String | ApplyAs String
               | MachineReadable | HumanReadable
               | Pipe | Interactive
               | DiffCmd String
               | ExternalMerge String | Summary | NoSummary
               | PauseForGui | NoPauseForGui
               | Unified | NonUnified | Reverse | Forward
               | Complete | Lazy
               | DiffFlags String
               | XMLOutput
               | ForceReplace
               | OnePattern String | SeveralPattern String
               | AfterPattern String | UpToPattern String
               | NonApply | NonVerify | NonForce
               | DryRun
               | SetDefault | NoSetDefault
               | Disable | SetScriptsExecutable | DontSetScriptsExecutable
               | Once | Linear | Backoff | Bisect
               | Hashed -- deprecated flag, here to output an error message
               | UseFormat1 | UseFormat2 | UseNoWorkingDir | UseWorkingDir
               | Sibling AbsolutePath
               | Files | NoFiles | Directories | NoDirectories
               | Pending | NoPending
               | PosthookCmd String | NoPosthook | AskPosthook | RunPosthook
               | PrehookCmd String  | NoPrehook  | AskPrehook  | RunPrehook
               | UMask String
               | StoreInMemory | ApplyOnDisk
               | NoHTTPPipelining
               | Packs | NoPacks
               | NoCache
               | AllowUnrelatedRepos
               | Check | Repair | JustThisRepo
               | ReadMarks String | WriteMarks String
               | NullFlag
               | NoAmendUnrecord | AmendUnrecord
               | PatchIndexFlag
               | NoPatchIndexFlag
                 deriving ( Eq, Show )
