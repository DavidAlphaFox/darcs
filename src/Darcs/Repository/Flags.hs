module Darcs.Repository.Flags
    (
      Compression (..)
    , RemoteDarcs (..)
    , Reorder (..)
    , Verbosity (..)
    , UpdateWorking (..)
    , UseCache (..)
    , DryRun (..)
    , UMask (..)
    , LookForAdds (..)
    , LookForReplaces (..)
    , DiffAlgorithm (..)
    , LookForMoves (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , UseIndex (..)
    , ScanKnown (..)
    , CloneKind (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , WantGuiPause (..)
    , WithPatchIndex (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    ) where

import Darcs.Util.Diff ( DiffAlgorithm(..) )

data Verbosity = Quiet | NormalVerbosity | Verbose
    deriving ( Eq, Show )

data Compression = NoCompression
                 | GzipCompression
    deriving ( Eq, Show )

data WithPatchIndex = YesPatchIndex | NoPatchIndex
    deriving ( Eq, Show )

data RemoteDarcs = RemoteDarcs String
                 | DefaultRemoteDarcs
    deriving ( Eq, Show )

data Reorder = NoReorder | Reorder
    deriving ( Eq )

data UpdateWorking = YesUpdateWorking | NoUpdateWorking
    deriving ( Eq, Show )

data UseCache = YesUseCache | NoUseCache
    deriving ( Eq, Show )

data DryRun = YesDryRun | NoDryRun
    deriving ( Eq, Show )

data UMask = YesUMask String | NoUMask
    deriving ( Eq, Show )

data LookForAdds = YesLookForAdds | NoLookForAdds
    deriving ( Eq, Show )

data LookForReplaces = YesLookForReplaces | NoLookForReplaces
    deriving ( Eq, Show )

data LookForMoves = YesLookForMoves | NoLookForMoves
    deriving ( Eq, Show )

data RunTest = YesRunTest | NoRunTest
    deriving ( Eq, Show )

data SetScriptsExecutable = YesSetScriptsExecutable | NoSetScriptsExecutable
    deriving ( Eq, Show )

data LeaveTestDir = YesLeaveTestDir | NoLeaveTestDir
    deriving ( Eq, Show )

data RemoteRepos = RemoteRepos [String]
    deriving ( Eq, Show )

data SetDefault = YesSetDefault Bool | NoSetDefault Bool
    deriving ( Eq, Show )

data UseIndex = UseIndex | IgnoreIndex deriving ( Eq, Show )

data ScanKnown = ScanKnown -- ^Just files already known to darcs
               | ScanAll -- ^All files, i.e. look for new ones
               | ScanBoring -- ^All files, even boring ones

-- Various kinds of getting repositories
data CloneKind = LazyClone       -- ^Just copy pristine and inventories
               | NormalClone     -- ^First do a lazy clone then copy everything
               | CompleteClone   -- ^Same as Normal but omit telling user they can interrumpt
    deriving ( Eq, Show )

data AllowConflicts = NoAllowConflicts | YesAllowConflicts | YesAllowConflictsAndMark
    deriving ( Eq, Show )

data ExternalMerge = YesExternalMerge String | NoExternalMerge
    deriving ( Eq, Show )

data WorkRepo = WorkRepoDir String | WorkRepoPossibleURL String | WorkRepoCurrentDir
    deriving ( Eq, Show )

data WantGuiPause = YesWantGuiPause | NoWantGuiPause
    deriving ( Eq, Show )

data WithWorkingDir = WithWorkingDir | NoWorkingDir
    deriving ( Eq, Show )

data ForgetParent = YesForgetParent | NoForgetParent
    deriving ( Eq, Show )
