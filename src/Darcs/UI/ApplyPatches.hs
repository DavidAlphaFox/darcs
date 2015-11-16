module Darcs.UI.ApplyPatches
    ( PatchApplier(..), PatchProxy(..)
    , StandardPatchApplier(..)
    ) where

import System.Exit ( ExitCode ( ExitSuccess ), exitSuccess )
import Prelude hiding ( catch )
import System.IO ( hClose, stdout, stderr )
import Control.Exception
                 ( catch, fromException, SomeException, throwIO )
import Control.Monad ( when, unless )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.UI.Commands
    ( putVerbose
    , putInfo
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    )
import Darcs.UI.CommandsAux ( checkPaths )
import Darcs.UI.Flags
    ( DarcsFlag, verbosity, compression, reorder, allowConflicts, externalMerge
    , wantGuiPause, diffingOpts, setScriptsExecutable, isInteractive, testChanges
    , hasXmlOutput, getReply, getCc, getSendmailCmd, hasSummary, dryRun
    )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands.Util ( testTentativeAndMaybeExit )
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.Repository
    ( Repository
    , tentativelyMergePatches
    , finalizeRepositoryChanges
    , applyToWorking
    , invalidateIndex
    , setScriptsExecutablePatches
    )
import Darcs.Repository.Job ( RepoJob(RepoJob) )
import Darcs.Patch ( RepoPatch, description, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered
    ( FL, mapFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )

import Darcs.UI.External ( sendEmail )
import Darcs.Repository.Lock ( withStdoutTemp, readBinFile )
import Darcs.Util.Printer ( vcat, text )
import Storage.Hashed.Tree( Tree )

data PatchProxy (p :: * -> * -> *) = PatchProxy

-- |This class is a hack to abstract over pull/apply and rebase pull/apply.
class PatchApplier pa where

    -- |'CarrierType pa p' resolves to either 'p' or 'Rebasing p' 
    type CarrierType pa (p :: * -> * -> *) :: * -> * -> *

    repoJob
        :: pa
        -> [DarcsFlag]
        -> (forall p wR wU
               . ( RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree
                 , RepoPatch (CarrierType pa p), ApplyState (CarrierType pa p) ~ Tree
                 , ApplyState (PrimOf (CarrierType pa p)) ~ Tree
                 )
              => (PatchProxy p -> Repository (CarrierType pa p) wR wU wR -> IO ()))
        -> RepoJob ()

    applyPatches
        :: forall p wR wU wT wX wZ
         . (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
        => pa
        -> PatchProxy p
        -> String
        -> [DarcsFlag]
        -> String
        -> Repository (CarrierType pa p) wR wU wT
        -> FL (PatchInfoAnd (CarrierType pa p)) wX wT
        -> FL (PatchInfoAnd (CarrierType pa p)) wX wZ -> IO ()

data StandardPatchApplier = StandardPatchApplier

instance PatchApplier StandardPatchApplier where
    type CarrierType StandardPatchApplier p = p
    repoJob StandardPatchApplier _opts f = RepoJob (f PatchProxy)
    applyPatches StandardPatchApplier PatchProxy = standardApplyPatches

standardApplyPatches
           :: forall p wR wU wT wX wZ . (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
           => String -> [DarcsFlag] -> String -> Repository p wR wU wT
           -> FL (PatchInfoAnd p) wX wT -> FL (PatchInfoAnd p) wX wZ -> IO ()
standardApplyPatches cmdName opts from_whom repository us' to_be_applied = do
   printDryRunMessageAndExit cmdName
      (verbosity opts)
      (hasSummary O.NoSummary opts)
      (dryRun opts)
      (hasXmlOutput opts)
      (isInteractive True opts)
      to_be_applied
   when (nullFL to_be_applied && reorder opts == O.NoReorder) $ do 
           putStrLn $ "You don't want to " ++ cmdName ++ " any patches, so I'm exiting!"
           exitSuccess
   checkPaths opts to_be_applied
   redirectOutput opts from_whom $ do
    unless (nullFL to_be_applied) $ do
        putVerbose opts $ text $ "Will " ++ cmdName ++ " the following patches:"
        putVerbose opts . vcat $ mapFL description to_be_applied
        setEnvDarcsPatches to_be_applied
    Sealed pw <- tentativelyMergePatches repository cmdName
                         (allowConflicts opts) YesUpdateWorking
                         (externalMerge opts) (wantGuiPause opts)
                         (compression opts) (verbosity opts)
                         (reorder opts) (diffingOpts opts)
                         us' to_be_applied
    invalidateIndex repository
    testTentativeAndMaybeExit repository
         (verbosity opts)
         (testChanges opts)
         (setScriptsExecutable opts)
         (isInteractive True opts)
         "those patches do not pass the tests." (cmdName ++ " them") Nothing
    withSignalsBlocked $ do finalizeRepositoryChanges repository YesUpdateWorking (compression opts)
                            _ <- applyToWorking repository (verbosity opts) pw `catch` \(e :: SomeException) ->
                                fail ("Error applying patch to working dir:\n" ++ show e)
                            when (setScriptsExecutable opts == O.YesSetScriptsExecutable) $
                              setScriptsExecutablePatches pw
                            return ()
    case (nullFL to_be_applied, reorder opts == O.Reorder) of
                (True,True)  -> putInfo opts $ text $ "Nothing to " ++ cmdName ++ ", finished reordering."
                (False,True) -> putInfo opts $ text $ "Finished " ++ cmdName ++ "ing and reordering."
                _            -> putInfo opts $ text $ "Finished " ++ cmdName ++ "ing."

redirectOutput :: [DarcsFlag] -> String -> IO () -> IO ()
redirectOutput opts to doit = case getReply opts of
    Nothing -> doit
    Just from -> withStdoutTemp $ \tempf -> doitAndCleanup `catch` sendit tempf from
  where
    -- TODO: I suggest people writing such code should *at least* put in some comments.
    -- It is unclear how this works and how the intertwined exception handlers make
    -- this do what the author wanted.
    doitAndCleanup = doit >> hClose stdout >> hClose stderr
    sendit :: FilePath -> String -> SomeException -> IO a
    sendit tempf from e | Just ExitSuccess <- fromException e =
      do sendSanitizedEmail opts from to "Patch applied" cc tempf
         throwIO e
    sendit tempf from e | Just (_ :: ExitCode) <- fromException e =
      do sendSanitizedEmail opts from to "Patch failed!" cc tempf
         throwIO ExitSuccess
    sendit tempf from e =
      do sendSanitizedEmail opts from to "Darcs error applying patch!" cc $
                   tempf ++ "\n\nCaught exception:\n"++
                   show e++"\n"
         throwIO ExitSuccess
    cc = getCc opts

-- |sendSanitizedEmail sends a sanitized email using the given sendmailcmd
-- It takes @DacrsFlag@ options a file with the mail contents,
-- To:, Subject:, CC:, and mail body
sendSanitizedEmail :: [DarcsFlag] -> String -> String -> String -> String -> String -> IO ()
sendSanitizedEmail opts from to subject cc mailtext =
    do scmd <- getSendmailCmd opts
       body <- sanitizeFile mailtext
       sendEmail from to subject cc scmd body

-- sanitizeFile is used to clean up the stdout/stderr before sticking it in
-- an email.

sanitizeFile :: FilePath -> IO String
sanitizeFile f = sanitize `fmap` readBinFile f
    where sanitize s = wash $ remove_backspaces "" s
          wash ('\000':s) = "\\NUL" ++ wash s
          wash ('\026':s) = "\\EOF" ++ wash s
          wash (c:cs) = c : wash cs
          wash [] = []
          remove_backspaces rev_sofar "" = reverse rev_sofar
          remove_backspaces (_:rs) ('\008':s) = remove_backspaces rs s
          remove_backspaces "" ('\008':s) = remove_backspaces "" s
          remove_backspaces rs (s:ss) = remove_backspaces (s:rs) ss

