--  Copyright (C) 2003-2005 David Roundy
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

module Darcs.UI.Commands.Apply
    ( apply, applyCmd
    , getPatchBundle -- used by darcsden
    ) where

import System.Exit ( exitSuccess )
import Prelude hiding ( (^), catch )
import Control.Monad ( when )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefullyM, info )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putVerbose
    , amInHashedRepository
    )
import Darcs.UI.Flags
    ( DarcsFlag
    , doHappyForwarding, doReverse, verbosity, useCache, dryRun
    , reorder, umask
    , fixUrl, getCc, getSendmailCmd
    , diffAlgorithm, isUnified, getReply
    )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.Util.Path ( toFilePath, AbsolutePath )
import Darcs.Repository
    ( Repository
    , SealedPatchSet
    , withRepoLock
    , readRepo
    , filterOutConflicts
    )
import Darcs.Patch.Set ( Origin, newset2RL )
import Darcs.Patch ( RepoPatch, PrimOf )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Info ( PatchInfo, showPatchInfoUI )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..), (:\/:)(..), (:>)(..)
    , mapRL, nullFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Util.ByteString ( linesPS, unlinesPS, gzReadStdin )
import Data.List( (\\) )
import qualified Data.ByteString as B (ByteString, null, init, take, drop)
import qualified Data.ByteString.Char8 as BC (unpack, last, pack)

import Darcs.Util.Download ( Cachable(Uncachable) )
import Darcs.Repository.External ( gzFetchFilePS )
import Darcs.UI.External
    ( sendEmailDoc
    , resendEmail
    , verifyPS
    )
import Darcs.UI.Email ( readEmail )
import Darcs.Patch.Depends ( findUncommon, findCommonWithThem )
import Darcs.UI.ApplyPatches ( PatchApplier(..), StandardPatchApplier(..), PatchProxy )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , runSelection
    , selectionContext
    )
import qualified Darcs.UI.SelectChanges as S
import Darcs.Patch.Bundle ( scanBundle )
import Darcs.Util.Printer
    ( packedString, vcat, text, empty
    , renderString, RenderMode(..)
    )
import Storage.Hashed.Tree( Tree )

#include "impossible.h"


applyDescription :: String
applyDescription = "Apply a patch bundle created by `darcs send'."

applyHelp :: String
applyHelp = unlines
 [ "The `darcs apply` command takes a patch bundle and attempts to insert"
 , "it into the current repository.  In addition to invoking it directly"
 , "on bundles created by `darcs send`, it is used internally by `darcs"
 , "push` on the remote end of an SSH connection."
 , ""
 , "If no file is supplied, the bundle is read from standard input."
 , ""
 , "If given an email instead of a patch bundle, Darcs will look for the"
 , "bundle as a MIME attachment to that email.  Currently this will fail"
 , "if the MIME boundary is rewritten, such as in Courier and Mail.app."
 , ""
 , "If the `--reply noreply@example.net` option is used, and the bundle is"
 , "attached to an email, Darcs will send a report (indicating success or"
 , "failure) to the sender of the bundle (the `To` field).  The argument to"
 , "noreply is the address the report will appear to originate FROM."
 , ""
 , "The `--cc` option will cause the report to be CC'd to another address,"
 , "for example `--cc reports@lists.example.net,admin@lists.example.net`."
 , "Using `--cc` without `--reply` is undefined."
 , ""
 , "If you want to use a command different from the default one for sending mail,"
 , "you need to specify a command line with the `--sendmail-command` option."
 , "The command line can contain the format specifier `%t` for to"
 , "and you can add `%<` to the end of the command line if the command"
 , "expects the complete mail on standard input. For example, the command line"
 , "for msmtp looks like this:"
 , ""
 , "    msmtp -t %<"
 , ""
 , "If gpg(1) is installed, you can use `--verify pubring.gpg` to reject"
 , "bundles that aren't signed by a key in `pubring.gpg`."
 , ""
 , "If `--test` is supplied and a test is defined (see `darcs setpref`), the"
 , "bundle will be rejected if the test fails after applying it.  In that"
 , "case, the rejection email from `--reply` will include the test output."
 ]

stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x

conflictsOpt :: DarcsOption a (Maybe O.AllowConflicts -> a)
conflictsOpt = O.conflicts O.NoAllowConflicts

applyBasicOpts :: DarcsOption a
                  (O.Verify
                   -> O.Reorder
                   -> Maybe Bool
                   -> O.DryRun
                   -> O.XmlOutput
                   -> [O.MatchFlag]
                   -> Maybe O.AllowConflicts
                   -> O.ExternalMerge
                   -> O.RunTest
                   -> O.LeaveTestDir
                   -> Maybe String
                   -> O.DiffAlgorithm
                   -> a)
applyBasicOpts
    = O.verify
    ^ O.reorder
    ^ O.interactive
    ^ O.dryRunXml
    ^ O.matchSeveral
    ^ conflictsOpt
    ^ O.useExternalMerge
    ^ O.test
    ^ O.leaveTestDir
    ^ O.workingRepoDir
    ^ O.diffAlgorithm

applyAdvancedOpts :: DarcsOption a
                     (Maybe String
                      -> Maybe String
                      -> Bool
                      -> (Bool, Maybe String)
                      -> O.UseIndex
                      -> O.Compression
                      -> O.SetScriptsExecutable
                      -> O.UMask
                      -> Bool
                      -> Bool
                      -> O.WantGuiPause
                      -> a)
applyAdvancedOpts
    = O.reply
    ^ O.ccApply
    ^ O.happyForwarding
    ^ O.sendmail
    ^ O.useIndex
    ^ O.compress
    ^ O.setScriptsExecutable
    ^ O.umask
    ^ O.restrictPaths
    ^ O.changesReverse
    ^ O.pauseForGui

applyOpts :: DarcsOption a
             (O.Verify
              -> O.Reorder
              -> Maybe Bool
              -> O.DryRun
              -> O.XmlOutput
              -> [O.MatchFlag]
              -> Maybe O.AllowConflicts
              -> O.ExternalMerge
              -> O.RunTest
              -> O.LeaveTestDir
              -> Maybe String
              -> O.DiffAlgorithm
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> Maybe String
              -> Maybe String
              -> Bool
              -> (Bool, Maybe String)
              -> O.UseIndex
              -> O.Compression
              -> O.SetScriptsExecutable
              -> O.UMask
              -> Bool
              -> Bool
              -> O.WantGuiPause
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)
applyOpts = applyBasicOpts `withStdOpts` applyAdvancedOpts

apply :: DarcsCommand [DarcsFlag]
apply = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "apply"
    , commandHelp = applyHelp ++ "\n" ++ applyHelp'
    , commandDescription = applyDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["<PATCHFILE>"]
    , commandCommand = applyCmd StandardPatchApplier
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = const stdindefault
    , commandAdvancedOptions = odesc applyAdvancedOpts
    , commandBasicOptions = odesc applyBasicOpts
    , commandDefaults = defaultFlags applyOpts
    , commandCheckOptions = ocheck applyOpts
    , commandParseOptions = onormalise applyOpts
    }

applyCmd :: PatchApplier pa => pa -> (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
applyCmd _ _ _ [""] = fail "Empty filename argument given to apply!"
applyCmd patchApplier _ opts ["-"] = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ repoJob patchApplier opts $ \patchProxy repository -> do
    -- for darcs users who try out 'darcs apply' without any arguments
  putVerbose opts $ text "reading patch bundle from stdin..."
  bundle <- gzReadStdin
  applyCmdCommon patchApplier patchProxy opts bundle repository

applyCmd patchApplier (_,o) opts [unfixed_patchesfile] = withRepoLock (dryRun opts) (useCache opts) YesUpdateWorking (umask opts) $ repoJob patchApplier opts $ \patchProxy repository -> do
  patchesfile <- fixUrl o unfixed_patchesfile
  bundle <- gzFetchFilePS (toFilePath patchesfile) Uncachable
  applyCmdCommon patchApplier patchProxy opts bundle repository

applyCmd _ _ _ _ = impossible

applyCmdCommon
    :: forall pa p wR wU
     . ( PatchApplier pa, RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree
       , RepoPatch (CarrierType pa p), ApplyState (CarrierType pa p) ~ Tree 
       )
    => pa
    -> PatchProxy p
    -> [DarcsFlag]
    -> B.ByteString
    -> Repository (CarrierType pa p) wR wU wR
    -> IO ()
applyCmdCommon patchApplier patchProxy opts bundle repository = do
  let from_whom = getFrom bundle
  us <- readRepo repository
  either_them <- getPatchBundle opts bundle
  Sealed them
     <- case either_them of
          Right t -> return t
          Left er -> do forwarded <- considerForwarding opts bundle
                        if forwarded
                          then exitSuccess
                          else fail er
  common :> _ <- return $ findCommonWithThem us them

  -- all patches that are in "them" and not in "common" need to be available; check that
  let common_i = mapRL info $ newset2RL common
      them_i = mapRL info $ newset2RL them
      required = them_i \\ common_i -- FIXME quadratic?
      check :: RL (PatchInfoAnd (CarrierType pa p)) wX wY -> [PatchInfo] -> IO ()
      check (p :<: ps') bad = case hopefullyM p of
        Nothing | info p `elem` required -> check ps' (info p : bad)
        _ -> check ps' bad
      check NilRL [] = return ()
      check NilRL bad = fail . renderString Encode $ vcat $ map showPatchInfoUI bad ++
                        [ text "\nFATAL: Cannot apply this bundle. We are missing the above patches." ]

  check (newset2RL them) []

  (us':\/:them') <- return $ findUncommon us them
  (hadConflicts, Sealed their_ps)
    <- if parseFlags conflictsOpt opts == Nothing -- skip conflicts
        then filterOutConflicts (reverseFL us') repository them'
        else return (False, Sealed them')
  when hadConflicts $ putStrLn "Skipping some patches which would cause conflicts."
  when (nullFL their_ps) $
       do if hadConflicts
           then putStrLn ("All new patches of the bundle cause conflicts.  " ++
                          "Nothing to do.") >> exitSuccess
           else putStrLn ("All these patches have already been applied.  " ++
                          "Nothing to do.") >> when (reorder opts /= O.Reorder) exitSuccess
          
  let direction = if doReverse opts then FirstReversed else First
      context = selectionContext direction "apply" (patchSelOpts opts) Nothing Nothing
  (to_be_applied :> _) <- runSelection (selectChanges their_ps) context
  applyPatches patchApplier patchProxy "apply" opts from_whom repository us' to_be_applied
--    see the default (False) for the option
--    where fixed_opts = if Interactive `elem` opts
--                          then opts
--                          else All : opts

getPatchBundle :: RepoPatch p => [DarcsFlag] -> B.ByteString
                 -> IO (Either String (SealedPatchSet p Origin))
getPatchBundle opts fps = do
    let opt_verify = parseFlags O.verify opts
    mps <- verifyPS opt_verify $ readEmail fps
    mops <- verifyPS opt_verify fps
    case (mps, mops) of
      (Nothing, Nothing) ->
          return $ Left "Patch bundle not properly signed, or gpg failed."
      (Just bundle, Nothing) -> return $ scanBundle bundle
      (Nothing, Just bundle) -> return $ scanBundle bundle
      -- We use careful_scan_bundle only below because in either of the two
      -- above case we know the patch was signed, so it really shouldn't
      -- need stripping of CRs.
      (Just ps1, Just ps2) -> case careful_scan_bundle ps1 of
                              Left _ -> return $ careful_scan_bundle ps2
                              Right x -> return $ Right x
          where careful_scan_bundle bundle =
                    case scanBundle bundle of
                    Left e -> case scanBundle $ stripCrPS bundle of
                              Right x -> Right x
                              _ -> Left e
                    x -> x
                stripCrPS :: B.ByteString -> B.ByteString
                stripCrPS bundle = unlinesPS $ map stripline $ linesPS bundle
                stripline p | B.null p = p
                            | BC.last p == '\r' = B.init p
                            | otherwise = p

applyHelp' :: String
applyHelp' =
 "A patch bundle may introduce unresolved conflicts with existing\n" ++
 "patches or with the working tree.  By default, Darcs will add conflict\n" ++
 "markers (see `darcs mark-conflicts`).\n" ++
 "\n" ++
 "The `--external-merge` option lets you resolve these conflicts\n" ++
 "using an external merge tool.  In the option, `%a` is replaced with\n" ++
 "the common ancestor (merge base), `%1` with the first version, `%2`\n" ++
 "with the second version, and `%o` with the path where your resolved\n" ++
 "content should go.  For example, to use the xxdiff visual merge tool\n" ++
 "you'd specify: `--external-merge='xxdiff -m -O -M %o %1 %a %2'`\n" ++
 "\n" ++
 "The `--allow-conflicts` option will skip conflict marking; this is\n" ++
 "useful when you want to treat a repository as just a bunch of patches,\n" ++
 "such as using `darcs pull --union` to download of your co-workers\n" ++
 "patches before going offline.\n" ++
 "\n" ++
 "This can mess up unrecorded changes in the working tree, forcing you\n" ++
 "to resolve the conflict immediately.  To simply reject bundles that\n" ++
 "introduce unresolved conflicts, using the `--dont-allow-conflicts`\n" ++
 "option.  Making this the default in push-based workflows is strongly\n" ++
 "recommended.\n" ++
 "\n" ++
 "Unlike most Darcs commands, `darcs apply` defaults to `--all`.  Use the\n" ++
 "`--interactive` option to pick which patches to apply from a bundle.\n"

getFrom :: B.ByteString -> String
getFrom bundle = readFrom $ linesPS bundle
    where readFrom [] = ""
          readFrom (x:xs)
           | B.take 5 x == fromStart = BC.unpack $ B.drop 5 x
           | otherwise = readFrom xs

forwardingMessage :: B.ByteString
forwardingMessage = BC.pack $
    "The following patch was either unsigned, or signed by a non-allowed\n"++
    "key, or there was a GPG failure.\n"

considerForwarding :: [DarcsFlag] -> B.ByteString -> IO Bool
considerForwarding opts bundle = case getReply opts of
  Nothing -> return False
  Just from -> case break is_from (linesPS bundle) of
    (m1, f:m2) ->
        let m_lines = forwardingMessage:m1 ++ m2
            m' = unlinesPS m_lines
            f' = BC.unpack (B.drop 5 f) in
            if from == f' || from == init f'
            then return False -- Refuse possible email loop.
            else do
              scmd <- getSendmailCmd opts
              if doHappyForwarding opts
               then resendEmail from scmd bundle
               else sendEmailDoc f' from "A forwarded darcs patch" cc
                                 scmd (Just (empty,empty))
                                 (packedString m')
              return True
    _ -> return False -- Don't forward emails lacking headers!
  where
    cc = getCc opts
    is_from l = B.take 5 l == fromStart

fromStart :: B.ByteString
fromStart = BC.pack "From:"

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = maybeIsInteractive flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = O.NoSummary -- option not supported, use default
    , S.withContext = isUnified flags
    }

maybeIsInteractive :: [DarcsFlag] -> Bool
maybeIsInteractive = maybe False id . parseFlags O.interactive
