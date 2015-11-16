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

{-# LANGUAGE CPP, TypeOperators, OverloadedStrings #-}

module Darcs.UI.Commands.Send ( send ) where

import Prelude hiding ( (^), catch )

import System.Exit
    ( exitSuccess
#ifndef HAVE_MAPI
    , ExitCode ( ExitFailure )
    , exitWith
#endif
    )
import System.IO.Error ( ioeGetErrorString )
import System.IO ( hClose )
import Control.Exception ( catch, IOException )
import Control.Monad ( when, unless, forM_ )
import Storage.Hashed.Tree ( Tree )
import Data.List ( intercalate, isPrefixOf )
#ifdef HAVE_HTTP
import Data.List ( stripPrefix )
#endif
import Data.Maybe ( isNothing, fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putInfo
    , putVerbose
    , printDryRunMessageAndExit
    , setEnvDarcsPatches
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Flags
    ( DarcsFlag( Target
               , Context
               , Mail
               , DryRun
               , Quiet
               , AllowUnrelatedRepos
               )
    , willRemoveLogFile, doReverse, dryRun, useCache, remoteRepos, setDefault
    , fixUrl
    , getCc
    , getAuthor
    , getSubject
    , getInReplyTo
    , getSendmailCmd
    , getOutput
    , getCharset
    , verbosity
    , hasSummary
    , isInteractive
    , hasAuthor
    , hasLogfile
    , selectDeps
    , minimize
    , editDescription
    )
import Darcs.UI.Options
    ( DarcsOption, (^), odesc, ocheck, onormalise
    , defaultFlags, parseFlags
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, patchDesc )
import Darcs.Repository ( PatchSet, Repository,
                          identifyRepositoryFor, withRepository, RepoJob(..),
                          readRepo, readRecorded, prefsUrl, checkUnrelatedRepos )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( RepoPatch, description, applyToTree, invert )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), (:\/:)(..),
    mapFL, mapFL_FL, lengthFL, nullFL )
import Darcs.Patch.Bundle ( minContext, makeBundleN, scanContextFile, patchFilename )
import Darcs.Repository.Prefs ( addRepoSource, getPreflist )
import Darcs.Repository.External ( fetchFilePS, Cachable(..) )
import Darcs.UI.External
    ( signString
    , sendEmailDoc
    , generateEmail
    , editFile
    , catchall
    , getSystemEncoding
    , isUTF8Locale
#ifndef HAVE_MAPI
    , haveSendmail
#endif
    )
import Darcs.Util.ByteString ( mmapFilePS, isAscii )
import qualified Data.ByteString.Char8 as BC (unpack)
import Darcs.Repository.Lock
    ( withOpenTemp
    , writeDocBinFile
    , readDocBinFile
    , removeFileMayNotExist
    )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Util.Prompt ( askUser, promptYorn )
import Data.Text.Encoding       ( decodeUtf8' )
import Darcs.Util.Progress ( debugMessage )
import Darcs.UI.Email ( makeEmail )
import Darcs.Util.Printer
    ( Doc, vsep, text, ($$), (<+>), (<>), putDoc, putDocLn
    , renderPS, RenderMode(..)
    )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Path ( FilePathLike, toFilePath, AbsolutePath, AbsolutePathOrStd,
                        getCurrentDirectory, useAbsoluteOrStd, makeAbsoluteOrStd )
import Darcs.Util.Download.HTTP ( postUrl )
import Darcs.Util.Workaround ( renameFile )
import Darcs.Util.Global ( darcsSendMessage, darcsSendMessageFinal )
import Darcs.Util.SignalHandler ( catchInterrupt )

import qualified Darcs.UI.Message.Send as Msg
#include "impossible.h"

sendBasicOpts :: DarcsOption a
                 ([O.MatchFlag]
                  -> O.SelectDeps
                  -> Maybe Bool
                  -> O.HeaderFields
                  -> Maybe String
                  -> Maybe String
                  -> (Bool, Maybe String)
                  -> Maybe O.Output
                  -> O.Sign
                  -> O.DryRun
                  -> O.XmlOutput
                  -> Maybe O.Summary
                  -> Bool
                  -> Maybe Bool
                  -> Maybe String
                  -> Bool
                  -> Bool
                  -> a)
sendBasicOpts
    = O.matchSeveral
    ^ O.selectDeps
    ^ O.interactive -- True
    ^ O.headerFields
    ^ O.author
    ^ O.charset
    ^ O.sendmail
    ^ O.output
    ^ O.sign
    ^ O.dryRunXml
    ^ O.summary
    ^ O.editDescription
    ^ O.setDefault
    ^ O.workingRepoDir
    ^ O.minimize
    ^ O.allowUnrelatedRepos

sendAdvancedOpts :: DarcsOption a
                    (O.Logfile
                     -> O.RemoteRepos
                     -> Maybe AbsolutePath
                     -> Bool
                     -> O.NetworkOptions
                     -> a)
sendAdvancedOpts
    = O.logfile
    ^ O.remoteRepos
    ^ O.sendToContext 
    ^ O.changesReverse
    ^ O.network

sendOpts :: DarcsOption a
            ([O.MatchFlag]
             -> O.SelectDeps
             -> Maybe Bool
             -> O.HeaderFields
             -> Maybe String
             -> Maybe String
             -> (Bool, Maybe String)
             -> Maybe O.Output
             -> O.Sign
             -> O.DryRun
             -> O.XmlOutput
             -> Maybe O.Summary
             -> Bool
             -> Maybe Bool
             -> Maybe String
             -> Bool
             -> Bool
             -> Maybe O.StdCmdAction
             -> Bool
             -> Bool
             -> O.Verbosity
             -> Bool
             -> O.Logfile
             -> O.RemoteRepos
             -> Maybe AbsolutePath
             -> Bool
             -> O.NetworkOptions
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
sendOpts = sendBasicOpts `withStdOpts` sendAdvancedOpts

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.diffAlgorithm = O.PatienceDiff
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps flags
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = O.NoContext
    }

send :: DarcsCommand [DarcsFlag]
send = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "send"
    , commandHelp = Msg.cmdHelp
    , commandDescription = Msg.cmdDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["[REPOSITORY]"]
    , commandCommand = sendCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = getPreflist "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc sendAdvancedOpts
    , commandBasicOptions = odesc sendBasicOpts
    , commandDefaults = defaultFlags sendOpts
    , commandCheckOptions = ocheck sendOpts
    , commandParseOptions = onormalise sendOpts
    }

sendCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
sendCmd fps input_opts [""] = sendCmd fps input_opts []
sendCmd (_,o) input_opts [unfixedrepodir] =
 withRepository (useCache input_opts) $ RepoJob $
  \(repository :: Repository p wR wU wR) -> do
  context_ps <- the_context input_opts
  case context_ps of
    Just them -> do
        wtds <- decideOnBehavior input_opts (Nothing :: Maybe (Repository p wR wU wR))
        sendToThem repository input_opts wtds "CONTEXT" them
    Nothing -> do
        repodir <- fixUrl o unfixedrepodir
        -- Test to make sure we aren't trying to push to the current repo
        here <- getCurrentDirectory
        when (repodir == toFilePath here) $
           fail Msg.cannotSendToSelf
        old_default <- getPreflist "defaultrepo"
        when (old_default == [repodir] && Quiet `notElem` input_opts) $
             putDocLn (Msg.creatingPatch repodir)
        repo <- identifyRepositoryFor repository (useCache input_opts) repodir
        them <- readRepo repo
        addRepoSource repodir (dryRun input_opts) (remoteRepos input_opts) (setDefault False input_opts)
        wtds <- decideOnBehavior input_opts (Just repo)
        sendToThem repository input_opts wtds repodir them
    where the_context [] = return Nothing
          the_context (Context foo:_)
              = Just `fmap` scanContextFile (toFilePath foo)
          the_context (_:fs) = the_context fs
sendCmd _ _ _ = impossible

sendToThem :: (RepoPatch p, ApplyState p ~ Tree)
           => Repository p wR wU wT -> [DarcsFlag] -> [WhatToDo] -> String
           -> PatchSet p Origin wX -> IO ()
sendToThem repo opts wtds their_name them = do
#ifndef HAVE_MAPI
  -- Check if the user has sendmail or provided a --sendmail-cmd
  -- (unless -o/-O or --dry-run is used)
  sendmail <- haveSendmail
  sm_cmd <- getSendmailCmd opts
  when (isNothing (getOutput opts "") && DryRun `notElem` opts &&
        not sendmail && sm_cmd == "") $ do
      putInfo opts Msg.noWorkingSendmail
      exitWith $ ExitFailure 1
#endif
  us <- readRepo repo
  common :> us' <- return $ findCommonWithThem us them
  checkUnrelatedRepos (AllowUnrelatedRepos `elem` opts) us them
  case us' of
      NilFL -> do putInfo opts Msg.nothingSendable
                  exitSuccess
      _     -> putVerbose opts $ Msg.selectionIs (mapFL description us')
  pristine <- readRecorded repo
  let direction = if doReverse opts then FirstReversed else First
      context = selectionContext direction "send" (patchSelOpts opts) Nothing Nothing
  (to_be_sent :> _) <- runSelection (selectChanges us') context
  printDryRunMessageAndExit "send"
      (verbosity opts)
      (hasSummary O.NoSummary opts)
      (dryRun opts)
      O.NoXml
      (isInteractive True opts)
      to_be_sent
  when (nullFL to_be_sent) $ do
      putInfo opts Msg.selectionIsNull
      exitSuccess
  setEnvDarcsPatches to_be_sent

  let genFullBundle = prepareBundle opts common  (Right (pristine, us':\/:to_be_sent))
  bundle <- if not (minimize opts)
             then genFullBundle
             else do putInfo opts "Minimizing context, to send with full context hit ctrl-C..."
                     ( case minContext common to_be_sent of
                         Sealed (common' :> to_be_sent') -> prepareBundle opts common' (Left to_be_sent') )
                     `catchInterrupt` genFullBundle
  here   <- getCurrentDirectory
  let make_fname (tb:>:_) = patchFilename $ patchDesc tb
      make_fname _ = impossible
      fname = make_fname to_be_sent
      outname = case getOutput opts fname of
                    Just f  -> Just f
                    Nothing | Mail `elem` opts -> Nothing
                            | not $ null [ p | Post p <- wtds] -> Nothing
                            | otherwise        -> Just (makeAbsoluteOrStd here fname)
  case outname of
    Just fname' -> writeBundleToFile opts to_be_sent bundle fname' wtds their_name
    Nothing     -> sendBundle opts to_be_sent bundle fname wtds their_name


prepareBundle :: forall p wX wY wZ. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet p Origin wZ
              -> Either (FL (PatchInfoAnd p) wX wY)
                        (Tree IO, (FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) wX wY)
              -> IO Doc
prepareBundle opts common e = do
  unsig_bundle <-
     case e of
       (Right (pristine, us' :\/: to_be_sent)) -> do
         pristine' <- applyToTree (invert $ mapFL_FL hopefully us') pristine
         makeBundleN (Just pristine')
                     (unsafeCoerceP common)
                     (mapFL_FL hopefully to_be_sent)
       Left to_be_sent -> makeBundleN Nothing
                                      (unsafeCoerceP common)
                                      (mapFL_FL hopefully to_be_sent)
  signString (parseFlags O.sign opts) unsig_bundle

sendBundle :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag] -> FL (PatchInfoAnd p) wX wY
             -> Doc -> String -> [WhatToDo] -> String -> IO ()
sendBundle opts to_be_sent bundle fname wtds their_name=
         let
           auto_subject :: forall pp wA wB . FL (PatchInfoAnd pp) wA wB -> String
           auto_subject (p:>:NilFL)  = "darcs patch: " ++ trim (patchDesc p) 57
           auto_subject (p:>:ps) = "darcs patch: " ++ trim (patchDesc p) 43 ++
                            " (and " ++ show (lengthFL ps) ++ " more)"
           auto_subject _ = error "Tried to get a name from empty patch list."
           trim st n = if length st <= n then st
                       else take (n-3) st ++ "..."
           in do
           thetargets <- getTargets wtds
           from <- getAuthor (hasAuthor opts) False
           let thesubject = fromMaybe (auto_subject to_be_sent) $ getSubject opts
           (mailcontents, mailfile, mailcharset) <- getDescription opts their_name to_be_sent

           let warnMailBody = case mailfile of
                                  Just mf -> putDocLn $ Msg.emailBackedUp mf
                                  Nothing -> return ()

               warnCharset msg = do
                 confirmed <- promptYorn $ Msg.promptCharSetWarning msg
                 unless confirmed $ do
                    putDocLn Msg.charsetAborted
                    warnMailBody
                    exitSuccess

           thecharset <- case getCharset opts of
                              -- Always trust provided charset
                              providedCset@(Just _) -> return providedCset
                              Nothing ->
                                case mailcharset of
                                Nothing -> do
                                  warnCharset Msg.charsetCouldNotGuess
                                  return mailcharset
                                Just "utf-8" -> do
                                  -- Check the locale encoding for consistency
                                  encoding <- getSystemEncoding
                                  debugMessage $ Msg.currentEncodingIs encoding
                                  unless (isUTF8Locale encoding) $
                                    warnCharset Msg.charsetUtf8MailDiffLocale
                                  return mailcharset
                                -- Trust other cases (us-ascii)
                                Just _ -> return mailcharset

           let body = makeEmail their_name
                        (maybe [] (\x -> [("In-Reply-To", x), ("References", x)]) . getInReplyTo $ opts)
                        (Just mailcontents)
                        thecharset
                        bundle
                        (Just fname)
               contentAndBundle = Just (mailcontents, bundle)

               sendmail = do
                 sm_cmd <- getSendmailCmd opts
                 let to = generateEmailToString thetargets
                 sendEmailDoc from to thesubject (getCc opts)
                               sm_cmd contentAndBundle body >>
                  putInfo opts (Msg.success to (getCc opts))
                 `catch` \e -> do warnMailBody
                                  fail $ ioeGetErrorString e

           when (null [ p | Post p <- thetargets]) sendmail
           nbody <- withOpenTemp $ \ (fh,fn) -> do
               let to = generateEmailToString thetargets
               generateEmail fh from to thesubject (getCc opts) body
               hClose fh
               mmapFilePS fn
           forM_ [ p | Post p <- thetargets]
             (\url -> do
                putInfo opts $ Msg.postingPatch url
                postUrl url (BC.unpack nbody) "message/rfc822")
             `catch` (\(_ :: IOException) -> sendmail)
           cleanup opts mailfile

generateEmailToString :: [WhatToDo] -> String
generateEmailToString = intercalate " , " . filter (/= "") . map extractEmail
  where
    extractEmail (SendMail t) = t
    extractEmail _ = ""

cleanup :: (FilePathLike t) => [DarcsFlag] -> Maybe t -> IO ()
cleanup opts (Just mailfile) = when (isNothing (hasLogfile opts) || willRemoveLogFile opts) $
                                      removeFileMayNotExist mailfile
cleanup _ Nothing = return ()

writeBundleToFile :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
                  => [DarcsFlag] -> FL (PatchInfoAnd p) wX wY -> Doc ->
                    AbsolutePathOrStd -> [WhatToDo] -> String -> IO ()
writeBundleToFile opts to_be_sent bundle fname wtds their_name =
    do (d,f,_) <- getDescription opts their_name to_be_sent
       let putabs a = do writeDocBinFile a (d $$ bundle)
                         putDocLn (Msg.wroteBundle a)
           putstd = putDoc (d $$ bundle)
       useAbsoluteOrStd putabs putstd fname
       let to = generateEmailToString wtds
       unless (null to) $ putInfo opts $ Msg.savedButNotSent to
       cleanup opts f

data WhatToDo
    = Post String        -- ^ POST the patch via HTTP
    | SendMail String    -- ^ send patch via email

decideOnBehavior :: RepoPatch p => [DarcsFlag] -> Maybe (Repository p wR wU wT) -> IO [WhatToDo]
decideOnBehavior opts remote_repo =
    case the_targets of
    [] -> do wtds <- case remote_repo of
                     Nothing -> return []
                     Just r -> check_post r
             unless (null wtds) $ announce_recipients wtds
             return wtds
    ts -> do announce_recipients ts
             return ts
    where the_targets = collectTargets opts
#ifdef HAVE_HTTP
          -- the ifdef above is to so that darcs only checks the remote
          -- _darcs/post if we have an implementation of postUrl.  See
          -- our HTTP module for more details
          check_post the_remote_repo =
                       do p <- ((readPost . BC.unpack) `fmap`
                                fetchFilePS (prefsUrl the_remote_repo++"/post")
                                (MaxAge 600)) `catchall` return []
                          emails <- who_to_email the_remote_repo
                          return (p++emails)
          readPost = map parseLine . lines where
              parseLine t = maybe (Post t) SendMail $ stripPrefix "mailto:" t
#else
          check_post = who_to_email
#endif
          who_to_email the_remote_repo =
              do email <- (BC.unpack `fmap`
                           fetchFilePS (prefsUrl the_remote_repo++"/email")
                                       (MaxAge 600))
                          `catchall` return ""
                 if '@' `elem` email then return . map SendMail $ lines email
                                     else return []
          announce_recipients emails =
            let pn (SendMail s) = s
                pn (Post p) = p
                msg = Msg.willSendTo (dryRun opts) (map pn emails)
            in if DryRun `elem` opts
            then putInfo opts msg
            else when (null the_targets && isNothing (getOutput opts "")) $
                 putInfo opts msg

getTargets :: [WhatToDo] -> IO [WhatToDo]
getTargets [] = fmap ((:[]) . SendMail) $ askUser Msg.promptTarget
getTargets wtds = return wtds

collectTargets :: [DarcsFlag] -> [WhatToDo]
collectTargets flags = [ f t | Target t <- flags ] where
    f url | "http:" `isPrefixOf` url = Post url
    f em = SendMail em

getDescription :: (RepoPatch p, ApplyState p ~ Tree)
               => [DarcsFlag] -> String -> FL (PatchInfoAnd p) wX wY -> IO (Doc, Maybe String, Maybe String)
getDescription opts their_name patches =
    case get_filename of
        Just file -> do
                     when (editDescription opts) $ do
                       when (isNothing $ hasLogfile opts) $
                            writeDocBinFile file patchdesc
                       debugMessage $ Msg.aboutToEdit file
                       (_, changed) <- editFile file
                       unless changed $ do
                         confirmed <- promptYorn Msg.promptNoDescriptionChange
                         unless confirmed $ do putDocLn Msg.aborted
                                               exitSuccess
                       return ()
                     
                     updatedFile <- updateFilename file
                     doc <- readDocBinFile updatedFile
                     
                     return (doc, Just updatedFile, tryGetCharset doc)
        Nothing -> return (patchdesc, Nothing, tryGetCharset patchdesc)
    where patchdesc = text (show len)
                      <+> text (englishNum len (Noun "patch") "")
                      <+> text "for repository" <+> text their_name <> text ":"
                      $$ text ""
                      $$ vsep (mapFL description patches)
            where
              len = lengthFL patches
          updateFilename file = 
                maybe (renameFile file darcsSendMessageFinal >>
                       return darcsSendMessageFinal) (return . toFilePath) $ hasLogfile opts
          get_filename = case hasLogfile opts of
                                Just f -> Just $ toFilePath f
                                Nothing -> if editDescription opts
                                              then Just darcsSendMessage
                                              else Nothing
          tryGetCharset content = let body = renderPS Standard content in
                                  if isAscii body
                                  then Just "us-ascii"
                                  else either (const Nothing)
                                              (const $ Just "utf-8")
                                              (decodeUtf8' body)
