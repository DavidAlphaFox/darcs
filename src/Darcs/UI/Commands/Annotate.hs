--  Copyright (C) 2003 David Roundy, 2010-2011 Petr Rockai
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
{-# LANGUAGE CPP, OverloadedStrings #-}

module Darcs.UI.Commands.Annotate ( annotate ) where

import Prelude hiding ( (^) )

import Control.Arrow ( first )
import Control.Monad ( unless )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Flags ( DarcsFlag(NoPatchIndexFlag), isUnified, useCache, fixSubPaths, hasSummary, umask )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise
                        , defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Repository.State ( readRecorded )
import Darcs.Repository
    ( withRepository
    , withRepoLockCanFail
    , RepoJob(..)
    , readRepo
    , repoPatchType
    , listRegisteredFiles
    )
import Darcs.Repository.Flags ( UpdateWorking(..) )
import Darcs.Repository.PatchIndex ( attemptCreatePatchIndex )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch ( RepoPatch, Named, patch2patchinfo, invertRL )
import qualified Darcs.Patch ( summary )
import Darcs.Patch.Type ( PatchType(..) )
import Darcs.Patch.Dummy ( DummyPatch )
import qualified Data.ByteString.Char8 as BC ( pack, concat, intercalate )
import Data.ByteString.Lazy ( toChunks )
import Darcs.UI.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Patch.ApplyMonad( withFileNames )
import System.FilePath.Posix ( (</>) )
import Darcs.Patch.Info ( showPatchInfoUI, showPatchInfo )
import Darcs.Patch.Match ( matchPatch, haveNonrangeMatch, getNonrangeMatchS  )
import Darcs.Repository.Match ( getFirstMatch, getOnePatchset )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Repository.PatchIndex ( getRelevantSubsequence, canUsePatchIndex )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), Sealed(..), seal )
import qualified Darcs.Patch.Annotate as A
import Darcs.Util.Printer ( putDocLn, Doc )

import Storage.Hashed.Tree( TreeItem(..), readBlob, list, expand )
import Storage.Hashed.Monad( findM, virtualTreeIO )
import Darcs.Util.Path( floatPath, anchorPath, fp2fn, toFilePath
                      , AbsolutePath )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm(MyersDiff) )

#include "impossible.h"

annotateDescription :: String
annotateDescription = "Display which patch last modified something."

annotateHelp :: String
annotateHelp = unlines
 [ "The `darcs annotate` command provides two unrelated operations.  When"
 , "called on a file, it will find the patch that last modified each line"
 , "in that file.  When called on a patch (e.g. using `--patch`), it will"
 , "print the internal representation of that patch."
 , ""
 , "The `--summary` option will result in a summarized patch annotation,"
 , "similar to `darcs whatsnew`.  It has no effect on file annotations."
 , ""
 , "By default, output is in a human-readable format.  The `--machine-readable`"
 , "option can be used to generate output for machine postprocessing."
 ]

annotateBasicOpts :: DarcsOption a
                     (Maybe O.Summary
                      -> O.WithContext
                      -> Bool
                      -> [O.MatchFlag]
                      -> Maybe String
                      -> a)
annotateBasicOpts = O.summary
                  ^ O.withContext
                  ^ O.machineReadable
                  ^ O.matchOne
                  ^ O.workingRepoDir

annotateAdvancedOpts :: DarcsOption a (O.WithPatchIndex -> a)
annotateAdvancedOpts = O.patchIndexYes

annotateOpts :: DarcsOption a
                (Maybe O.Summary
                 -> O.WithContext
                 -> Bool
                 -> [O.MatchFlag]
                 -> Maybe String
                 -> Maybe O.StdCmdAction
                 -> Bool
                 -> Bool
                 -> O.Verbosity
                 -> Bool
                 -> O.WithPatchIndex
                 -> O.UseCache
                 -> Maybe String
                 -> Bool
                 -> Maybe String
                 -> Bool
                 -> a)
annotateOpts = annotateBasicOpts `withStdOpts` annotateAdvancedOpts

annotate :: DarcsCommand [DarcsFlag]
annotate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "annotate"
    , commandHelp = annotateHelp
    , commandDescription = annotateDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = annotateCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc annotateAdvancedOpts
    , commandBasicOptions = odesc annotateBasicOpts
    , commandDefaults = defaultFlags annotateOpts
    , commandCheckOptions = ocheck annotateOpts
    , commandParseOptions = onormalise annotateOpts
}

annotateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
annotateCmd fps opts [""] = annotate' fps opts [] -- when does that happen?
annotateCmd fps opts [] = do
  let matchFlags = parseFlags O.matchOne opts
  unless (haveNonrangeMatch (PatchType :: PatchType DummyPatch) matchFlags) $
      fail $ "Annotate requires either a patch pattern or a " ++
               "file or directory argument."
  annotate' fps opts []
annotateCmd fps opts args = annotate' fps opts args

annotate' :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
annotate' _ opts [] =    -- annotating a patch (ie, showing its contents)
  withRepository (useCache opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchOne opts
  Sealed2 p <- matchPatch matchFlags `fmap` readRepo repository
  if hasSummary O.NoSummary opts == O.YesSummary
     then do putDocLn $ showpi $ patch2patchinfo p
             putDocLn $ show_summary p
     else if isUnified opts == O.YesContext
          then withTempDir "context" $ \_ ->
               do getFirstMatch repository matchFlags
                  c <- readPlainTree "."
                  contextualPrintPatch c p
          else printPatch p
    where showpi | parseFlags O.machineReadable opts = showPatchInfo
                 | otherwise                         = showPatchInfoUI
          show_summary :: RepoPatch p => Named p wX wY -> Doc
          show_summary = Darcs.Patch.summary

annotate' fps opts args@[_] = do -- annotating a file or a directory
 unless (NoPatchIndexFlag `elem` opts)
   $ withRepoLockCanFail (useCache opts) YesUpdateWorking (umask opts) $ RepoJob attemptCreatePatchIndex
 withRepository (useCache opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchOne opts
  r <- readRepo repository
  (origpath:_) <- fixSubPaths fps args
  recorded <- readRecorded repository

  (patches, initial, path') <-
    if haveNonrangeMatch (repoPatchType repository) matchFlags
       then do Sealed x <- getOnePatchset repository matchFlags
               let fn = [fp2fn $ toFilePath origpath]
                   nonRangeMatch = getNonrangeMatchS matchFlags r
                   (_, [path], _) = withFileNames Nothing fn nonRangeMatch
               initial <- snd `fmap` virtualTreeIO (getNonrangeMatchS matchFlags r) recorded
               return (seal $ newset2RL x, initial, toFilePath path)
       else return (seal $ newset2RL r, recorded, toFilePath origpath)
  let path = "./" ++ path'

  found <- findM initial (floatPath $ toFilePath path)
  -- TODO need to decide about the --machine flag
  let fmt = if parseFlags O.machineReadable opts then A.machineFormat else A.format
  case found of
    Nothing -> fail $ "No such file or directory: " ++ toFilePath path
    Just (SubTree s) -> do
      s' <- expand s
      let subs = map (fp2fn . (path </>) . anchorPath "" . fst) $ list s'
          showPath (n, File _) = BC.pack (path </> n)
          showPath (n, _) = BC.concat [BC.pack (path </> n), "/"]
      (Sealed ans_patches) <- do
         upi <- canUsePatchIndex repository
         if not upi
            then return patches
            else getRelevantSubsequence patches repository subs
      putStrLn $ fmt (BC.intercalate "\n" $
                        map (showPath . first (anchorPath "")) $ list s') $
        A.annotateDirectory D.MyersDiff (invertRL ans_patches) (fp2fn path) subs
    Just (File b) -> do (Sealed ans_patches) <- do
                           upi <- canUsePatchIndex repository
                           if not upi
                              then return patches
                              else getRelevantSubsequence patches repository [fp2fn path]
                        con <- BC.concat `fmap` toChunks `fmap` readBlob b
                        putStrLn $ fmt con $ A.annotate D.MyersDiff (invertRL ans_patches) (fp2fn path) con
    Just (Stub _ _) -> impossible

annotate' _ _ _ = fail "annotate accepts at most one argument"
