--  Copyright (C) 2002-2014 David Roundy, Petr Rockai, Owen Stephens
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

{-# LANGUAGE CPP, MagicHash, OverloadedStrings, DeriveDataTypeable #-}

module Darcs.UI.Commands.Convert ( convert ) where

import Prelude hiding ( (^), readFile, log, lex )

import System.FilePath.Posix ( (</>) )
import System.Directory ( setCurrentDirectory, doesDirectoryExist, doesFileExist,
                   createDirectory, removeFile )
import System.IO ( stdin )
import Data.IORef ( newIORef, modifyIORef, readIORef )
import Data.Char ( isSpace )
import Control.Monad ( when, unless, void, forM_ )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State.Strict ( gets, modify )
import Control.Exception ( finally )
import Control.Applicative ( (<|>) )

import GHC.Base ( unsafeCoerce# )
import System.Time ( toClockTime )
import Data.Maybe ( catMaybes, fromMaybe )
import qualified Data.IntMap as M

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8 as BLU

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8( (<?>) )

import qualified Storage.Hashed.Tree as T
import qualified Storage.Hashed.Monad as TM
import Storage.Hashed.Monad hiding ( createDirectory, exists, rename )
import Storage.Hashed.Darcs ( hashedTreeIO, darcsAddMissingHashes )
import Storage.Hashed.Tree( Tree, treeHash, readBlob, TreeItem(..)
                          , emptyTree, listImmediate, findTree )
import Storage.Hashed.AnchoredPath( anchorPath, appendPath, floatPath
                                  , AnchoredPath(..), Name(..)  )
import Storage.Hashed.Hash( encodeBase16, sha256, Hash(..) )

import Darcs.Util.DateTime ( formatDateTime, fromClockTime, parseDateTime, startOfTime )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Path ( ioAbsoluteOrRemote, toPath, AbsolutePath )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Prompt ( askUser )
import Darcs.Util.Printer ( text, ($$) )
import Darcs.Util.Printer.Color ( traceDoc )
import Darcs.Util.Workaround ( getCurrentDirectory )

import Darcs.Patch.Depends ( getUncovered )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, info, hopefully )
import Darcs.Patch ( Named, showPatch, patch2patchinfo, fromPrim, fromPrims,
                     infopatch, adddeps, getdeps, effect, patchcontents,
                     RepoPatch, apply, listTouchedFiles )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), (=/\=) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), bunchFL, mapFL, mapFL_FL,
    concatFL, mapRL, nullFL, (+>+) )
import Darcs.Patch.Witnesses.Sealed ( FlippedSeal(..), Sealed(..), unFreeLeft
                                    ,  flipSeal, unsafeUnsealFlipped )
import Darcs.Patch.Info ( piRename, piTag, isTag, PatchInfo, patchinfo,
                          piName, piLog, piDate, piAuthor, makePatchname )
import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.V1.Commute ( publicUnravel )
import Darcs.Patch.V1.Core ( Patch(PP), isMerger )
import Darcs.Patch.V2.Real ( mergeUnravelled )
import Darcs.Patch.Prim ( sortCoalesceFL )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), newset2RL, newset2FL )
import Darcs.Patch.Progress ( progressFL )

import Darcs.Repository.Flags ( UpdateWorking(..), Reorder (..), UseIndex(..), ScanKnown(..)
                              , AllowConflicts(..), ExternalMerge(..), WantGuiPause(..)
                              , Compression(..), DryRun(NoDryRun), DiffAlgorithm(MyersDiff, PatienceDiff) )
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..), withRepositoryDirectory,
                          createRepository, invalidateIndex,
                          tentativelyMergePatches, patchSetToPatches,
                          createPristineDirectoryTree,
                          revertRepositoryChanges, finalizeRepositoryChanges,
                          applyToWorking
                        , readRepo, readTentativeRepo, cleanRepository )
import qualified Darcs.Repository as R( setScriptsExecutable )
import Darcs.Repository.State( readRecorded )
import Darcs.Repository.Cache ( HashedDir( HashedPristineDir ) )
import Darcs.Repository.InternalTypes ( extractCache )
import Darcs.Repository.HashedRepo ( readHashedPristineRoot, addToTentativeInventory )
import Darcs.Repository.HashedIO ( cleanHashdir )
import Darcs.Repository.Prefs( FileType(..) )
import Darcs.Repository.Format(identifyRepoFormat, formatHas, RepoProperty(Darcs2))
import Darcs.Repository.Motd ( showMotd )
import Darcs.Repository.Lock ( writeBinFile )
import Darcs.Repository.External ( fetchFilePS, Cachable(Uncachable) )
import Darcs.Repository.Diff( treeDiff )


import Darcs.UI.External ( catchall )
import Darcs.UI.Flags
    ( verbosity, useCache, umask, withWorkingDir, runPatchIndex
    , DarcsFlag ( NewRepo )
    , getRepourl, patchFormat
    )
import Darcs.UI.Commands ( DarcsCommand(..), amInRepository, nodefaults, putInfo
                         , normalCommand, withStdOpts )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O

#include "impossible.h"

convertDescription :: String
convertDescription = "Convert repositories between various formats."

convertHelp :: String
convertHelp = unlines
 [ "This command converts a repository that uses the old patch semantics"
 , "`darcs-1` to a new repository with current `darcs-2` semantics."
 , ""
 , convertHelp'
 ]

-- | This part of the help is split out because it is used twice: in
-- the help string, and in the prompt for confirmation.
convertHelp' :: String
convertHelp' = unlines
 [ "WARNING: the repository produced by this command is not understood by"
 , "Darcs 1.x, and patches cannot be exchanged between repositories in"
 , "darcs-1 and darcs-2 formats."
 , ""
 , "Furthermore, repositories created by different invocations of"
 , "this command SHOULD NOT exchange patches."
 ]

convertExportHelp :: String
convertExportHelp = unlines
 [ "This command enables you to export darcs repositories into git."
 , ""
 , "For a one-time export you can use the recipe:"
 , ""
 , "    $ cd repo"
 , "    $ git init ../mirror"
 , "    $ darcs convert export | (cd ../mirror && git fast-import)"
 , ""
 , "For incremental export using marksfiles:"
 , ""
 , "    $ cd repo"
 , "    $ git init ../mirror"
 , "    $ touch ../mirror/git.marks"
 , "    $ darcs convert export --read-marks darcs.marks --write-marks darcs.marks"
 , "       | (cd ../mirror && git fast-import --import-marks=git.marks --export-marks=git.marks)"
 , ""
 , "In the case of incremental export, be careful to never amend, delete or"
 , "reorder patches in the source darcs repository."
 , ""
 , "Also, be aware that exporting a darcs repo to git will not be exactly"
 , "faithful in terms of history if the darcs repository contains conflicts."
 , ""
 , "Limitations:"
 , ""
 , "* Empty directories are not supported by the fast-export protocol."
 , "* Unicode filenames are currently not correctly handled."
 , "  See http://bugs.darcs.net/issue2359 ."
 ]

convertImportHelp :: String
convertImportHelp = unlines
 [ "This command imports git repositories into new darcs repositories."
 , "Further options are accepted (see `darcs help init`)."
 , ""
 , "To convert a git repo to a new darcs one you may run:"
 , "    $ (cd gitrepo && git fast-export --all) | darcs convert import darcsmirror"
 , ""
 , "WARNING: git repositories with branches will produce weird results,"
 , "         use at your own risks."
 , ""
 , "Incremental import with marksfiles is currently not supported."
 ]

convert :: DarcsCommand [DarcsFlag]
convert = SuperCommand {
      commandProgramName = "darcs"
    , commandName = "convert"
    , commandHelp = ""
    , commandDescription = convertDescription
    , commandPrereq = amInRepository
    , commandSubCommands = [  normalCommand convertDarcs2,
                              normalCommand convertExport,
                              normalCommand convertImport
                           ]
    }

convertDarcs2BasicOpts :: DarcsOption a (Maybe String -> O.SetScriptsExecutable -> O.WithWorkingDir -> a)
convertDarcs2BasicOpts = O.reponame ^ O.setScriptsExecutable ^ O.useWorkingDir

convertDarcs2AdvancedOpts :: DarcsOption a (O.NetworkOptions -> O.WithPatchIndex -> a)
convertDarcs2AdvancedOpts = O.network ^ O.patchIndex

convertDarcs2Opts :: DarcsOption a
                     (Maybe String
                      -> O.SetScriptsExecutable
                      -> O.WithWorkingDir
                      -> Maybe O.StdCmdAction
                      -> Bool
                      -> Bool
                      -> O.Verbosity
                      -> Bool
                      -> O.NetworkOptions
                      -> O.WithPatchIndex
                      -> O.UseCache
                      -> Maybe String
                      -> Bool
                      -> Maybe String
                      -> Bool
                      -> a)
convertDarcs2Opts = convertDarcs2BasicOpts `withStdOpts` convertDarcs2AdvancedOpts

convertDarcs2SilentOpts :: DarcsOption a (O.PatchFormat -> a)
convertDarcs2SilentOpts = O.patchFormat

convertDarcs2 :: DarcsCommand [DarcsFlag]
convertDarcs2 = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "darcs-2"
    , commandHelp = convertHelp
    , commandDescription = "Convert darcs-1 repository to the darcs-2 patch format"
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<SOURCE>", "[<DESTINATION>]"]
    , commandCommand = toDarcs2
    , commandPrereq = \_ -> return $ Right ()
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertDarcs2AdvancedOpts
    , commandBasicOptions = odesc convertDarcs2BasicOpts
    , commandDefaults = defaultFlags (convertDarcs2Opts ^ convertDarcs2SilentOpts)
    , commandCheckOptions = ocheck convertDarcs2Opts
    , commandParseOptions = onormalise convertDarcs2Opts
    }

convertExportBasicOpts :: DarcsOption a
                          (Maybe String -> Maybe String -> Maybe String -> a)
convertExportBasicOpts = O.reponame ^ O.marks

convertExportAdvancedOpts :: DarcsOption a (O.NetworkOptions -> a)
convertExportAdvancedOpts = O.network

convertExportOpts :: DarcsOption a
                     (Maybe String
                      -> Maybe String
                      -> Maybe String
                      -> Maybe O.StdCmdAction
                      -> Bool
                      -> Bool
                      -> O.Verbosity
                      -> Bool
                      -> O.NetworkOptions
                      -> O.UseCache
                      -> Maybe String
                      -> Bool
                      -> Maybe String
                      -> Bool
                      -> a)
convertExportOpts = convertExportBasicOpts `withStdOpts` convertExportAdvancedOpts

convertExport :: DarcsCommand [DarcsFlag]
convertExport = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "export"
    , commandHelp = convertExportHelp
    , commandDescription = "Export a darcs repository to a git-fast-import stream"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = fastExport
    , commandPrereq = amInRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertExportAdvancedOpts
    , commandBasicOptions = odesc convertExportBasicOpts
    , commandDefaults = defaultFlags convertExportOpts
    , commandCheckOptions = ocheck convertExportOpts
    , commandParseOptions = onormalise convertExportOpts
    }

convertImportBasicOpts :: DarcsOption a
                          (Maybe String
                           -> O.SetScriptsExecutable
                           -> O.PatchFormat
                           -> O.WithWorkingDir
                           -> a)
convertImportBasicOpts
  = O.reponame
  ^ O.setScriptsExecutable
  ^ O.patchFormat
  ^ O.useWorkingDir

convertImportAdvancedOpts :: DarcsOption a (O.WithPatchIndex -> a)
convertImportAdvancedOpts = O.patchIndex

convertImportOpts :: DarcsOption a
                     (Maybe String
                      -> O.SetScriptsExecutable
                      -> O.PatchFormat
                      -> O.WithWorkingDir
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
convertImportOpts = convertImportBasicOpts `withStdOpts` convertImportAdvancedOpts

convertImport :: DarcsCommand [DarcsFlag]
convertImport = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "import"
    , commandHelp = convertImportHelp
    , commandDescription = "Import from a git-fast-export stream into darcs"
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DIRECTORY>]"]
    , commandCommand = fastImport
    , commandPrereq = \_ -> return $ Right ()
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertImportAdvancedOpts
    , commandBasicOptions = odesc convertImportBasicOpts
    , commandDefaults = defaultFlags convertImportOpts
    , commandCheckOptions = ocheck convertImportOpts
    , commandParseOptions = onormalise convertImportOpts
    }

toDarcs2 :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
toDarcs2 fps opts [inrepodir, outname] = toDarcs2 fps (NewRepo outname:opts) [inrepodir]
toDarcs2 _ opts [inrepodir] = do

  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir

  --test for converting darcs-2 repository
  format <- identifyRepoFormat repodir
  when (formatHas Darcs2 format) $ fail "Repository is already in darcs 2 format."

  putStrLn convertHelp'
  let vow = "I understand the consequences of my action"
  putStrLn "Please confirm that you have read and understood the above"
  vow' <- askUser ("by typing `" ++ vow ++ "': ")
  when (vow' /= vow) $ fail "User didn't understand the consequences."

  unless (parseFlags O.verbosity opts == O.Quiet) $ showMotd repodir
  mysimplename <- makeRepoName opts repodir
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  createRepository False (withWorkingDir opts) (runPatchIndex opts)
  writeBinFile (darcsdir++"/hashed_inventory") ""
  withRepoLock NoDryRun (useCache opts) NoUpdateWorking (umask opts) $ V2Job $ \repository ->
    withRepositoryDirectory (useCache opts) repodir $ V1Job $ \themrepo -> do
      theirstuff <- readRepo themrepo
      let patches = mapFL_FL convertNamed $ patchSetToPatches theirstuff
          outOfOrderTags = catMaybes $ mapRL oot $ newset2RL theirstuff
              where oot t = if isTag (info t) && info t `notElem` inOrderTags theirstuff
                            then Just (info t, getdeps $ hopefully t)
                            else Nothing
          fixDep p = case lookup p outOfOrderTags of
                     Just d -> p : concatMap fixDep d
                     Nothing -> [p]
          convertOne :: Patch Prim wX wY -> FL (RealPatch Prim) wX wY
          convertOne x | isMerger x = case mergeUnravelled $ publicUnravel x of
                                       Just (FlippedSeal y) ->
                                           case effect y =/\= effect x of
                                           IsEq -> y :>: NilFL
                                           NotEq ->
                                               traceDoc (text "lossy conversion:" $$
                                                         showPatch x)
                                               fromPrims (effect x)
                                       Nothing -> traceDoc (text
                                                            "lossy conversion of complicated conflict:" $$
                                                            showPatch x)
                                                  fromPrims (effect x)
          convertOne (PP x) = fromPrim x :>: NilFL
          convertOne _ = impossible
          convertFL :: FL (Patch Prim) wX wY -> FL (RealPatch Prim) wX wY
          convertFL = concatFL . mapFL_FL convertOne
          convertNamed :: Named (Patch Prim) wX wY -> PatchInfoAnd (RealPatch Prim) wX wY
          convertNamed n = n2pia $
                           adddeps (infopatch (convertInfo $ patch2patchinfo n) $
                                              convertFL $ patchcontents n)
                                   (map convertInfo $ concatMap fixDep $ getdeps n)
          convertInfo n | n `elem` inOrderTags theirstuff = n
                        | otherwise = maybe n (\t -> piRename n ("old tag: "++t)) $ piTag n
          applySome xs = do -- TODO this unsafeCoerce hack is because we don't keep track of the repository state properly
                            -- Really sequence_ $ mapFL applySome below should instead be a repeated add operation -
                            -- there doesn't seem to be any reason we need to do a merge here.
                            let repository2 = unsafeCoerce# repository :: Repository (RealPatch Prim) wA wB wA
                            Sealed pw <- tentativelyMergePatches repository2 "convert"
                                             YesAllowConflicts NoUpdateWorking
                                             NoExternalMerge NoWantGuiPause
                                             GzipCompression (verbosity opts)
                                             NoReorder
                                             (UseIndex, ScanKnown, MyersDiff)
                                             NilFL xs
                            finalizeRepositoryChanges repository2 NoUpdateWorking GzipCompression -- this is to clean out pristine.hashed
                            revertRepositoryChanges repository2 NoUpdateWorking
                            _ <- revertable $ applyToWorking repository2 (verbosity opts) pw
                            invalidateIndex repository2
      sequence_ $ mapFL applySome $ bunchFL 100 $ progressFL "Converting patch" patches
      invalidateIndex repository
      revertable $ createPristineDirectoryTree repository "." (withWorkingDir opts)
      when (parseFlags O.setScriptsExecutable opts == O.YesSetScriptsExecutable)
        R.setScriptsExecutable

      -- Copy over the prefs file
      let prefsRelPath = darcsdir </> "prefs" </> "prefs"
      (fetchFilePS (repodir </> prefsRelPath) Uncachable >>= B.writeFile prefsRelPath)
       `catchall` return ()

      putInfo opts $ text "Finished converting."
      where revertable x = x `clarifyErrors` unlines
                  ["An error may have left your new working directory an inconsistent",
                   "but recoverable state. You should be able to make the new",
                   "repository consistent again by running darcs revert -a."]

toDarcs2 _ _ _ = fail "You must provide either one or two arguments."

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName (NewRepo n:_) _ =
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
makeRepoName (_:as) d = makeRepoName as d
makeRepoName [] d =
  case dropWhile (=='.') $ reverse $
       takeWhile (\c -> c /= '/' && c /= ':') $
       dropWhile (=='/') $ reverse d of
  "" -> modifyRepoName "anonymous_repo"
  base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i

fastExport :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fastExport _ opts _ = do
  let repodir = fromMaybe "." $ getRepourl opts
  marks <- case parseFlags O.readMarks opts of
    Nothing -> return emptyMarks
    Just f  -> readMarks f
  newMarks <- withRepositoryDirectory (useCache opts) repodir $ RepoJob $ \repo -> fastExport' repo marks
  case parseFlags O.writeMarks opts of
    Nothing -> return ()
    Just f  -> writeMarks f newMarks

fastExport' :: (RepoPatch p, ApplyState p ~ Tree) => Repository p r u r -> Marks -> IO Marks
fastExport' repo marks = do
  putStrLn "progress (reading repository)"
  patchset <- readRepo repo
  marksref <- newIORef marks
  let patches = newset2FL patchset
      tags = inOrderTags patchset
      mark :: (PatchInfoAnd p) x y -> Int -> TreeIO ()
      mark p n = liftIO $ do putStrLn $ "mark :" ++ show n
                             modifyIORef marksref $ \m -> addMark m n (patchHash p)
      -- apply a single patch to build the working tree of the last exported version
      checkOne :: (RepoPatch p, ApplyState p ~ Tree)
               => Int -> (PatchInfoAnd p) x y -> TreeIO ()
      checkOne n p = do apply p
                        unless (inOrderTag tags p ||
                                (getMark marks n == Just (patchHash p))) $
                          fail $ "FATAL: Marks do not correspond: expected " ++
                                 (show $ getMark marks n) ++ ", got " ++ (BC.unpack $ patchHash p)
      -- build the working tree of the last version exported by convert --export
      check :: (RepoPatch p, ApplyState p ~ Tree)
            => Int -> FL (PatchInfoAnd p) x y -> TreeIO (Int,  FlippedSeal( FL (PatchInfoAnd p)) y) 
      check _ NilFL = return (1, flipSeal NilFL)
      check n allps@(p:>:ps)
        | n <= lastMark marks = checkOne n p >> check (next tags n p) ps
        | n > lastMark marks = return (n, flipSeal allps)
        | lastMark marks == 0 = return (1, flipSeal allps)
        | otherwise = undefined
  ((n, patches'), tree') <- hashedTreeIO (check 1 patches) emptyTree $ darcsdir </> "pristine.hashed"
  let patches'' = unsafeUnsealFlipped patches'
  void $ hashedTreeIO (dumpPatches tags mark n patches'') tree' $ darcsdir </> "pristine.hashed"
  readIORef marksref
 `finally` do
  putStrLn "progress (cleaning up)"
  current <- readHashedPristineRoot repo
  cleanHashdir (extractCache repo) HashedPristineDir $ catMaybes [current]
  putStrLn "progress done"

dumpPatches ::  (RepoPatch p, ApplyState p ~ Tree)
            =>  [PatchInfo]
            -> (forall p0 x0 y0 . (PatchInfoAnd p0) x0 y0 -> Int -> TreeIO ())
            -> Int -> FL (PatchInfoAnd p) x y -> TreeIO ()
dumpPatches _ _ _ NilFL = liftIO $ putStrLn "progress (patches converted)"
dumpPatches tags mark n (p:>:ps) = do
  apply p
  if inOrderTag tags p && n > 0
     then dumpTag p n
     else do dumpPatch mark p n
             dumpFiles $ map floatPath $ listTouchedFiles p
  dumpPatches tags mark (next tags n p) ps

dumpTag :: (PatchInfoAnd p) x y  -> Int -> TreeIO () 
dumpTag p n =
  dumpBits [ BLU.fromString $ "progress TAG " ++ cleanTagName p
           , BLU.fromString $ "tag " ++ cleanTagName p -- FIXME is this valid?
           , BLU.fromString $ "from :" ++ show (n - 1)
           , BLU.fromString $ unwords ["tagger", patchAuthor p, patchDate p]
           -- -3 == (-4 for "TAG " and +1 for newline)
           , BLU.fromString $ "data "
                 ++ show (BL.length (patchMessage p) - 3)
           , BL.drop 4 $ patchMessage p ]
   where
     -- FIXME forbidden characters and subsequences in tags:
     -- https://www.kernel.org/pub/software/scm/git/docs/git-check-ref-format.html
     cleanTagName = map cleanup . drop 4 . piName . info
         where cleanup x | x `elem` bad = '_'
                         | otherwise = x
               bad :: String
               bad = " ~^:"

dumpFiles :: [AnchoredPath] -> TreeIO ()
dumpFiles files = forM_ files $ \file -> do
  isfile <- fileExists file
  isdir <- directoryExists file
  when isfile $ do bits <- readFile file
                   dumpBits [ BLU.fromString $ "M 100644 inline " ++ anchorPath "" file
                            , BLU.fromString $ "data " ++ show (BL.length bits)
                            , bits ]
  when isdir $ do -- Always delete directory before dumping its contents. This fixes
                  -- a corner case when a same patch moves dir1 to dir2, and creates
                  -- another directory dir1.
                  -- As we always dump its contents anyway this is not more costly.
                  liftIO $ putStrLn $ "D " ++ anchorPath "" file
                  tt <- gets tree -- ick
                  let subs = [ file `appendPath` n | (n, _) <-
                                  listImmediate $ fromJust $ findTree tt file ]
                  dumpFiles subs
  when (not isfile && not isdir) $ liftIO $ putStrLn $ "D " ++ anchorPath "" file

dumpPatch ::  (forall p0 x0 y0 . (PatchInfoAnd p0) x0 y0 -> Int -> TreeIO ())
          -> (PatchInfoAnd p) x y -> Int
          -> TreeIO ()
dumpPatch mark p n =
  do dumpBits [ BLC.pack $ "progress " ++ show n ++ ": " ++ piName (info p)
              , "commit refs/heads/master" ]
     mark p n
     dumpBits [ BLU.fromString $ "committer " ++ patchAuthor p ++ " " ++ patchDate p
              , BLU.fromString $ "data " ++ show (BL.length $ patchMessage p)
              , patchMessage p ]
     when (n > 1) $ dumpBits [ BLU.fromString $ "from :" ++ show (n - 1) ]

dumpBits :: [BL.ByteString] -> TreeIO ()
dumpBits = liftIO . BLC.putStrLn . BL.intercalate "\n"

-- patchAuthor attempts to fixup malformed author strings
-- into format: "Name <Email>"
-- e.g.
-- <john@home>      -> john <john@home>
-- john@home        -> john <john@home>
-- john <john@home> -> john <john@home>
-- john <john@home  -> john <john@home>
-- <john>           -> john <unknown>
patchAuthor :: (PatchInfoAnd p) x y -> String
patchAuthor p
 | null author = unknownEmail "unknown"
 | otherwise = case span (/='<') author of
               -- No name, but have email (nothing spanned)
               ("", email) -> case span (/='@') (tail email) of
                   -- Not a real email address (no @).
                   (n, "") -> case span (/='>') n of
                       (name, _) -> unknownEmail name
                   -- A "real" email address.
                   (user, rest) -> case span (/= '>') (tail rest) of
                       (dom, _) -> mkAuthor user $ emailPad (user ++ "@" ++ dom)
               -- No email (everything spanned)
               (_, "") -> case span (/='@') author of
                   (n, "") -> unknownEmail n
                   (name, _) -> mkAuthor name $ emailPad author
               -- Name and email
               (n, rest) -> case span (/='>') $ tail rest of
                   (email, _) -> n ++ emailPad email
 where
   author = dropWhile isSpace $ piAuthor (info p)
   unknownEmail = flip mkAuthor "<unknown>"
   emailPad email = "<" ++ email ++ ">"
   mkAuthor name email = name ++ " " ++ email

patchDate :: (PatchInfoAnd p) x y -> String
patchDate = formatDateTime "%s +0000" . fromClockTime . toClockTime .
  piDate . info

patchMessage :: (PatchInfoAnd p) x y -> BLU.ByteString
patchMessage p = BL.concat [ BLU.fromString (piName $ info p)
                           , case unlines . piLog $ info p of
                                 "" -> BL.empty
                                 plog -> BLU.fromString ("\n\n" ++ plog)
                           ]

type Marked = Maybe Int
type Branch = B.ByteString
type AuthorInfo = B.ByteString
type Message = B.ByteString
type Content = B.ByteString

data RefId = MarkId Int | HashId B.ByteString | Inline
           deriving Show

data Object = Blob (Maybe Int) Content
            | Reset Branch (Maybe RefId)
            | Commit Branch Marked AuthorInfo Message
            | Tag Int AuthorInfo Message
            | Modify (Either Int Content) B.ByteString -- (mark or content), filename
            | Gitlink B.ByteString
            | Delete B.ByteString -- filename
            | From Int
            | Merge Int
            | Progress B.ByteString
            | End
            deriving Show

type Ancestors = (Marked, [Int])
data State = Toplevel Marked Branch
           | InCommit Marked Ancestors Branch (Tree IO) PatchInfo
           | Done

instance Show State where
  show (Toplevel _ _) = "Toplevel"
  show (InCommit _ _ _ _ _) = "InCommit"
  show Done =  "Done"

fastImport :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fastImport _ opts [outrepo] =
  do createDirectory outrepo
     withCurrentDirectory outrepo $ do
       createRepository (patchFormat opts == O.PatchFormat1) (withWorkingDir opts) (runPatchIndex opts)
       withRepoLock NoDryRun (useCache opts) NoUpdateWorking (umask opts) $ V2Job $ \repo -> do
         -- TODO implement --dry-run, which would be read-only?
         marks <- fastImport' repo emptyMarks
         createPristineDirectoryTree repo "." (withWorkingDir opts)
         return marks
fastImport _ _ _ = fail "I need exactly one output repository."

fastImport' :: forall p r u . (RepoPatch p, ApplyState p ~ Tree) =>
               Repository p r u r -> Marks -> IO ()
fastImport' repo marks = do
    pristine <- readRecorded repo
    marksref <- newIORef marks
    let initial = Toplevel Nothing $ BC.pack "refs/branches/master"

        go :: State -> B.ByteString -> TreeIO ()
        go state rest = do (rest', item) <- parseObject rest
                           state' <- process state item
                           case state' of
                             Done -> return ()
                             _ -> go state' rest'

        -- sort marks into buckets, since there can be a *lot* of them
        markpath :: Int -> AnchoredPath
        markpath n = floatPath (darcsdir </> "marks")
                        `appendPath` (Name $ BC.pack $ show (n `div` 1000))
                        `appendPath` (Name $ BC.pack $ show (n `mod` 1000))

        makeinfo author message tag = do
          let (name:log) = lines $ BC.unpack message
              (author'', date'') = span (/='>') $ BC.unpack author
              date' = dropWhile (`notElem` ("0123456789" :: String)) date''
              author' = author'' ++ ">"
              date = formatDateTime "%Y%m%d%H%M%S" $ case (parseDateTime "%s %z" date') of
                Just x -> x
                Nothing -> startOfTime
          liftIO $ patchinfo date (if tag then "TAG " ++ name else name) author' log

        addtag author msg =
          do info_ <- makeinfo author msg True
             gotany <- liftIO $ doesFileExist $ darcsdir </> "tentative_hashed_pristine"
             deps <- if gotany then liftIO $ getUncovered `fmap` readTentativeRepo repo
                               else return []
             let ident = NilFL :: FL (RealPatch Prim) cX cX
                 patch = adddeps (infopatch info_ ident) deps
             void $ liftIO $ addToTentativeInventory (extractCache repo)
                                                     GzipCompression (n2pia patch)

        -- processing items
        updateHashes = do
          let nodarcs = (\(AnchoredPath (Name x:_)) _ -> x /= BC.pack darcsdir)
              hashblobs (File blob@(T.Blob con NoHash)) =
                do hash <- sha256 `fmap` readBlob blob
                   return $ File (T.Blob con hash)
              hashblobs x = return x
          tree' <- liftIO . T.partiallyUpdateTree hashblobs nodarcs =<< gets tree
          modify $ \s -> s { tree = tree' }
          return $ T.filter nodarcs tree'

        process :: State -> Object -> TreeIO State
        process s (Progress p) = do
          liftIO $ putStrLn ("progress " ++ BC.unpack p)
          return s

        process (Toplevel _ _) End = do
          tree' <- (liftIO . darcsAddMissingHashes) =<< updateHashes
          modify $ \s -> s { tree = tree' } -- lets dump the right tree, without _darcs
          let root = encodeBase16 $ treeHash tree'
          liftIO $ do
            putStrLn $ "\\o/ It seems we survived. Enjoy your new repo."
            B.writeFile (darcsdir </> "tentative_pristine") $
              BC.concat [BC.pack "pristine:", root]
          return Done

        process (Toplevel n b) (Tag what author msg) = do
          if Just what == n
             then addtag author msg
             else liftIO $ putStrLn $ "WARNING: Ignoring out-of-order tag " ++
                             (head $ lines $ BC.unpack msg)
          return (Toplevel n b)

        process (Toplevel n _) (Reset branch from) =
          do case from of
               (Just (MarkId k)) | Just k == n ->
                 addtag (BC.pack "Anonymous Tagger <> 0 +0000") branch
               _ -> liftIO $ putStrLn $ "WARNING: Ignoring out-of-order tag " ++
                                        BC.unpack branch
             return $ Toplevel n branch

        process (Toplevel n b) (Blob (Just m) bits) = do
          TM.writeFile (markpath m) $ (BLC.fromChunks [bits])
          return $ Toplevel n b

        process x (Gitlink link) = do
          liftIO $ putStrLn $ "WARNING: Ignoring gitlink " ++ BC.unpack link
          return x

        process (Toplevel previous pbranch) (Commit branch mark author message) = do
          when (pbranch /= branch) $ do
            liftIO $ putStrLn ("Tagging branch: " ++ BC.unpack pbranch)
            addtag author pbranch
          info_ <- makeinfo author message False
          startstate <- updateHashes
          return $ InCommit mark (previous, []) branch startstate info_

        process s@(InCommit _ _ _ _ _) (Modify (Left m) path) = do
          TM.copy (markpath m) (floatPath $ BC.unpack path)
          return s

        process s@(InCommit _ _ _ _ _) (Modify (Right bits) path) = do
          TM.writeFile (floatPath $ BC.unpack path) (BLC.fromChunks [bits])
          return s

        process s@(InCommit _ _ _ _ _) (Delete path) = do
          TM.unlink (floatPath $ BC.unpack path)
          return s

        process (InCommit mark (prev, current) branch start info_) (From from) = do
          return $ InCommit mark (prev, from:current) branch start info_

        process (InCommit mark (prev, current) branch start info_) (Merge from) = do
          return $ InCommit mark (prev, from:current) branch start info_

        process (InCommit mark ancestors branch start info_) x = do
          case ancestors of
            (_, []) -> return () -- OK, previous commit is the ancestor
            (Just n, list)
              | n `elem` list -> return () -- OK, we base off one of the ancestors
              | otherwise -> liftIO $ putStrLn $
                               "WARNING: Linearising non-linear ancestry:" ++
                               " currently at " ++ show n ++ ", ancestors " ++ show list
            (Nothing, list) ->
              liftIO $ putStrLn $ "WARNING: Linearising non-linear ancestry " ++ show list

          current <- updateHashes
          Sealed diff
                <- unFreeLeft `fmap` (liftIO $ treeDiff PatienceDiff (const TextFile) start current)
          prims <- return $ fromPrims $ sortCoalesceFL diff
          let patch = infopatch info_ ((NilFL :: FL p cX cX) +>+ prims)
          void $ liftIO $ addToTentativeInventory (extractCache repo)
                                                  GzipCompression (n2pia patch)
          case mark of
            Nothing -> return ()
            Just n -> case getMark marks n of
              Nothing -> liftIO $ modifyIORef marksref $ \m -> addMark m n (patchHash $ n2pia patch)
              Just n' -> fail $ "FATAL: Mark already exists: " ++ BC.unpack n'
          process (Toplevel mark branch) x

        process state obj = do
          liftIO $ print obj
          fail $ "Unexpected object in state " ++ show state

    void $ hashedTreeIO (go initial B.empty) pristine $ darcsdir </> "pristine.hashed"
    finalizeRepositoryChanges repo YesUpdateWorking GzipCompression
    cleanRepository repo

parseObject :: BC.ByteString -> TreeIO ( BC.ByteString, Object )
parseObject = next' mbObject
  where mbObject = A.parse p_maybeObject

        p_maybeObject = Just `fmap` p_object
                        <|> (A.endOfInput >> return Nothing)

        lex p = p >>= \x -> A.skipSpace >> return x
        lexString s = A.string (BC.pack s) >> A.skipSpace
        line = lex $ A.takeWhile (/='\n')

        optional p = Just `fmap` p <|> return Nothing

        p_object = p_blob
                   <|> p_reset
                   <|> p_commit
                   <|> p_tag
                   <|> p_modify
                   <|> p_from
                   <|> p_merge
                   <|> p_delete
                   <|> (lexString "progress" >> Progress `fmap` line)

        p_author name = lexString name >> line

        p_reset = do lexString "reset"
                     branch <- line
                     refid <- optional $ lexString "from" >> p_refid
                     return $ Reset branch refid

        p_commit = do lexString "commit"
                      branch <- line
                      mark <- optional p_mark
                      _ <- optional $ p_author "author"
                      committer <- p_author "committer"
                      message <- p_data
                      return $ Commit branch mark committer message

        p_tag = do _ <- lexString "tag" >> line -- FIXME we ignore branch for now
                   lexString "from"
                   mark <- p_marked
                   author <- p_author "tagger"
                   message <- p_data
                   return $ Tag mark author message

        p_blob = do lexString "blob"
                    mark <- optional p_mark
                    Blob mark `fmap` p_data
                  <?> "p_blob"

        p_mark = do lexString "mark"
                    p_marked
                  <?> "p_mark"

        p_refid = MarkId `fmap` p_marked
                  <|> (lexString "inline" >> return Inline)
                  <|> HashId `fmap` p_hash

        p_data = do lexString "data"
                    len <- A.decimal
                    _ <- A.char '\n'
                    lex $ A.take len
                  <?> "p_data"

        p_marked = lex $ A.char ':' >> A.decimal
        p_hash = lex $ A.takeWhile1 (A.inClass "0123456789abcdefABCDEF")
        p_from = lexString "from" >> From `fmap` p_marked
        p_merge = lexString "merge" >> Merge `fmap` p_marked
        p_delete = lexString "D" >> Delete `fmap` line
        p_modify = do lexString "M"
                      mode <- lex $ A.takeWhile (A.inClass "01234567890")
                      mark <- p_refid
                      path <- line
                      case mark of
                        HashId hash | mode == BC.pack "160000" -> return $ Gitlink hash
                                    | otherwise -> fail ":(("
                        MarkId n -> return $ Modify (Left n) path
                        Inline -> do bits <- p_data
                                     return $ Modify (Right bits) path

        next' :: (B.ByteString -> A.Result (Maybe Object)) -> B.ByteString -> TreeIO (B.ByteString, Object)
        next' parser rest =
          do chunk <- if B.null rest then liftIO $ B.hGet stdin (64 * 1024)
                                     else return rest
             next_chunk parser chunk

        next_chunk :: (B.ByteString -> A.Result (Maybe Object)) -> B.ByteString -> TreeIO (B.ByteString, Object)
        next_chunk parser chunk =
          case parser chunk of
             A.Done rest result -> return (rest, maybe End id result) -- not sure about the maybe
             A.Partial cont -> next' cont B.empty
             A.Fail _ ctx err -> do
               liftIO $ putStrLn $ "=== chunk ===\n" ++ BC.unpack chunk ++ "\n=== end chunk ===="
               fail $ "Error parsing stream. " ++ err ++ "\nContext: " ++ show ctx


patchHash :: PatchInfoAnd p cX cY -> BC.ByteString
patchHash p = BC.pack $ show $ makePatchname (info p)

inOrderTag :: (Effect p) => [PatchInfo] -> PatchInfoAnd p wX wZ -> Bool
inOrderTag tags p = isTag (info p) && info p `elem` tags && nullFL (effect p)

next :: (Effect p) => [PatchInfo] -> Int ->  PatchInfoAnd p x y -> Int
next tags n p = if inOrderTag tags p then n else n + 1

inOrderTags :: PatchSet p wS wX -> [PatchInfo]
inOrderTags (PatchSet _ ts) = go ts
  where go :: RL(Tagged t1) wT wY -> [PatchInfo]
        go (Tagged t _ _ :<: ts') = info t : go ts'
        go NilRL = []

type Marks = M.IntMap BC.ByteString

emptyMarks :: Marks
emptyMarks = M.empty

lastMark :: Marks -> Int
lastMark m = if M.null m then 0 else fst $ M.findMax m

getMark :: Marks -> Int -> Maybe BC.ByteString
getMark marks key = M.lookup key marks

addMark :: Marks -> Int -> BC.ByteString -> Marks
addMark marks key value = M.insert key value marks

readMarks :: FilePath -> IO Marks
readMarks p = do lines' <- BC.split '\n' `fmap` BC.readFile p
                 return $ foldl merge M.empty lines'
               `catchall` return emptyMarks
  where merge set line = case (BC.split ':' line) of
          [i, hash] -> M.insert (read $ BC.unpack i) (BC.dropWhile (== ' ') hash) set
          _ -> set -- ignore, although it is maybe not such a great idea...

writeMarks :: FilePath -> Marks -> IO ()
writeMarks fp m = do removeFile fp `catchall` return () -- unlink
                     BC.writeFile fp marks
  where marks = BC.concat $ map format $ M.assocs m
        format (k, s) = BC.concat [BC.pack $ show k, BC.pack ": ", s, BC.pack "\n"]
