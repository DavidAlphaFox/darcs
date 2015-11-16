-- Copyright (C) 2002-2004 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# LANGUAGE CPP, ScopedTypeVariables #-}


module Darcs.Repository
    ( Repository
    , HashedDir(..)
    , Cache(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , RepoJob(..)
    , maybeIdentifyRepository
    , identifyRepositoryFor
    , withRecorded
    , withRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryDirectory
    , writePatchSet
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , replacePristine
    , readRepo
    , prefsUrl
    , repoPatchType
    , readRepoUsingSpecificInventory
    , addToPending
    , addPendingDiffToPending
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyAddToPending
    , tentativelyReplacePatches
    , readTentativeRepo
    , withManualRebaseUpdate
    , tentativelyMergePatches
    , considerMergeToWorking
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , createRepository
    , cloneRepository
    , patchSetToRepository
    , unrevertUrl
    , applyToWorking
    , patchSetToPatches
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , PatchSet
    , SealedPatchSet
    , PatchInfoAnd
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , checkUnrelatedRepos
    , testTentative
    , modifyCache
    , reportBadSources
    -- * Recorded and unrecorded and pending.
    , readRecorded
    , readUnrecorded
    , unrecordedChanges
    , unrecordedChangesWithPatches
    , filterOutConflicts
    , readPending
    , readRecordedAndPending
    -- * Index.
    , readIndex
    , invalidateIndex
    -- * Used as command arguments
    , listFiles
    , listRegisteredFiles
    , listUnregisteredFiles
    ) where

import Prelude hiding ( catch, pi )

import System.Exit ( exitSuccess )
import Data.List ( (\\), isPrefixOf )
import Data.Maybe( catMaybes, isJust, listToMaybe )

import Darcs.Repository.State
    ( readRecorded
    , readUnrecorded
    , readWorking
    , unrecordedChanges
    , unrecordedChangesWithPatches
    , readPendingAndWorking
    , readPending
    , readIndex
    , invalidateIndex
    , readRecordedAndPending
    , restrictDarcsdir
    , restrictBoring
    , applyTreeFilter
    , filterOutConflicts
    )

import Darcs.Repository.Internal
    (Repository(..)
    , maybeIdentifyRepository
    , identifyRepositoryFor
    , identifyRepository
    , IdentifyRepo(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , readRepo
    , readTentativeRepo
    , readRepoUsingSpecificInventory
    , prefsUrl
    , withRecorded
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyReplacePatches
    , tentativelyAddToPending
    , revertRepositoryChanges
    , finalizeRepositoryChanges
    , unrevertUrl
    , applyToWorking
    , patchSetToPatches
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , makeNewPending
    , seekRepo
    )
import Darcs.Repository.Job
    ( RepoJob(..)
    , withRepoLock
    , withRepoLockCanFail
    , withRepository
    , withRepositoryDirectory
    )
import Darcs.Repository.Rebase ( withManualRebaseUpdate )
import Darcs.Repository.Test
    ( testTentative )

import Darcs.Repository.Merge( tentativelyMergePatches
                             , considerMergeToWorking
                             )
import Darcs.Repository.Cache ( unionRemoteCaches
                              , fetchFileUsingCache
                              , speculateFileUsingCache
                              , HashedDir(..)
                              , Cache(..)
                              , CacheLoc(..)
                              , WritableOrNot(..)
                              , hashedDir
                              , bucketFolder
                              , CacheType(Directory)
                              , reportBadSources
                              )

import Darcs.Patch ( RepoPatch
                   , apply
                   , invert
                   , effect
                   , PrimOf
                   )
import Darcs.Patch.Set ( Origin
                       , PatchSet(..)
                       , SealedPatchSet
                       , newset2RL
                       , newset2FL
                       , progressPatchSet
                       )
import Darcs.Patch.Match ( MatchFlag(..), havePatchsetMatch )
import Darcs.Patch.Commute( commuteFL )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Control.Exception ( catch, Exception, throwIO, finally, IOException )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar
                               , newMVar
                               , putMVar
                               , takeMVar
                               )
import Control.Monad ( unless, when, void )
import Control.Applicative( (<$>) )
import System.Directory ( createDirectory
                        , createDirectoryIfMissing
                        , renameFile
                        , doesFileExist
                        , removeFile
                        , getDirectoryContents
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.IO ( stderr )
import System.IO.Error ( isAlreadyExistsError )
import System.Posix.Files ( createLink )

import qualified Darcs.Repository.HashedRepo as HashedRepo

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, extractHash, hopefully )
import Darcs.Repository.ApplyPatches ( applyPatches, runDefault )
import Darcs.Repository.HashedRepo ( applyToTentativePristine
                                   , pris2inv
                                   , inv2pris
                                   , revertTentativeChanges
                                   , copySources
                                   )
import Darcs.Repository.InternalTypes ( modifyCache )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), FreeLeft, unFreeLeft )
import Darcs.Patch.Witnesses.Ordered
       ((:>)(..), reverseRL, reverseFL, lengthFL, mapFL_FL, FL(..),
        RL(..), bunchFL, mapFL, mapRL, lengthRL, (+>+), (:\/:)(..))
import Darcs.Repository.Format ( RepoProperty ( HashedInventory, Darcs2 )
                               , RepoFormat
                               , createRepoFormat
                               , formatHas
                               , writeRepoFormat
                               , readProblem
                               )
import Darcs.Repository.Prefs ( writeDefaultPrefs, addRepoSource, deleteSources )
import Darcs.Repository.Match ( getOnePatchset )
import Darcs.Patch.Depends ( areUnrelatedRepos, findUncommon, findCommonWithThem
                           , countUsThem )
import Darcs.Patch.Type ( PatchType(..) )

import Darcs.Util.Exception ( catchall )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Repository.External
    ( copyFileOrUrl
    , Cachable(..)
    , fetchFileLazyPS
    , gzFetchFilePS
    )
import Darcs.Util.Progress ( debugMessage
                , tediousSize
                , beginTedious
                , endTedious
                )
import Darcs.Patch.Progress
    ( progressRLShowTags
    , progressFL
    )
import Darcs.Repository.Lock
    ( writeBinFile
    , writeDocBinFile
    , withTemp
    )
import Darcs.Repository.Flags
    ( UpdateWorking(..)
    , UseCache(..)
    , UseIndex(..)
    , ScanKnown(..)
    , RemoteDarcs (..)
    , Reorder (..)
    , Compression (..)
    , CloneKind (..)
    , Verbosity (..)
    , DryRun (..)
    , UMask (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WantGuiPause (..)
    , SetScriptsExecutable (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , DiffAlgorithm (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    , WithPatchIndex (..)
    )

import Darcs.Util.Download ( maxPipelineLength )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.SignalHandler ( catchInterrupt )
import Darcs.Util.Printer ( Doc, text, hPutDocLn, putDocLn, errorDoc, RenderMode(..) )

import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Tree( Tree, emptyTree, expand, list )
import Storage.Hashed.Hash( encodeBase16 )
import Darcs.Util.Path( anchorPath )
import Storage.Hashed.Darcs( writeDarcsHashed, darcsAddMissingHashes )
import Darcs.Util.ByteString( gzReadFilePS )

import System.FilePath( (</>)
                      , takeFileName
                      , splitPath
                      , joinPath
                      , takeDirectory
                      )
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip ( compress, decompress )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Darcs.Repository.PatchIndex (createOrUpdatePatchIndexDisk, doesPatchIndexExist, createPIWithInterrupt)
#include "impossible.h"


-- @createRepository useFormat1 useNoWorkingDir patchIndex@
createRepository :: Bool -> WithWorkingDir -> WithPatchIndex -> IO ()
createRepository useFormat1 withWorkingDir createPatchIndex = do
  createDirectory darcsdir `catch`
      (\e-> if isAlreadyExistsError e
            then fail "Tree has already been initialized!"
            else fail $ "Error creating directory `"++darcsdir++"'.")
  cwd <- getCurrentDirectory
  x <- seekRepo
  when (isJust x) $ do
      setCurrentDirectory cwd
      putStrLn "WARNING: creating a nested repository."
  createDirectory $ darcsdir ++ "/pristine.hashed"
  createDirectory $ darcsdir ++ "/patches"
  createDirectory $ darcsdir ++ "/inventories"
  createDirectory $ darcsdir ++ "/prefs"
  writeDefaultPrefs
  let repoFormat = createRepoFormat useFormat1 withWorkingDir
  writeRepoFormat repoFormat (darcsdir++"/format")
  writeBinFile (darcsdir++"/hashed_inventory") ""
  writePristine "." emptyTree
  withRepository NoUseCache $ RepoJob $ \repo -> case createPatchIndex of
      NoPatchIndex -> return () -- default
      YesPatchIndex -> createOrUpdatePatchIndexDisk repo

repoPatchType :: Repository p wR wU wT -> PatchType p
repoPatchType _ = PatchType

cloneRepository ::
    String    -- origin repository path
    -> String -- new repository name (for relative path)
    -> Verbosity -> UseCache
    -> CloneKind
    -> UMask -> RemoteDarcs
    -> SetScriptsExecutable
    -> RemoteRepos -> SetDefault
    -> [MatchFlag]
    -> RepoFormat
    -> WithWorkingDir
    -> WithPatchIndex   -- use patch index
    -> Bool   -- use packs
    -> Bool   -- --to-match given
    -> ForgetParent
    -> IO ()
cloneRepository repodir mysimplename v uc cloneKind um rdarcs sse remoteRepos
                setDefault matchFlags rfsource withWorkingDir usePatchIndex usePacks toMatch forget = do
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  createRepository (not $ formatHas Darcs2 rfsource)
                   withWorkingDir
                   (if cloneKind == LazyClone then NoPatchIndex else usePatchIndex)
  debugMessage "Finished initializing new directory."
  addRepoSource repodir NoDryRun remoteRepos setDefault

  if toMatch && cloneKind /= LazyClone
    then withRepository uc $ RepoJob $ \repository -> do
      debugMessage "Using economical clone --to-match handling"
      fromrepo <- identifyRepositoryFor repository uc repodir
      Sealed patches_to_get <- getOnePatchset fromrepo matchFlags
      patchSetToRepository fromrepo patches_to_get uc rdarcs
      debugMessage "Finished converting selected patch set to new repository"
    else copyRepoAndGoToChosenVersion repodir v uc cloneKind um rdarcs sse
                                      matchFlags withWorkingDir usePacks forget

-- assumes that the target repo of the get is the current directory,
-- and that an inventory in the right format has already been created.
copyRepoAndGoToChosenVersion ::
               String -- repository directory
               -> Verbosity -> UseCache
               -> CloneKind
               -> UMask -> RemoteDarcs
               -> SetScriptsExecutable
               -> [MatchFlag]
               -> WithWorkingDir -> Bool
               -> ForgetParent
               -> IO ()
copyRepoAndGoToChosenVersion repodir v uc gk um rdarcs sse matchFlags withWorkingDir usePacks forget =
  withRepository uc $ RepoJob $ \repository -> do
     debugMessage "Identifying and copying repository..."
     fromRepo@(Repo fromDir rffrom _ _) <- identifyRepositoryFor repository uc repodir
     case readProblem rffrom of
       Just e ->  fail $ "Incompatibility with repository " ++ fromDir ++ ":\n" ++ e
       Nothing -> return ()
     debugMessage "Copying prefs"
     copyFileOrUrl rdarcs (fromDir ++ "/" ++ darcsdir ++ "/prefs/prefs")
       (darcsdir ++ "/prefs/prefs") (MaxAge 600) `catchall` return ()
     if formatHas HashedInventory rffrom
      then do
        -- copying basic repository (hashed_inventory and pristine)
        if usePacks && (not . isValidLocalPath) fromDir
          then copyBasicRepoPacked    fromRepo v uc um rdarcs withWorkingDir
          else copyBasicRepoNotPacked fromRepo v uc um rdarcs withWorkingDir
        when (gk /= LazyClone) $ do
          when (gk /= CompleteClone) $
            putInfo v $ text "Copying patches, to get lazy repository hit ctrl-C..."
        -- copying complete repository (inventories and patches)
          if usePacks && (not . isValidLocalPath) fromDir
            then copyCompleteRepoPacked    fromRepo v uc um gk
            else copyCompleteRepoNotPacked fromRepo v uc um gk
      else
        -- old-fashioned repositories are cloned diferently since
        -- we need to copy all patches first and then build pristine
        copyRepoOldFashioned fromRepo v uc um withWorkingDir
     when (sse == YesSetScriptsExecutable) setScriptsExecutable
     when (havePatchsetMatch matchFlags) $ do
      putStrLn "Going to specified version..."
      -- read again repository on disk to get caches and sources right
      withRepoLock NoDryRun uc YesUpdateWorking um $ RepoJob $ \repository' -> do
        patches <- readRepo repository'
        Sealed context <- getOnePatchset repository' matchFlags
        when (snd (countUsThem patches context) > 0) $
             errorDoc $ text "Missing patches from context!" -- FIXME : - (
        _ :> us' <- return $ findCommonWithThem patches context
        let ps = mapFL_FL hopefully us'
        putInfo v $ text $ "Unapplying " ++ show (lengthFL ps) ++ " " ++
                    englishNum (lengthFL ps) (Noun "patch") ""
        invalidateIndex repository'
        _ <- tentativelyRemovePatches repository' GzipCompression YesUpdateWorking us'
        tentativelyAddToPending repository' YesUpdateWorking $ invert $ effect us'
        finalizeRepositoryChanges repository' YesUpdateWorking GzipCompression
        runDefault (apply (invert $ effect ps)) `catch` \(e :: IOException) ->
            fail ("Couldn't undo patch in working dir.\n" ++ show e)
        when (sse == YesSetScriptsExecutable) $ setScriptsExecutablePatches (invert $ effect ps)
     when (forget == YesForgetParent) deleteSources

putInfo :: Verbosity -> Doc -> IO ()
putInfo Quiet _ = return ()
putInfo _ d = hPutDocLn Encode stderr d

putVerbose :: Verbosity -> Doc -> IO ()
putVerbose Verbose d = putDocLn d
putVerbose _ _ = return ()

copyBasicRepoNotPacked  :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> Verbosity -> UseCache
                        -> UMask -> RemoteDarcs
                        -> WithWorkingDir
                        -> IO ()
copyBasicRepoNotPacked (Repo fromDir _ _ fromCache) verb useCache umask rdarcs withWorkingDir = do
  toRepo@(Repo toDir toFormat toPristine toCache) <- identifyRepository useCache "."
  let (_dummy :: Repository p wR wU wT) = toRepo --The witnesses are wrong, but cannot escape
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo2 :: Repository p wR wU wT
      toRepo2 = Repo toDir toFormat toPristine toCache2
  HashedRepo.copyHashedInventory toRepo2 rdarcs fromDir
  HashedRepo.copySources toRepo2 fromDir
  debugMessage "Grabbing lock in new repository to copy basic repo..."
  withRepoLock NoDryRun useCache YesUpdateWorking umask
   $ RepoJob $ \torepository -> do
      putVerbose verb $ text "Writing pristine and working directory contents..."
      createPristineDirectoryTree torepository "." withWorkingDir


copyCompleteRepoNotPacked :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> Verbosity -> UseCache
                        -> UMask -> CloneKind
                        -> IO ()
copyCompleteRepoNotPacked _ verb useCache umask cloneKind = do
  debugMessage "Grabbing lock in new repository to copy complete repo..."
  withRepoLock NoDryRun useCache YesUpdateWorking umask
   $ RepoJob $ \torepository@(Repo todir _ _ _) -> do
       let cleanup = putInfo verb $ text "Using lazy repository."
       allowCtrlC cloneKind cleanup $ do
         fetchPatchesIfNecessary torepository
         pi <- doesPatchIndexExist todir
         when pi $ createPIWithInterrupt torepository

packsDir :: String
packsDir = "/" ++ darcsdir ++ "/packs/"

copyBasicRepoPacked ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> UMask -> RemoteDarcs
  -> WithWorkingDir
  -> IO ()
copyBasicRepoPacked r@(Repo fromDir _ _ _) verb useCache umask rdarcs withWorkingDir =
  do let hashURL = fromDir ++ packsDir ++ "pristine"
     mPackHash <- (Just <$> gzFetchFilePS hashURL Uncachable) `catchall` (return Nothing)
     let hiURL = fromDir ++ "/" ++ darcsdir ++ "/hashed_inventory"
     i <- gzFetchFilePS hiURL Uncachable
     let currentHash = BS.pack $ inv2pris i
     let copyNormally = copyBasicRepoNotPacked r verb useCache umask rdarcs withWorkingDir
     case mPackHash of
      Just packHash | packHash == currentHash
              -> ( copyBasicRepoPacked2 r verb useCache withWorkingDir
                    `catchall` do putStrLn "Problem while copying basic pack, copying normally."
                                  copyNormally)
      _       -> do putVerbose verb $ text "Remote repo has no basic pack or outdated basic pack, copying normally."
                    copyNormally

copyBasicRepoPacked2 ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> WithWorkingDir -> IO ()
copyBasicRepoPacked2 fromRepo@(Repo fromDir _ _ fromCache) verb useCache withWorkingDir = do
  b <- fetchFileLazyPS (fromDir ++ packsDir ++ "basic.tar.gz") Uncachable
  putVerbose verb $ text "Cloning packed basic repository."
  Repo toDir toFormat toPristine toCache <-
    identifyRepositoryFor fromRepo useCache "."
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo :: Repository p wR wU wR -- In empty repo, t(entative) = r(ecorded)
      toRepo = Repo toDir toFormat toPristine toCache2
  copySources toRepo fromDir
  Repo _ _ _ toCache3 <-
    identifyRepositoryFor toRepo useCache "."
  -- unpack inventory & pristine cache
  cleanDir "pristine.hashed"
  removeFile $ darcsdir </> "hashed_inventory"
  unpackBasic toCache3 . Tar.read $ decompress b
  createPristineDirectoryTree toRepo "." withWorkingDir
  putVerbose verb $ text "Basic repository unpacked. Will now see if there are new patches."
  -- pull new patches
  us <- readRepo toRepo
  them <- readRepo fromRepo
  us' :\/: them' <- return $ findUncommon us them
  revertTentativeChanges
  Sealed pw <- tentativelyMergePatches toRepo "clone" NoAllowConflicts YesUpdateWorking NoExternalMerge NoWantGuiPause GzipCompression verb NoReorder ( UseIndex, ScanKnown, MyersDiff ) us' them'
  invalidateIndex toRepo
  finalizeRepositoryChanges toRepo YesUpdateWorking GzipCompression
  when (withWorkingDir == WithWorkingDir) $ void $ applyToWorking toRepo verb pw
 where
  cleanDir d = mapM_ (\x -> removeFile $ darcsdir </> d </> x) .
    filter (\x -> head x /= '.') =<< getDirectoryContents (darcsdir </> d)

copyCompleteRepoPacked ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> UMask
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked r verb useCache umask cloneKind =
  ( copyCompleteRepoPacked2 r verb useCache cloneKind
  `catchall` do putVerbose verb $ text "Problem while copying patches pack, copying normally."
                copyCompleteRepoNotPacked r verb useCache umask cloneKind )

copyCompleteRepoPacked2 ::
  forall p wR wU wT. (RepoPatch p, ApplyState (PrimOf p) ~ Tree, ApplyState p ~ Tree)
  => Repository p wR wU wT
  -> Verbosity -> UseCache
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked2 fromRepo@(Repo fromDir _ _ fromCache) verb useCache cloneKind = do
  Repo toDir toFormat toPristine toCache <- identifyRepositoryFor fromRepo useCache "."
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo :: Repository p wR wU wR -- In empty repo, t(entative) = r(ecorded)
      toRepo = Repo toDir toFormat toPristine toCache2
  Repo _ _ _ toCache3 <- identifyRepositoryFor toRepo useCache "."
  us <- readRepo toRepo
  -- get old patches
  let cleanup = putInfo verb $ text "Using lazy repository."
  allowCtrlC cloneKind cleanup $ do
    cleanDir "patches"
    putVerbose verb $ text "Using patches pack."
    unpackPatches toCache3 (mapRL hashedPatchFileName $ newset2RL us) .
      Tar.read . decompress =<< fetchFileLazyPS (fromDir ++ packsDir ++ "patches.tar.gz") Uncachable
    pi <- doesPatchIndexExist toDir
    when pi $ createPIWithInterrupt toRepo
 where
  cleanDir d = mapM_ (\x -> removeFile $ darcsdir </> d </> x) .
    filter (\x -> head x /= '.') =<< getDirectoryContents (darcsdir </> d)

allowCtrlC :: CloneKind -> IO () -> IO () -> IO ()
allowCtrlC CompleteClone _       action = action
allowCtrlC _             cleanup action = action `catchInterrupt` cleanup

copyRepoOldFashioned :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> Verbosity -> UseCache
                        -> UMask
                        -> WithWorkingDir
                        -> IO ()
copyRepoOldFashioned fromrepository verb useCache umask withWorkingDir = do
  toRepo@(Repo _ _ _ toCache) <- identifyRepository useCache "."
  let (_dummy :: Repository p wR wU wT) = toRepo --The witnesses are wrong, but cannot escape
  -- copy all patches from remote
  HashedRepo.revertTentativeChanges
  patches <- readRepo fromrepository
  let k = "Copying patch"
  beginTedious k
  tediousSize k (lengthRL $ newset2RL patches)
  let patches' = progressPatchSet k patches
  HashedRepo.writeTentativeInventory toCache GzipCompression patches'
  endTedious k
  HashedRepo.finalizeTentativeChanges toRepo GzipCompression
  -- apply all patches into current hashed repository
  debugMessage "Grabbing lock in new repository..."
  withRepoLock NoDryRun useCache YesUpdateWorking umask
   $ RepoJob $ \torepository -> do
      local_patches <- readRepo torepository
      replacePristine torepository emptyTree
      let patchesToApply = progressFL "Applying patch" $ newset2FL local_patches
      sequence_ $ mapFL applyToTentativePristine $ bunchFL 100 patchesToApply
      finalizeRepositoryChanges torepository YesUpdateWorking GzipCompression
      putVerbose verb $ text "Writing pristine and working directory contents..."
      createPristineDirectoryTree torepository "." withWorkingDir

withControlMVar :: (MVar () -> IO ()) -> IO ()
withControlMVar f = do
  mv <- newMVar ()
  f mv
  takeMVar mv

forkWithControlMVar :: MVar () -> IO () -> IO ()
forkWithControlMVar mv f = do
  takeMVar mv
  _ <- forkIO $ finally f (putMVar mv ())
  return ()

removeMetaFiles :: IO ()
removeMetaFiles = mapM_ (removeFile . (darcsdir </>)) .
  filter ("meta-" `isPrefixOf`) =<< getDirectoryContents darcsdir

unpackBasic :: Exception e => Cache -> Tar.Entries e -> IO ()
unpackBasic c x = do
  withControlMVar $ \mv -> unpackTar c (basicMetaHandler c mv) x
  removeMetaFiles

unpackPatches :: Exception e => Cache -> [String] -> Tar.Entries e -> IO ()
unpackPatches c ps x = do
  withControlMVar $ \mv -> unpackTar c (patchesMetaHandler c ps mv) x
  removeMetaFiles

unpackTar :: Exception e => Cache -> IO () -> Tar.Entries e -> IO ()
unpackTar  _ _ Tar.Done = return ()
unpackTar  _ _ (Tar.Fail e)= throwIO e
unpackTar c mh (Tar.Next x xs) = case Tar.entryContent x of
  Tar.NormalFile x' _ -> do
    let p = Tar.entryPath x
    if "meta-" `isPrefixOf` takeFileName p
      then do
        BL.writeFile p x'
        mh
        unpackTar c mh xs
      else do
        ex <- doesFileExist p
        if ex
          then debugMessage $ "Tar thread: STOP " ++ p
          else do
            if p == darcsdir </> "hashed_inventory"
              then writeFile' Nothing p x'
              else writeFile' (cacheDir c) p $ compress x'
            debugMessage $ "Tar thread: GET " ++ p
            unpackTar c mh xs
  _ -> fail "Unexpected non-file tar entry"
 where
  writeFile' Nothing path content = withTemp $ \tmp -> do
    BL.writeFile tmp content
    renameFile tmp path
  writeFile' (Just ca) path content = do
    let fileFullPath = case splitPath path of
          _:hDir:hFile:_  -> joinPath [ca, hDir, bucketFolder hFile, hFile]
          _               -> fail "Unexpected file path"
    createDirectoryIfMissing True $ takeDirectory path
    createLink fileFullPath path `catch` (\(ex :: IOException) -> do
      if isAlreadyExistsError ex then
        return () -- so much the better
      else
        -- ignore cache if we cannot link
        writeFile' Nothing path content)

basicMetaHandler :: Cache -> MVar () -> IO ()
basicMetaHandler ca mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-pristine"
  when ex . forkWithControlMVar mv $
    fetchFilesUsingCache ca HashedPristineDir . lines =<<
      readFile (darcsdir </> "meta-filelist-pristine")

patchesMetaHandler :: Cache -> [String] -> MVar () -> IO ()
patchesMetaHandler ca ps mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-inventories"
  when ex $ do
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPristineDir .
      lines =<< readFile (darcsdir </> "meta-filelist-inventories")
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPatchesDir ps

cacheDir :: Cache -> Maybe String
cacheDir (Ca cs) = listToMaybe . catMaybes .flip map cs $ \x -> case x of
  Cache Directory Writable x' -> Just x'
  _ -> Nothing

hashedPatchFileName :: PatchInfoAnd p wA wB -> String
hashedPatchFileName x = case extractHash x of
  Left _ -> fail "unexpected unhashed patch"
  Right h -> h

-- | fetchFilesUsingCache is similar to mapM fetchFileUsingCache, exepts
-- it stops execution if file it's going to fetch already exists.
fetchFilesUsingCache :: Cache -> HashedDir -> [FilePath] -> IO ()
fetchFilesUsingCache _ _ [] = return ()
fetchFilesUsingCache c d (f:fs) = do
  ex <- doesFileExist $ darcsdir </> hashedDir d </> f
  if ex
    then debugMessage $ "Cache thread: STOP " ++
      (darcsdir </> hashedDir d </> f)
    else do
      debugMessage $ "Cache thread: GET " ++
        (darcsdir </> hashedDir d </> f)
      _ <- fetchFileUsingCache c d f
      fetchFilesUsingCache c d fs

-- | writePatchSet is like patchSetToRepository, except that it doesn't
-- touch the working directory or pristine cache.
writePatchSet :: (RepoPatch p, ApplyState p ~ Tree)
              => PatchSet p Origin wX
              -> UseCache
              -> IO (Repository p wR wU wT)
writePatchSet patchset useCache = do
    maybeRepo <- maybeIdentifyRepository useCache "."
    let repo@(Repo _ _ _ c) =
          case maybeRepo of
            GoodRepository r -> r
            BadRepository e -> bug ("Current directory is a bad repository in writePatchSet: " ++ e)
            NonRepository e -> bug ("Current directory not a repository in writePatchSet: " ++ e)
    debugMessage "Writing inventory"
    HashedRepo.writeTentativeInventory c GzipCompression patchset
    HashedRepo.finalizeTentativeChanges repo GzipCompression
    return repo

-- | patchSetToRepository takes a patch set, and writes a new repository
--   in the current directory that contains all the patches in the patch
--   set.  This function is used when 'darcs get'ing a repository with
--   the --to-match flag.
patchSetToRepository :: (RepoPatch p, ApplyState p ~ Tree)
                     => Repository p wR1 wU1 wR1
                     -> PatchSet p Origin wX
                     -> UseCache -> RemoteDarcs
                     -> IO ()
patchSetToRepository (Repo fromrepo rf _ _) patchset useCache remoteDarcs = do
    when (formatHas HashedInventory rf) $ -- set up sources and all that
       do writeFile (darcsdir </> "tentative_pristine") "" -- this is hokey
          repox <- writePatchSet patchset useCache
          HashedRepo.copyHashedInventory repox remoteDarcs fromrepo
          HashedRepo.copySources repox fromrepo
    repo <- writePatchSet patchset useCache
    readRepo repo >>= (runDefault . applyPatches . newset2FL)
    debugMessage "Writing the pristine"
    pristineFromWorking repo

checkUnrelatedRepos :: RepoPatch p
                    => Bool
                    -> PatchSet p wStart wX
                    -> PatchSet p wStart wY
                    -> IO ()
checkUnrelatedRepos allowUnrelatedRepos us them =
    when ( not allowUnrelatedRepos && areUnrelatedRepos us them ) $
         do confirmed <- promptYorn "Repositories seem to be unrelated. Proceed?"
            unless confirmed $ do putStrLn "Cancelled."
                                  exitSuccess

-- | This function fetches all patches that the given repository has
--   with fetchFileUsingCache, unless --lazy is passed.
fetchPatchesIfNecessary :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                        => Repository p wR wU wT
                        -> IO ()
fetchPatchesIfNecessary torepository@(Repo _ _ _ c) =
  do  r <- readRepo torepository
      pipelineLength <- maxPipelineLength
      let patches = newset2RL r
          ppatches = progressRLShowTags "Copying patches" patches
          (first, other) = splitAt (pipelineLength - 1) $ tail $ hashes patches
          speculate | pipelineLength > 1 = [] : first : map (:[]) other
                    | otherwise = []
      mapM_ fetchAndSpeculate $ zip (hashes ppatches) (speculate ++ repeat [])
  where hashes :: forall wX wY . RL (PatchInfoAnd p) wX wY -> [String]
        hashes = catMaybes . mapRL (either (const Nothing) Just . extractHash)
        fetchAndSpeculate :: (String, [String]) -> IO ()
        fetchAndSpeculate (f, ss) = do
          _ <- fetchFileUsingCache c HashedPatchesDir f
          mapM_ (speculateFileUsingCache c HashedPatchesDir) ss

-- | Add an FL of patches started from the pending state to the pending patch.
-- TODO: add witnesses for pending so we can make the types precise: currently
-- the passed patch can be applied in any context, not just after pending.
addPendingDiffToPending :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT -> UpdateWorking
                          -> FreeLeft (FL (PrimOf p)) -> IO ()
addPendingDiffToPending _ NoUpdateWorking  _ = return ()
addPendingDiffToPending repo@(Repo{}) uw@YesUpdateWorking newP = do
    (toPend :> _) <-
        readPendingAndWorking (UseIndex, ScanKnown, MyersDiff) repo Nothing
    invalidateIndex repo
    case unFreeLeft newP of
        (Sealed p) -> makeNewPending repo uw $ toPend +>+ p

-- | Add a FL of patches starting from the working state to the pending patch,
-- including as much extra context as is necessary (context meaning
-- dependencies), by commuting the patches to be added past as much of the
-- changes between pending and working as is possible, and including anything
-- that doesn't commute, and the patch itself in the new pending patch.
addToPending :: (RepoPatch p, ApplyState p ~ Tree)
             => Repository p wR wU wT -> UpdateWorking -> FL (PrimOf p) wU wY -> IO ()
addToPending _ NoUpdateWorking  _ = return ()
addToPending repo@(Repo{}) uw@YesUpdateWorking p = do
   (toPend :> toUnrec) <- readPendingAndWorking (UseIndex, ScanKnown, MyersDiff) repo Nothing
   invalidateIndex repo
   case genCommuteWhatWeCanRL commuteFL (reverseFL toUnrec :> p) of
       (toP' :> p'  :> _excessUnrec) ->
           makeNewPending repo uw $ toPend +>+ reverseRL toP' +>+ p'

-- | Replace the existing pristine with a new one (loaded up in a Tree object).
replacePristine :: Repository p wR wU wT -> Tree IO -> IO ()
replacePristine (Repo r _ _ _) = writePristine r

writePristine :: FilePath -> Tree IO -> IO ()
writePristine r tree = withCurrentDirectory r $
    do let t = darcsdir </> "hashed_inventory"
       i <- gzReadFilePS t
       tree' <- darcsAddMissingHashes tree
       root <- writeDarcsHashed tree' $ darcsdir </> "pristine.hashed"
       writeDocBinFile t $ pris2inv (BS.unpack $ encodeBase16 root) i

pristineFromWorking :: RepoPatch p => Repository p wR wU wT -> IO ()
pristineFromWorking repo@(Repo dir _ _ _) =
  withCurrentDirectory dir $ readWorking >>= replacePristine repo

-- | Get a list of all files and directories in the working copy, including
-- boring files if necessary
listFiles :: Bool -> IO [String]
listFiles takeBoring =
  do
    nonboring <- considered emptyTree
    working <- expand =<< applyTreeFilter nonboring <$> readPlainTree "."
    return $ map (anchorPath "" . fst) $ list working
  where
    considered = if takeBoring
                 then const (return restrictDarcsdir)
                 else restrictBoring

-- | 'listUnregisteredFiles' returns the list of all non-boring unregistered
-- files in the repository.
listUnregisteredFiles :: Bool -> IO [String]
listUnregisteredFiles includeBoring =
    do unregd <- listFiles includeBoring
       regd <- listRegisteredFiles
       return $ unregd \\ regd -- (inefficient)

-- | 'listRegisteredFiles' returns the list of all registered files in the repository.
listRegisteredFiles :: IO [String]
listRegisteredFiles =
    do recorded <- expand =<< withRepository YesUseCache (RepoJob readRecordedAndPending)
       return $ map (anchorPath "" . fst) $ list recorded
