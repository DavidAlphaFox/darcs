{-# LANGUAGE CPP, NamedFieldPuns #-}

-- Copyright (C) 2009-2010 Benedikt Schmidt
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

module Darcs.Repository.PatchIndex (
  doesPatchIndexExist,
  isPatchIndexDisabled,
  isPatchIndexInSync,
  canUsePatchIndex,
  canCreatePI,
  createPIWithInterrupt,
  createOrUpdatePatchIndexDisk,
  deletePatchIndex,
  dumpPatchIndex,
  dumpPatchIndexFiles,
  filterPatches,
  PatchFilter,
  maybeFilterPatches,
  getRelevantSubsequence,
  piTest,
  attemptCreatePatchIndex
) where

import Prelude hiding ( catch, pi )
import Data.Binary ( encodeFile, decodeFile )
import Data.Word ( Word32 )
import Data.Int ( Int8 )
import Data.List ( group, mapAccumL, sort, isPrefixOf, nub, (\\) )
import Data.Maybe ( fromMaybe, isJust )
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception ( catch )
import Control.Monad ( forM_, unless, when )
import Control.Monad.State.Strict ( evalState, execState, State, gets, modify )
import Control.Applicative ( (<$>) )
import System.Directory ( createDirectory, renameDirectory, doesFileExist, doesDirectoryExist )
import Darcs.Repository.Format ( formatHas, RepoProperty( HashedInventory ) )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository.Read ( readRepo )
import Darcs.Patch.Witnesses.Ordered ( mapFL, RL(..), FL(..), reverseRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), Sealed(..), seal, seal2, unsafeUnseal )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd(..), info )
import Darcs.Repository.Lock ( withPermDir, rmRecursive )
import Darcs.Patch ( RepoPatch, listTouchedFiles )
import Darcs.Util.Path ( FileName, fp2fn, fn2fp, toFilePath )
import Darcs.Patch.Apply ( applyToFileMods, ApplyState(..) )
import Darcs.Patch.Set ( newset2FL, Origin, newset2FL )
import Darcs.Patch.Patchy ( Commute )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Patch.Index.Types
import System.FilePath( (</>) )
import System.IO (openFile, IOMode(WriteMode), hClose)
import qualified Data.ByteString as B
import Darcs.Util.Crypt.SHA256 (sha256sum )
import Darcs.Util.Crypt.SHA1 ( SHA1(..), showAsHex )
import Storage.Hashed.Tree ( Tree(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Util.SignalHandler ( catchInterrupt )
#include "impossible.h"

{- -----------------------------------------------------------------------------
   The patch index stores additional information that is extracted from
   the PatchSet for the repository to speed up certain commands.

  createPatchIndexDisk:
     Create the on-disk patch-index index from scratch.
   updatePatchIndexDisk:
     Update the on-disk patch-index index.
  ----------------------------------------------------------------------------- -}

-- ---------------------------------------------------------------------
-- Data structures for the patch-index

data FileIdSpan = FidSpan
                    !FileId           -- the fileid has some fixed name in the
                    !PatchId          -- span starting here
                    !(Maybe PatchId)  -- and (maybe) ending here
  deriving (Show,Eq,Ord)

data FilePathSpan = FpSpan
                      !FileName         -- the file path has some fixed fileid in the
                      !PatchId          -- span starting here
                      !(Maybe PatchId)  -- and (maybe) ending here
  deriving (Show,Eq,Ord)

-- | info about a given fileid, e.g.. is a file or a directory
data FileInfo = FileInfo { isFile::Bool,
                           touching::Set Word32} -- first word of patch hash
  deriving (Show,Eq,Ord)

-- | timespans where a certain filename corresponds to a file with a given id
type FileIdSpans = Map FileName [FileIdSpan]

-- | timespans where a file with a certain id corresponds to given filenames
type FilePathSpans = Map FileId [FilePathSpan]

-- | information file with a given ID
type InfoMap = Map FileId FileInfo

-- | the patch-index
data PatchIndex =
    PatchIndex {
        -- |all the PatchIds tracked by this patch index, with the most
        -- recent patch at the head of the list (note, stored in the
        -- reverse order to this on disk for backwards compatibility
        -- with an older format).
        pids::[PatchId],
        fidspans::FileIdSpans,
        fpspans::FilePathSpans,
        infom::InfoMap
    }

-- | an empty patch-index
emptyPatchIndex :: PatchIndex
emptyPatchIndex = PatchIndex [] M.empty M.empty M.empty

-- | On-disk version of patch index
--   version 1 is the one introduced in darcs 2.10
--           2 changes the pids order to newer-to-older
version :: Int8
version = 2

-- ---------------------------------------------------------------------
-- Query the patch-index

getInventoryHash :: FilePath -> IO String
getInventoryHash repodir = do
  inv <- B.readFile (repodir </> darcsdir </> "hashed_inventory")
  return $ sha256sum inv

-- ---------------------------------------------------------------------
-- create patch-index

-- | 'applyPatchMods pmods pindex' applies a list of PatchMods to the given
--   patch index pindex
applyPatchMods :: [(PatchId, [PatchMod FileName])] -> PatchIndex -> PatchIndex
applyPatchMods pmods pindex =
  flip execState pindex $ mapM_ goList pmods
 where goList :: (PatchId, [PatchMod FileName]) -> PIM ()
       -- nubSeq handles invalid patch in darcs repo:
       --   move with identical target name "rename darcs_patcher to darcs-patcher."
       goList (pid, mods) = do
           modify (\pind -> pind{pids = pid:pids pind})
           mapM_ (curry go pid) (nubSeq mods)

       go :: (PatchId, PatchMod FileName) -> PIM ()
       go (pid, PCreateFile fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid True
         insertTouch fid pid
       go (pid, PCreateDir fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid False
         insertTouch fid pid
       go (pid, PTouch fn) = do
         fid <- lookupFid fn
         insertTouch fid pid
       go (pid, PRename oldfn newfn) = do
         fid <- lookupFid oldfn
         stopFpSpan fid pid
         startFpSpan fid newfn pid
         insertTouch fid pid
         stopFidSpan oldfn pid
         startFidSpan newfn pid fid
       go (pid, PRemove fn) = do
         fid <- lookupFid fn
         insertTouch fid pid
         stopFidSpan fn pid
         stopFpSpan fid pid
       go (_, PInvalid _) = return () -- just ignore invalid changes
       go (pid, PDuplicateTouch fn) = do
         fidm <- gets fidspans
         case M.lookup fn fidm of
           Just (FidSpan fid _ _:_) -> insertTouch fid pid
           Nothing -> return ()
           Just [] -> error $ "applyPatchMods: impossible, no entry for "++show fn
                              ++" in FileIdSpans in duplicate, empty list"

-- ---------------------------------------------------------------------
-- Update and query patch index

type PIM a = State PatchIndex a

-- | create new filespan for created file
createFidStartSpan :: FileName -> PatchId -> PIM FileId
createFidStartSpan fn pstart = do
  fidspans <- gets fidspans
  case M.lookup fn fidspans of
    Nothing -> do
      let fid = FileId fn 1
      modify (\pind -> pind {fidspans=M.insert fn [FidSpan fid pstart Nothing] fidspans})
      return fid
    Just fspans -> do
      let fid = FileId fn (length fspans+1)
      modify (\pind -> pind {fidspans=M.insert fn (FidSpan fid pstart Nothing:fspans) fidspans})
      return fid

-- | start new span for name fn for file fid starting with patch pid
startFpSpan :: FileId -> FileName -> PatchId -> PIM ()
startFpSpan fid fn pstart = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = Just [FpSpan fn pstart Nothing]
        alt (Just spans) = Just (FpSpan fn pstart Nothing:spans)

-- | stop current span for file name fn
stopFpSpan :: FileId -> PatchId -> PIM ()
stopFpSpan fid pend = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fid
        alt (Just []) = error $ "impossible: no span for " ++ show fid++", empty list"
        alt (Just (FpSpan fp pstart Nothing:spans)) =
          Just (FpSpan fp pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fid

-- | start new span for name fn for file fid starting with patch pid
startFidSpan :: FileName -> PatchId -> FileId -> PIM ()
startFidSpan fn pstart fid = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = Just [FidSpan fid pstart Nothing]
        alt (Just spans) = Just (FidSpan fid pstart Nothing:spans)

-- | stop current span for file name fn
stopFidSpan :: FileName -> PatchId -> PIM ()
stopFidSpan fn pend = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fn
        alt (Just []) = error $ "impossible: no span for " ++ show fn++", empty list"
        alt (Just (FidSpan fid pstart Nothing:spans)) =
          Just (FidSpan fid pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fn

-- | insert touching patchid for given file id
createInfo :: FileId -> Bool -> PIM ()
createInfo fid isF = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing = Just (FileInfo isF S.empty)
        alt (Just _) = Just (FileInfo isF S.empty) -- forget old false positives

-- | insert touching patchid for given file id
insertTouch :: FileId -> PatchId -> PIM ()
insertTouch fid pid = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing =  impossible "Fileid does not exist"
        alt (Just (FileInfo isF pids)) = Just (FileInfo isF (S.insert (short pid) pids))

-- | lookup current fid of filepath
lookupFid :: FileName -> PIM FileId
lookupFid fn = do
    maybeFid <- lookupFid' fn
    case maybeFid of
        Nothing -> bug $ "couldn't find " ++ fn2fp fn ++ " in patch index"
        Just fid -> return fid

-- | lookup current fid of filepatch, returning a Maybe to allow failure
lookupFid' :: FileName -> PIM (Maybe FileId)
lookupFid' fn = do
   fidm <- gets fidspans
   case M.lookup fn fidm of
    Just (FidSpan fid _ _:_) -> return $ Just fid
    _ -> return Nothing


-- | lookup all the file ids of a given path
lookupFidf' :: FileName -> PIM [FileId]
lookupFidf' fn = do
   fidm <- gets fidspans
   case M.lookup fn fidm of
      Just spans -> return $ map (\(FidSpan fid _ _) -> fid) spans
      Nothing ->
         error $ "lookupFidf': no entry for " ++ show fn ++ " in FileIdSpans"

-- |  return all fids of matching subpaths
--    of the given filepath
lookupFids :: FileName -> PIM [FileId]
lookupFids fn = do
   fid_spans <- gets fidspans
   file_idss <- mapM (lookupFidf' . fp2fn) $ filter (isPrefixOf (fn2fp fn)) (fpSpans2filePaths' fid_spans)
   return $ nub $ concat file_idss

-- | returns a single file id if the given path is a file
--   if it is a directory, if returns all the file ids of all paths inside it,
--   at any point in repository history
lookupFids' :: FileName -> PIM [FileId]
lookupFids' fn = do
  info_map <- gets infom
  fps_spans <- gets fpspans
  a <- lookupFid' fn
  if isJust a then do
                let fid = fromJust a
                case M.lookup fid info_map of
                  Just (FileInfo True _) -> return [fid]
                  Just (FileInfo False _) ->
                    let file_names = map (\(FpSpan x _ _) -> x) (fps_spans M.! fid)
                    in nub . concat <$> mapM lookupFids file_names
                  Nothing -> error "lookupFids' : could not find file"
              else return []

-- | remove sequential duplicates
nubSeq :: Eq a => [a] -> [a]
nubSeq = map head . group

-- ---------------------------------------------------------------------
-- Create/Update patch-index on disk

-- | create patch index that corresponds to all patches in repo
createPatchIndexDisk :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
createPatchIndexDisk repository = do
  rawpatches <- newset2FL `fmap` readRepo repository
  let patches = mapFL Sealed2 rawpatches
  createPatchIndexFrom repository $ patches2patchMods patches S.empty

-- | convert patches to patchmods
patches2patchMods :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
                  => [Sealed2 (PatchInfoAnd p)] -> Set FileName -> [(PatchId, [PatchMod FileName])]
patches2patchMods patches fns = snd $ mapAccumL go fns patches
  where
    go filenames (Sealed2 p) = (filenames', (pid, pmods_effect ++ pmods_dup))
      where pid = makePatchID . info $ p
            (filenames', pmods_effect) = applyToFileMods p filenames
            -- applyToFileMods only returns patchmods that actually modify a file,
            -- i.e., never duplicate patches
            touched pm = case pm of {PTouch f -> [f]; PRename a b -> [a,b];
                                     PCreateDir f -> [f]; PCreateFile f -> [f];
                                     PRemove f -> [f]; _ -> []}
            touched_all = map fp2fn $ listTouchedFiles p
            touched_effect = concatMap touched pmods_effect
            touched_invalid = [ f | (PInvalid f) <- pmods_effect]
            -- listTouchedFiles returns all files that touched by these
            --  patches, even if they have no effect, e.g. by duplicate patches
            pmods_dup = map PDuplicateTouch . S.elems
                            $ S.difference (S.fromList touched_all)
                                           (S.fromList touched_invalid
                                            `S.union`
                                            S.fromList touched_effect)

-- | return set of current filenames in patch index
fpSpans2fileNames :: FilePathSpans -> Set FileName
fpSpans2fileNames fpSpans =
  S.fromList [fn | (FpSpan fn _ Nothing:_)<- M.elems fpSpans]

-- | remove all patch effects of given patches from patch index.
--   assumes that the given list of patches is a suffix of the
--   patches tracked by the patch-index
removePidSuffix :: Map PatchId Int -> [PatchId] -> PatchIndex -> PatchIndex
removePidSuffix _ [] pindex = pindex
removePidSuffix pid2idx oldpids@(oldpid:_) (PatchIndex pids fidspans fpspans infom) =
    PatchIndex (pids \\ oldpids)
               (M.mapMaybe removefid fidspans)
               (M.mapMaybe removefp fpspans)
               infom -- leave hashes in infom, false positives are harmless
  where
    findIdx pid = fromMaybe (impossible "removePidSuffix") (M.lookup pid pid2idx)
    oldidx = findIdx oldpid
    from `after` idx = findIdx from > idx
    mto `afterM` idx | Just to <- mto, findIdx to > idx = True
                     | otherwise = False
    removefid fidsps = if null fidsps' then Nothing else Just fidsps'
      where
        fidsps' = concatMap go fidsps
        go (FidSpan fid from mto)
          | from `after` oldidx && mto `afterM` oldidx = [FidSpan fid from mto]
          | from `after` oldidx = [FidSpan fid from Nothing]
          | otherwise = []
    removefp fpsps = if null fpsps' then Nothing else Just fpsps'
      where
        fpsps' = concatMap go fpsps
        go (FpSpan fn from mto)
          | from `after` oldidx && mto `afterM` oldidx = [FpSpan fn from mto]
          | from `after` oldidx = [FpSpan fn from Nothing]
          | otherwise = []

-- | update the patch index to the current state of the repository
updatePatchIndexDisk :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
updatePatchIndexDisk repo@(Repo repodir _ _ _) = do
    (_,_,pid2idx,pindex) <- loadPatchIndex repodir
    -- check that patch index is up to date
    patches <- newset2FL `fmap` readRepo repo
    let pidsrepo = mapFL (makePatchID . info) patches
        (oldpids,_,len_common) = uncommon (reverse $ pids pindex) pidsrepo
        pindex' = removePidSuffix pid2idx oldpids pindex
        filenames = fpSpans2fileNames (fpspans pindex')
        cdir = repodir </> indexDir
    -- reread to prevent holding onto patches for too long
    rawpatches <- newset2FL `fmap` readRepo repo
    let newpatches = drop len_common $ mapFL seal2 rawpatches
        newpmods = patches2patchMods newpatches filenames
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir inv_hash (applyPatchMods newpmods pindex')
  where
    -- return uncommon suffixes and length of common prefix of as and bs
    uncommon = uncommon' 0
    uncommon' x (a:as) (b:bs)
      | a == b     = uncommon' (x+1) as bs
      | otherwise  =  (a:as,b:bs,x)
    uncommon' x as bs = (as,bs,x)

-- | 'createPatchIndexFrom repo pmods' creates a patch index from the given
--   patchmods.
createPatchIndexFrom :: RepoPatch p => Repository p wR wU wT
                     -> [(PatchId, [PatchMod FileName])] -> IO ()
createPatchIndexFrom (Repo repodir _ _ _) pmods = do
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir inv_hash (applyPatchMods pmods emptyPatchIndex)
  where cdir = repodir </> indexDir


-- ---------------------------------------------------------------------
-- Load/Store patch-Index

-- | load patch-index from disk
loadPatchIndex :: FilePath -> IO (Int8, String, Map PatchId Int, PatchIndex)
loadPatchIndex repodir = do
  let pindex_dir = repodir </> indexDir
  (v,inv_hash) <- loadRepoState (pindex_dir </> repoStateFile)
  pids <- loadPatchIds (pindex_dir </> pidsFile)
  let pid2idx  = M.fromList $ zip pids [(1::Int)..]
  infom <- loadInfoMap (pindex_dir </> touchMapFile)
  fidspans <- loadFidMap (pindex_dir </> fidMapFile)
  fpspans <- loadFpMap (pindex_dir </> fpMapFile)
  return (v, inv_hash, pid2idx, PatchIndex pids fidspans fpspans infom)

-- | load patch-index,
-- | ensuring that whenever loaded, the patch-index
-- | can actually be read by the current version of darcs,
-- | and up to date.
loadSafePatchIndex :: (RepoPatch p, ApplyState p ~ Tree)
                   => Repository p wR wU wT
                   -> IO (Map PatchId Int, PatchIndex)
loadSafePatchIndex repo@(Repo repodir _ _ _) = do
   can_use <- isPatchIndexInSync repo
   (_,_,pid2idx,pi) <-
     if can_use
       then loadPatchIndex repodir
       else do createOrUpdatePatchIndexDisk repo
               loadPatchIndex repodir
   return (pid2idx, pi)

-- | check if patch-index exits for this repository
doesPatchIndexExist :: FilePath -> IO Bool
doesPatchIndexExist repodir = do
 filesArePresent <- fmap and $ mapM (doesFileExist . (pindex_dir </>))
                    [repoStateFile, pidsFile, touchMapFile, fidMapFile, fpMapFile]
 if filesArePresent
  then do (v, _, _, _) <- loadPatchIndex repodir
          return (v == version)   -- consider PI only of on-disk format is the current one
  else return False
   where pindex_dir = repodir </> indexDir

-- | check if noPatchIndex exists
isPatchIndexDisabled :: FilePath -> IO Bool
isPatchIndexDisabled repodir = doesFileExist (repodir </> darcsdir  </> noPatchIndex)

-- | create or update patch index
createOrUpdatePatchIndexDisk :: (RepoPatch p, ApplyState p ~ Tree)
                             => Repository p wR wU wT -> IO ()
createOrUpdatePatchIndexDisk repo@(Repo repodir _ _ _)= do
   rmRecursive (repodir </> darcsdir </> noPatchIndex) `catch` \(_ :: IOError) -> return ()
   dpie <- doesPatchIndexExist repodir
   if dpie
      then updatePatchIndexDisk repo
      else createPatchIndexDisk repo

-- | Checks whether a patch index can (and should) be created. If we are not in
-- an old-fashioned repo, and if we haven't been told not to, then we should
-- create a patch index if it doesn't already exist.
canCreatePI :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
            -> IO Bool
canCreatePI (Repo repodir format _ _) =
    (not . or) <$> sequence [ doesntHaveHashedInventory format
                            , isPatchIndexDisabled repodir
                            , doesPatchIndexExist repodir
                            ]
  where
    doesntHaveHashedInventory = return . not . formatHas HashedInventory

-- | see if the default is to use patch index or not
-- | creates Patch index, if it does not exist, and noPatchIndex is not set
canUsePatchIndex :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT -> IO Bool
canUsePatchIndex (Repo repodir _ _ _) = do
     piExists <- doesPatchIndexExist repodir
     piDisabled <- isPatchIndexDisabled repodir
     case (piExists, piDisabled) of
        (True, False) -> return True
        (False, True) -> return False
        (True, True) -> error "patch index exists, and patch index is disabled. run optimize enable-patch-index or disable-patch-index to rectify."
        (False, False) -> return False

createPIWithInterrupt :: (RepoPatch p, ApplyState p ~ Tree)
                      => Repository p wR wU wT -> IO ()
createPIWithInterrupt repo@(Repo repodir _ _ _) = do
            putStrLn "Creating a patch index, please wait. To stop press Ctrl-C"
            (do
              createPatchIndexDisk repo
              putStrLn "Created patch index.") `catchInterrupt` (putStrLn "Patch Index Disabled" >> deletePatchIndex repodir)

-- | check if patch-index is in sync with repository
isPatchIndexInSync :: (RepoPatch p, ApplyState p ~ Tree)
                   => Repository p wR wU wT -> IO Bool
isPatchIndexInSync (Repo repodir _ _ _) = do
   dpie <- doesPatchIndexExist repodir
   if dpie
    then do
      (_, inv_hash_pindex, _, _) <- loadPatchIndex repodir
      inv_hash <- getInventoryHash repodir
      return (inv_hash == inv_hash_pindex)
    else return False

-- | store patch-index on disk
storePatchIndex :: FilePath -> FilePath -> String -> PatchIndex -> IO ()
storePatchIndex repodir cdir inv_hash (PatchIndex pids fidspans fpspans infom) = do
  createDirectory cdir `catch` \(_ :: IOError) -> return ()
  tmpdir <- withPermDir (repodir </> "filecache-tmp") $ \dir -> do
              debugMessage "About to create patch index..."
              let tmpdir = toFilePath dir
              storeRepoState (tmpdir </> repoStateFile) inv_hash
              storePatchIds (tmpdir </> pidsFile) pids
              storeInfoMap (tmpdir </> touchMapFile) infom
              storeFidMap (tmpdir </> fidMapFile) fidspans
              storeFpMap (tmpdir </> fpMapFile) fpspans
              debugMessage "Patch index created"
              return tmpdir
  rmRecursive cdir `catch` \(_ :: IOError) -> return ()
  renameDirectory tmpdir cdir

storeRepoState :: FilePath -> String -> IO ()
storeRepoState fp inv_hash = encodeFile fp (version,inv_hash)

loadRepoState :: FilePath -> IO (Int8, String)
loadRepoState = decodeFile

storePatchIds :: FilePath -> [PatchId] -> IO ()
storePatchIds = encodeFile

loadPatchIds :: FilePath -> IO [PatchId]
loadPatchIds = decodeFile

storeFidMap :: FilePath -> FileIdSpans -> IO ()
storeFidMap fp fidm =
  encodeFile fp $ M.map (map (\(FidSpan a b c) -> (a, b, toIdxM c))) fidm
 where toIdxM (Nothing) = zero
       toIdxM (Just pid) = pid

loadFidMap :: FilePath -> IO FileIdSpans
loadFidMap fp = M.map (map (\(a,b,c) -> FidSpan a b (toPidM c))) <$> decodeFile fp
  where toPidM pid | pid == zero = Nothing
                   | otherwise   = Just pid

storeFpMap :: FilePath -> FilePathSpans -> IO ()
storeFpMap fp fidm =
  encodeFile fp $ M.map (map (\(FpSpan a b c) -> (a, b, toIdxM c))) fidm
 where toIdxM (Nothing) = zero
       toIdxM (Just pid) = pid

loadFpMap :: FilePath -> IO FilePathSpans
loadFpMap fp = M.map (map (\(a,b,c) -> FpSpan a b (toPidM c))) <$> decodeFile fp
  where toPidM pid | pid == zero = Nothing
                   | otherwise   = Just pid

zero :: PatchId
zero = PID $ SHA1 0 0 0 0 0

storeInfoMap :: FilePath -> InfoMap -> IO ()
storeInfoMap fp infom =
  encodeFile fp $ M.map (\fi -> (isFile fi, touching fi)) infom

loadInfoMap :: FilePath -> IO InfoMap
loadInfoMap fp = M.map (\(isF,pids) -> FileInfo isF pids) <$> decodeFile fp

-- | Base directory for the patch index
indexDir :: String
indexDir = darcsdir </> "patch_index"

repoStateFile :: String
repoStateFile = "repo_state"

pidsFile :: String
pidsFile = "patch_ids"

fidMapFile :: String
fidMapFile = "fid_map"

fpMapFile :: String
fpMapFile = "fp_map"

touchMapFile :: String
touchMapFile = "touch_map"

noPatchIndex :: String
noPatchIndex = "no_patch_index"

-----------------------------------------------------------------------
-- Delete patch index
deletePatchIndex :: FilePath -> IO ()
deletePatchIndex repodir = do
    exists <- doesDirectoryExist indexDir
    when exists $
         rmRecursive indexDir
            `catch` \(e :: IOError) -> error $ "Error: Could not delete patch index\n" ++ show e
    (openFile (repodir </> darcsdir </> noPatchIndex) WriteMode >>= hClose)
            `catch` \(e :: IOError) -> error $ "Error: Could not disable patch index\n" ++ show e

-----------------------------------------------------------------------
-- Dump information in patch index

dumpRepoState :: [PatchId] -> String
dumpRepoState = unlines . map pid2string

dumpFileIdSpans :: FileIdSpans -> String
dumpFileIdSpans fidspans =
  unlines [fn2fp fn++" -> "++showFileId fid++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fn, fids) <- M.toList fidspans, FidSpan fid from mto <- fids]

dumpFilePathSpans :: FilePathSpans -> String
dumpFilePathSpans fpspans =
  unlines [showFileId fid++" -> "++ fn2fp fn++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fid, fns) <- M.toList fpspans, FpSpan fn from mto <- fns]

dumpTouchingMap :: InfoMap -> String
dumpTouchingMap infom = unlines [showFileId fid++(if isF then "" else "/")++" -> "++ showAsHex w32
                                | (fid,FileInfo isF w32s) <- M.toList infom, w32 <- S.elems w32s]

-- | return set of current filepaths in patch index
fpSpans2filePaths :: FilePathSpans -> InfoMap -> [FilePath]
fpSpans2filePaths fpSpans infom =
  sort [fn2fp fn ++ (if isF then "" else "/") | (fid,FpSpan fn _ Nothing:_) <- M.toList fpSpans,
                                                let Just (FileInfo isF _) = M.lookup fid infom]

-- | return set of current filepaths in patch index, for internal use
fpSpans2filePaths' :: FileIdSpans -> [FilePath]
fpSpans2filePaths' fidSpans = [fn2fp fp | (fp, _)  <- M.toList fidSpans]

dumpPatchIndex :: FilePath -> IO ()
dumpPatchIndex repodir = do
  (_,inv_hash,_,PatchIndex pids fidspans fpspans infom) <- loadPatchIndex repodir
  putStrLn $ "Inventory hash:" ++ inv_hash
  putStrLn "================="
  putStrLn "Repo state:"
  putStrLn "==========="
  putStrLn $ dumpRepoState pids
  putStrLn "Fileid spans:"
  putStrLn "============="
  putStrLn $ dumpFileIdSpans fidspans
  putStrLn "Filepath spans:"
  putStrLn "=============="
  putStrLn $ dumpFilePathSpans fpspans
  putStrLn "Info Map:"
  putStrLn "========="
  putStrLn $ dumpTouchingMap infom
  putStrLn "Files:"
  putStrLn "=============="
  putStrLn $ unlines $ fpSpans2filePaths fpspans infom


dumpPatchIndexFiles :: FilePath -> IO ()
dumpPatchIndexFiles repodir = do
  (_,_,_,PatchIndex _ _ fpspans infom) <- loadPatchIndex repodir
  putStr $ unlines $ fpSpans2filePaths fpspans infom

-----------------------------------------------------------------------
-- Filtering functions based on FilePaths
-- returns an RL in which the order of patches matters, for annotate to use
getRelevantSubsequence :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p)
                       => Sealed ((RL a) wK) -> Repository p wR wU wR -> [FileName] -> IO (Sealed ((RL a) Origin))
getRelevantSubsequence pxes repository fns = do
   (_, pi@(PatchIndex _ _ _ infom)) <- loadSafePatchIndex repository
   let fids = map (\fn -> evalState (lookupFid fn) pi) fns
       pidss = map ((\(FileInfo _ a) -> a).fromJust.(`M.lookup` infom)) fids
       pids = S.unions pidss
   let flpxes = reverseRL $ unsafeUnseal pxes
   return.seal $ keepElems flpxes NilRL pids

  where keepElems :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p)
                  => FL a wX wY -> RL a wB wX -> S.Set Word32 -> RL a wP wQ
        keepElems NilFL acc _ = unsafeCoerceP acc
        keepElems (x:>:xs) acc pids
          | (short $ makePatchID $ info x) `S.member` pids = keepElems xs (x:<:acc) pids
          | otherwise                                      = keepElems (unsafeCoerceP xs) acc pids

-- | filter given patches so as to keep only the patches that modify the given files
filterPatches :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd p) => Repository p wR wU wT -> [FilePath] -> [Sealed2 a] -> IO [Sealed2 a]
filterPatches repository fps ops = do
   (_, pi@(PatchIndex _ _ _ infom)) <- loadSafePatchIndex repository
   let fids = concatMap ((\fn -> evalState (lookupFids' fn) pi). fp2fn) fps
       npids = S.unions $ map (touching.fromJust.(`M.lookup` infom)) fids
   return $ filter (flip S.member npids . (\(Sealed2 (PIAP pin _)) -> short $ makePatchID pin)) ops

type PatchFilter p = [FilePath] -> [Sealed2 (PatchInfoAnd p)] -> IO [Sealed2 (PatchInfoAnd p)]

-- | If a patch index is available, filter given patches so as to keep only the patches that
-- modify the given files. If none is available, return the original input.
maybeFilterPatches
    :: (RepoPatch p, ApplyState p ~ Tree)
    => Repository p wR wU wT
    -> PatchFilter p
maybeFilterPatches repo fps ops = do
    usePI <- canUsePatchIndex repo
    -- in theory we could change the type signature to make this function staged,
    -- but it doesn't seem worth it.
    if usePI then filterPatches repo fps ops else return ops


-----------------------------------------------------------------------
-- Test patch index

piTest :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
piTest repository = do
   (_, PatchIndex rpids fidspans fpspans infom) <- loadSafePatchIndex repository
   let pids = reverse rpids

   -- test fidspans
   putStrLn "fidspans"
   putStrLn "==========="
   forM_ (M.toList fidspans) $ \(fn, spans) -> do
      let g :: FileIdSpan -> [PatchId]
          g (FidSpan _ x (Just y)) = [y,x]
          g (FidSpan _ x _) = [x]
          ascTs = reverse . nub . concat $ map g spans
      unless (isInOrder ascTs pids) (error $ "In order test failed! filename: " ++ show fn)
      forM_ spans $ \(FidSpan fid _ _) -> unless (M.member fid fpspans) (error $ "Valid file id test failed! fid: " ++ show fid)
   putStrLn "fidspans tests passed"

   -- test fpspans
   putStrLn "fpspans"
   putStrLn "==========="
   forM_ (M.toList fpspans) $ \(fid, spans) -> do
      let g :: FilePathSpan -> [PatchId]
          g (FpSpan _ x (Just y)) = [y,x]
          g (FpSpan _ x _) = [x]
          ascTs = reverse . nub . concat $ map g spans
      unless (isInOrder ascTs pids) (error $ "In order test failed! fileid: " ++ show fid)
      forM_ spans $ \(FpSpan fn _ _) -> unless (M.member fn fidspans) (error $ "Valid file name test failed! file name: " ++ show fn)
      let f :: FilePathSpan -> FilePathSpan -> Bool
          f (FpSpan _ x _) (FpSpan _ _ (Just y)) = x == y
          f _ _ = error "adj test of fpspans fail"
      unless (and $ zipWith f spans (tail spans)) (error $ "Adjcency test failed! fid: " ++ show fid)
   putStrLn "fpspans tests passed"

   -- test infomap
   putStrLn "infom"
   putStrLn "==========="
   putStrLn $ "Valid fid test: " ++ (show.and $ map (`M.member` fpspans) (M.keys infom))
   putStrLn $ "Valid pid test: " ++ (show.flip S.isSubsetOf (S.fromList $ map short pids)  . S.unions . map touching . M.elems $ infom)
   where
          isInOrder :: Eq a => [a] -> [a] -> Bool
          isInOrder (x:xs) (y:ys) | x == y = isInOrder xs ys
                                  | otherwise = isInOrder (x:xs) ys
          isInOrder [] _ = True
          isInOrder _ [] = False

-- | Check if patch index can be created and build it with interrupt.
attemptCreatePatchIndex :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
attemptCreatePatchIndex repo = do
  canCreate <- canCreatePI repo
  when canCreate $ createPIWithInterrupt repo
