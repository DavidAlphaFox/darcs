-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Darcs.Repository.HashedRepo
    ( inventoriesDir
    , pristineDir
    , patchesDir
    , hashedInventory
    , revertTentativeChanges
    , finalizeTentativeChanges
    , cleanPristine
    , filterDirContents
    , cleanInventories
    , cleanPatches
    , copyPristine
    , copyPartialsPristine
    , applyToTentativePristine
    , addToSpecificInventory
    , addToTentativeInventory
    , removeFromTentativeInventory
    , readRepo
    , readTentativeRepo
    , readRepoUsingSpecificInventory
    , writeAndReadPatch
    , writeTentativeInventory
    , copyHashedInventory
    , readHashedPristineRoot
    , pris2inv
    , inv2pris
    , copySources
    , listInventories
    , listInventoriesLocal
    , listInventoriesRepoDir
    , listPatchesLocalBucketed
    , writePatchIfNecessary
    , readRepoFromInventoryList
    , readPatchIds
    , set
    , unset
    ) where

#include "impossible.h"

import Prelude hiding ( catch )

import Control.Applicative ( (<$>) )
import Control.Arrow ( (&&&) )
import Control.Exception ( catch, IOException )
import Control.Monad ( unless )
import Data.Maybe
import qualified Data.ByteString as B ( null, length, empty ,tail, drop,
                                        ByteString, splitAt )
import qualified Data.ByteString.Char8 as BC ( unpack, dropWhile, break, pack,
                                                ByteString )
import Data.List ( delete )
import qualified Data.Set as Set
import Storage.Hashed.Darcs( hashedTreeIO, readDarcsHashedNosize,
                             readDarcsHashed, writeDarcsHashed,
                             decodeDarcsHash, decodeDarcsSize )
import Storage.Hashed.Tree( treeHash, Tree )
import Storage.Hashed.Hash( encodeBase16, Hash(..) )
import System.Directory ( createDirectoryIfMissing, getDirectoryContents
                        , doesFileExist, doesDirectoryExist )
import System.FilePath.Posix( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO ( stderr, hPutStrLn )

import Darcs.Util.Printer.Color ( showDoc )
import Darcs.Repository.External
    ( copyFileOrUrl
    , cloneFile
    , fetchFilePS
    , gzFetchFilePS
    , Cachable( Uncachable )
    )
import Darcs.Repository.Flags ( Compression, RemoteDarcs, WithWorkingDir )
import Darcs.Util.Global ( darcsdir )
import Darcs.Repository.Lock
    ( writeBinFile
    , writeDocBinFile
    , writeAtomicFilePS
    ,  appendBinFile
    , appendDocBinFile
    , removeFileMayNotExist
    )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, patchInfoAndPatch, info,
                                  extractHash, createHashed )
import Darcs.Patch ( RepoPatch, Patchy, showPatch, readPatch, apply )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.ReadMonads ( parseStrictly )
import Darcs.Patch.Depends ( commuteToEnd, slightlyOptimizePatchset )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo, showPatchInfoUI,
                          readPatchInfo )
import Darcs.Util.Path ( FilePathLike, ioAbsoluteOrRemote, toPath )
import Darcs.Repository.Cache ( Cache(..), CacheLoc(..), fetchFileUsingCache,
                                speculateFilesUsingCache, writeFileUsingCache,
                                unionCaches, repo2cache, okayHash, takeHash,
                                HashedDir(..), hashedDir, peekInCache, bucketFolder )
import qualified Darcs.Repository.Cache as DarcsCache
import Darcs.Repository.HashedIO ( copyHashed, copyPartialsHashed,
                                   cleanHashdir )
import Darcs.Repository.InternalTypes ( Repository(..), extractCache,
                                        modifyCache )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Patch.Witnesses.Ordered
    ( reverseRL, reverseFL, (+<+), FL(..), RL(..),
    (:>)(..), mapRL, mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal, unseal, mapSeal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.ByteString ( gzReadFilePS, dropSpace )
import Darcs.Util.Crypt.SHA256 ( sha256sum )
import Darcs.Util.Printer ( Doc, hcat, (<>), ($$), renderString, renderPS, text,
                 invisiblePS, RenderMode(..) )
import Darcs.Util.Progress ( beginTedious, endTedious, debugMessage, finishedOneIO )
import Darcs.Util.Workaround ( renameFile )
import Darcs.Repository.Prefs ( globalCacheDir )

makeDarcsdirPath :: String -> String
makeDarcsdirPath name = darcsdir </> name

hashedInventory, hashedInventoryPath :: String
hashedInventory = "hashed_inventory"
hashedInventoryPath = makeDarcsdirPath hashedInventory

tentativeHashedInventory, tentativeHashedInventoryPath :: String
tentativeHashedInventory = "tentative_hashed_inventory"
tentativeHashedInventoryPath = makeDarcsdirPath tentativeHashedInventory

inventoriesDir, inventoriesDirPath :: String
inventoriesDir = "inventories"
inventoriesDirPath = makeDarcsdirPath inventoriesDir

pristineDir, tentativePristinePath, pristineDirPath :: String
tentativePristinePath = makeDarcsdirPath "tentative_pristine"
pristineDir = "pristine.hashed"
pristineDirPath = makeDarcsdirPath pristineDir

patchesDir, patchesDirPath :: String
patchesDir = "patches"
patchesDirPath = makeDarcsdirPath patchesDir

pristineNamePrefix :: String
pristineNamePrefix = "pristine:"

pristineName :: B.ByteString
pristineName = BC.pack pristineNamePrefix

-- | 'applyToHashedPristine' takes a root hash, a patch @p@ and attempts to
-- apply the patch to the 'Tree' identified by @h@. If we encounter an old,
-- size-prefixed pristine, we first convert it to the non-size-prefixed format,
-- then apply the patch.
applyToHashedPristine :: (ApplyState p ~ Tree, Patchy p) => String -> p wX wY
                      -> IO String
applyToHashedPristine h p = applyOrConvertOldPristineAndApply
  where
    applyOrConvertOldPristineAndApply =
        tryApply hash `catch` \(_ :: IOException) -> handleOldPristineAndApply

    hash = decodeDarcsHash $ BC.pack h

    failOnMalformedRoot (SHA256 _) = return ()
    failOnMalformedRoot root = fail $ "Cannot handle hash: " ++ show root

    hash2root = BC.unpack . encodeBase16

    tryApply :: Hash -> IO String
    tryApply root = do
        failOnMalformedRoot root
        -- Read a non-size-prefixed pristine, failing if we encounter one.
        tree <- readDarcsHashedNosize pristineDirPath root
        (_, updatedTree) <- hashedTreeIO (apply p) tree pristineDirPath
        return . hash2root $ treeHash updatedTree

    warn = "WARNING: Doing a one-time conversion of pristine format.\n"
           ++ "This may take a while. The new format is backwards-compatible."

    handleOldPristineAndApply = do
        hPutStrLn stderr warn
        inv <- gzReadFilePS hashedInventoryPath
        let oldroot = BC.pack $ inv2pris inv
            oldrootSizeandHash = (decodeDarcsSize &&& decodeDarcsHash) oldroot
        -- Read the old size-prefixed pristine tree
        old <- readDarcsHashed pristineDirPath oldrootSizeandHash
        -- Write out the pristine tree as a non-size-prefixed pristine.
        root <- writeDarcsHashed old pristineDirPath
        let newroot = hash2root root
        -- Write out the new inventory.
        writeDocBinFile hashedInventoryPath $ pris2inv newroot inv
        cleanHashdir (Ca []) HashedPristineDir [newroot]
        hPutStrLn stderr "Pristine conversion done..."
        -- Retry applying the patch, which should now succeed.
        tryApply root

-- |revertTentativeChanges swaps the tentative and "real" hashed inventory
-- files, and then updates the tentative pristine with the "real" inventory
-- hash.
revertTentativeChanges :: IO ()
revertTentativeChanges = do
    cloneFile hashedInventoryPath tentativeHashedInventoryPath
    i <- gzReadFilePS hashedInventoryPath
    writeBinFile tentativePristinePath $ pristineNamePrefix ++ inv2pris i

-- |finalizeTentativeChanges trys to atomically swap the tentative
-- inventory/pristine pointers with the "real" pointers; it first re-reads the
-- inventory to optimize it, presumably to take account of any new tags, and
-- then writes out the new tentative inventory, and finally does the atomic
-- swap. In general, we can't clean the pristine cache at the same time, since
-- a simultaneous get might be in progress.
finalizeTentativeChanges :: (RepoPatch p, ApplyState p ~ Tree)
                         => Repository p wR wU wT -> Compression -> IO ()
finalizeTentativeChanges r compr = do
    debugMessage "Optimizing the inventory..."
    -- Read the tentative patches
    ps <- readTentativeRepo r "."
    writeTentativeInventory (extractCache r) compr ps
    i <- gzReadFilePS tentativeHashedInventoryPath
    p <- gzReadFilePS tentativePristinePath
    -- Write out the "optimised" tentative inventory.
    writeDocBinFile tentativeHashedInventoryPath $ pris2inv (inv2pris p) i
    -- Atomically swap.
    renameFile tentativeHashedInventoryPath hashedInventoryPath

-- |readHashedPristineRoot attempts to read the pristine hash from the current
-- inventory, returning Nothing if it cannot do so.
readHashedPristineRoot :: Repository p wR wU wT -> IO (Maybe String)
readHashedPristineRoot (Repo d _ _ _) = withCurrentDirectory d $ do
    i <- (Just <$> gzReadFilePS hashedInventoryPath)
         `catch` (\(_ :: IOException) -> return Nothing)
    return $ inv2pris <$> i

-- |cleanPristine removes any obsolete (unreferenced) entries in the pristine
-- cache.
cleanPristine :: Repository p wR wU wT -> IO ()
cleanPristine r@(Repo d _ _ _) = withCurrentDirectory d $ do
    debugMessage "Cleaning out the pristine cache..."
    i <- gzReadFilePS hashedInventoryPath
    cleanHashdir (extractCache r) HashedPristineDir [inv2pris i]

-- |filterDirContents returns the contents of the directory @d@
-- except files whose names begin with '.' (directories . and ..,
-- hidden files) and files whose names are filtered by the function @f@, if
-- @dir@ is empty, no paths are returned.
filterDirContents :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
filterDirContents d f = do
    let realPath = makeDarcsdirPath d
    exists <- doesDirectoryExist realPath
    if exists
        then filter (\x -> head x /= '.' && f x) <$>
            getDirectoryContents realPath
        else return []

-- |set converts a list of strings into a set of Char8 ByteStrings for faster
-- Set operations.
set :: [String] -> Set.Set BC.ByteString
set = Set.fromList . map BC.pack

-- |unset is the inverse of set.
unset :: Set.Set BC.ByteString -> [String]
unset = map BC.unpack . Set.toList

-- |cleanInventories removes any obsolete (unreferenced) files in the
-- inventories directory.
cleanInventories :: Repository p wR wU wT -> IO ()
cleanInventories _ = do
    debugMessage "Cleaning out inventories..."
    hs <- listInventoriesLocal
    fs <- filterDirContents inventoriesDir (const True)
    mapM_ (removeFileMayNotExist . (inventoriesDirPath </>))
        (unset $ (set fs) `Set.difference` (set hs))

-- |specialPatches list of special patch files that may exist in the directory
-- _darcs/patches/.
specialPatches :: [FilePath]
specialPatches = ["unrevert", "pending", "pending.tentative"]

-- |cleanPatches removes any obsolete (unreferenced) files in the
-- patches directory.
cleanPatches :: Repository p wR wU wT -> IO ()
cleanPatches _ = do
    debugMessage "Cleaning out patches..."
    hs <- listPatchesLocal darcsdir
    fs <- filterDirContents patchesDir (`notElem` specialPatches)
    mapM_ (removeFileMayNotExist . (patchesDirPath </>))
        (unset $ (set fs) `Set.difference` (set hs))


-- |addToSpecificInventory adds a patch to a specific inventory file, and
-- returns the FilePath whichs corresponds to the written-out patch.
addToSpecificInventory :: RepoPatch p => String -> Cache -> Compression
                       -> PatchInfoAnd p wX wY -> IO FilePath
addToSpecificInventory invPath c compr p = do
    let invFile = darcsdir </> invPath
    hash <- snd <$> writePatchIfNecessary c compr p
    appendDocBinFile invFile $ showPatchInfo $ info p
    appendBinFile invFile $ "\nhash: " ++ hash ++ "\n"
    return $ darcsdir </> "patches" </> hash

addToTentativeInventory :: RepoPatch p => Cache -> Compression
                        -> PatchInfoAnd p wX wY -> IO FilePath
addToTentativeInventory = addToSpecificInventory tentativeHashedInventory

-- |removeFromTentativeInventory attempts to remove an FL of patches from the
-- tentative inventory. This is used for commands that wish to modify
-- already-recorded patches.
removeFromTentativeInventory :: (RepoPatch p, ApplyState p ~ Tree)
                             => Repository p wR wU wT -> Compression
                             -> FL (PatchInfoAnd p) wX wT -> IO ()
removeFromTentativeInventory repo compr to_remove = do
    -- FIXME: This algorithm should be *far* simpler.  All we need do is to to
    -- remove the patches from a patchset and then write that patchset.  The
    -- commutation behavior of PatchInfoAnd should track which patches need to
    -- be rewritten for us.
    allpatches <- readTentativeRepo repo "."
    _ :> skipped <- return $ commuteToEnd (reverseFL to_remove) allpatches
    okay <- simpleRemoveFromTentativeInventory $
                mapFL info to_remove ++ mapRL info skipped
    unless okay $ bug "bug in HashedRepo.removeFromTentativeInventory"
    sequence_ $ mapFL (addToTentativeInventory (extractCache repo) compr)
                    (reverseRL skipped)
  where
    simpleRemoveFromTentativeInventory :: [PatchInfo] -> IO Bool
    simpleRemoveFromTentativeInventory pis = do
        inv <- readTentativeRepo repo "."
        case cut_inv pis inv of
            Nothing -> return False
            Just (Sealed inv') -> do
                writeTentativeInventory (extractCache repo) compr inv'
                return True
    cut_inv :: [PatchInfo] -> PatchSet p wStart wX
            -> Maybe (SealedPatchSet p wStart)
    cut_inv [] x = Just $ seal x
    cut_inv x (PatchSet NilRL (Tagged t _ ps :<: ts)) =
        cut_inv x (PatchSet (t :<: ps) ts)
    cut_inv xs (PatchSet (hp:<:r) ts) | info hp `elem` xs =
        cut_inv (info hp `delete` xs) (PatchSet r ts)
    cut_inv _ _ = Nothing

-- |writeHashFile takes a Doc and writes it as a hash-named file, returning the
-- filename that the contents were written to.
writeHashFile :: Cache -> Compression -> HashedDir -> Doc -> IO String
writeHashFile c compr subdir d = do
    debugMessage $ "Writing hash file to " ++ hashedDir subdir
    writeFileUsingCache c compr subdir $ renderPS Standard d

-- |readRepo returns the "current" repo patchset.
readRepo :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT
         -> String -> IO (PatchSet p Origin wR)
readRepo = readRepoUsingSpecificInventory hashedInventory

-- |readRepo returns the tentative repo patchset.
readTentativeRepo :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository p wR wU wT -> String
                  -> IO (PatchSet p Origin wT)
readTentativeRepo = readRepoUsingSpecificInventory tentativeHashedInventory

-- |readRepoUsingSpecificInventory uses the inventory at @invPath@ to read the
-- repository @repo@.
readRepoUsingSpecificInventory :: (RepoPatch p, ApplyState p ~ Tree)
                               => String -> Repository p wR wU wT
                               -> String -> IO (PatchSet p Origin wS)
readRepoUsingSpecificInventory invPath repo dir = do
    realdir <- toPath <$> ioAbsoluteOrRemote dir
    Sealed ps <- readRepoPrivate (extractCache repo) realdir invPath
                 `catch` \e -> do
                     hPutStrLn stderr ("Invalid repository: " ++ realdir)
                     ioError e
    return $ unsafeCoerceP ps
  where
    readRepoPrivate :: (RepoPatch p, ApplyState p ~ Tree) => Cache -> FilePath
                    -> FilePath -> IO (SealedPatchSet p Origin)
    readRepoPrivate cache d iname = do
      inventory <- readInventoryPrivate (d </> darcsdir) iname
      readRepoFromInventoryList cache inventory

-- |readRepoFromInventoryList allows the caller to provide an optional "from
-- inventory" hash, and a list of info/hash pairs that identify a list of
-- patches, returning a patchset of the resulting repo.
readRepoFromInventoryList :: (RepoPatch p, ApplyState p ~ Tree) => Cache
                          -> (Maybe String, [(PatchInfo, String)])
                          -> IO (SealedPatchSet p Origin)
readRepoFromInventoryList cache = parseinvs
  where
    speculateAndParse h is i = speculate h is >> parse i h

    read_patches :: (RepoPatch p, ApplyState p ~ Tree) => [(PatchInfo, String)]
                 -> IO (Sealed (RL (PatchInfoAnd p) wX))
    read_patches [] = return $ seal NilRL
    read_patches allis@((i1, h1) : is1) =
        lift2Sealed (\p rest -> i1 `patchInfoAndPatch` p :<: rest) (rp is1)
                    (createHashed h1 (const $ speculateAndParse h1 allis i1))
      where
        rp :: (RepoPatch p, ApplyState p ~ Tree) => [(PatchInfo, String)]
           -> IO (Sealed (RL (PatchInfoAnd p) wX))
        rp [] = return $ seal NilRL
        rp [(i, h), (il, hl)] =
            lift2Sealed (\p rest -> i `patchInfoAndPatch` p :<: rest)
                        (rp [(il, hl)])
                        (createHashed h
                            (const $ speculateAndParse h (reverse allis) i))
        rp ((i, h) : is) =
            lift2Sealed (\p rest -> i `patchInfoAndPatch` p :<: rest)
                        (rp is)
                        (createHashed h (parse i))

    read_tag :: (RepoPatch p, ApplyState p ~ Tree) => (PatchInfo, String)
             -> IO (Sealed (PatchInfoAnd p wX))
    read_tag (i, h) =
        mapSeal (patchInfoAndPatch i) <$> createHashed h (parse i)

    speculate :: String -> [(PatchInfo, String)] -> IO ()
    speculate h is = do
        already_got_one <- peekInCache cache HashedPatchesDir h
        unless already_got_one $
            speculateFilesUsingCache cache HashedPatchesDir (map snd is)

    parse :: ReadPatch p => PatchInfo -> String -> IO (Sealed (p wX))
    parse i h = do
        debugMessage ("Reading patch file: "++ showDoc Encode (showPatchInfoUI i))
        (fn, ps) <- fetchFileUsingCache cache HashedPatchesDir h
        case readPatch ps of
            Just p -> return p
            Nothing -> fail $ unlines [ "Couldn't parse file " ++ fn
                                      , "which is patch"
                                      , renderString Encode $ showPatchInfoUI i ]

    parseinvs :: (RepoPatch p, ApplyState p ~ Tree)
              => (Maybe String, [(PatchInfo, String)])
              -> IO (SealedPatchSet p Origin)
    parseinvs (Nothing, ris) =
        mapSeal (flip PatchSet NilRL) <$> read_patches (reverse ris)
    parseinvs (Just h, []) =
        bug $ "bad inventory " ++ h ++ " (no tag) in parseinvs!"
    parseinvs (Just h, t : ris) = do
        Sealed ts <- unseal seal <$> unsafeInterleaveIO (read_ts t h)
        Sealed ps <- unseal seal <$>
                        unsafeInterleaveIO (read_patches $ reverse ris)
        return $ seal $ PatchSet ps ts

    read_ts :: (RepoPatch p, ApplyState p ~ Tree) => (PatchInfo, String)
            -> String -> IO (Sealed (RL (Tagged p) Origin))
    read_ts tag0 h0 = do
        contents <- unsafeInterleaveIO $ readTaggedInventoryFromHash h0
        let is = reverse $ case contents of
                               (Just _, _ : ris0) -> ris0
                               (Nothing, ris0) -> ris0
                               (Just _, []) -> bug "inventory without tag!"
        Sealed ts <- unseal seal <$>
                         unsafeInterleaveIO
                            (case contents of
                                 (Just h', t' : _) -> read_ts t' h'
                                 (Just _, []) -> bug "inventory without tag!"
                                 (Nothing, _) -> return $ seal NilRL)
        Sealed ps <- unseal seal <$> unsafeInterleaveIO (read_patches is)
        Sealed tag00 <- read_tag tag0
        return $ seal $ Tagged tag00 (Just h0) ps :<: ts

    readTaggedInventoryFromHash :: String
                                -> IO (Maybe String, [(PatchInfo, String)])
    readTaggedInventoryFromHash invHash = do
        (fileName, pristineAndInventory) <-
            fetchFileUsingCache cache HashedInventoriesDir invHash
        readInventoryFromContent fileName pristineAndInventory

    lift2Sealed :: (forall wY wZ . q wY wZ -> p wX wY -> r wX wZ)
                -> IO (Sealed (p wX))
                -> (forall wB . IO (Sealed (q wB)))
                -> IO (Sealed (r wX))
    lift2Sealed f iox ioy = do
        Sealed x <- unseal seal <$> unsafeInterleaveIO iox
        Sealed y <- unseal seal <$> unsafeInterleaveIO ioy
        return $ seal $ f y x

-- |readInventoryPrivate reads the inventory with name @invName@ in @dir@.
readInventoryPrivate :: String -> String
                     -> IO (Maybe String, [(PatchInfo, String)])
readInventoryPrivate dir invName = do
    inv <- skipPristine <$> gzFetchFilePS (dir </> invName) Uncachable
    readInventoryFromContent (toPath dir ++ "/" ++ darcsdir ++ invName) inv

-- |readInventoryFromContent extracts an inventory from the content of an
-- inventory file, who's path is @fileName@.
readInventoryFromContent :: FilePath -> B.ByteString
                         -> IO (Maybe String, [(PatchInfo, String)])
readInventoryFromContent fileName pristineAndInventory = do
    (hash, patchIds) <-
        if mbStartingWith == BC.pack "Starting with inventory:"
            then let (hash, pids) = BC.break ('\n' ==) $ B.tail pistr
                     hashStr = BC.unpack hash in
                 if okayHash hashStr
                     then return (Just hashStr, pids)
                     else fail $ "Bad hash in file " ++ fileName
            else return (Nothing, inventory)
    return (hash, readPatchIds patchIds)
  where
    inventory = skipPristine pristineAndInventory
    (mbStartingWith, pistr) = BC.break ('\n' ==) inventory

-- |copyRepo copies the hashed inventory of @repo@ to the repository located at
-- @remote@.
copyHashedInventory :: RepoPatch p => Repository p wR wU wT -> RemoteDarcs -> String -> IO ()
copyHashedInventory (Repo outr _ _ _) remote inr = do
    createDirectoryIfMissing False (outr ++ "/" ++ inventoriesDirPath)
    copyFileOrUrl remote (inr </> darcsdir </> hashedInventory)
                         (outr </> darcsdir </> hashedInventory)
                  Uncachable -- no need to copy anything but hashed_inventory!
    debugMessage "Done copying hashed inventory."

-- |'copySources' copies the prefs/sources file to the local repo, from the
-- remote, having first filtered the local filesystem sources.
copySources :: RepoPatch p => Repository p wR wU wT -> String -> IO ()
copySources repo@(Repo outr _ _ _) inr = do
    let repoCache = extractCache $ modifyCache repo dropNonRepos
    appendBinFile (outr ++ "/" ++ darcsdir ++ "/prefs/sources")
                  (show $ repo2cache inr `unionCaches` repoCache )
    debugMessage "Done copying and filtering pref/sources."
  where
    dropNonRepos (Ca cache) = Ca $ filter notRepo cache
    notRepo xs = case xs of
        Cache DarcsCache.Directory _ _ -> False
        Cache _ DarcsCache.Writable _  -> False
        _                              -> True

-- |writeAndReadPatch makes a patch lazy, by writing it out to disk (thus
-- forcing it), and then re-reads the patch lazily.
writeAndReadPatch :: RepoPatch p => Cache -> Compression
                  -> PatchInfoAnd p wX wY -> IO (PatchInfoAnd p wX wY)
writeAndReadPatch c compr p = do
    (i, h) <- writePatchIfNecessary c compr p
    unsafeInterleaveIO $ readp h i
  where
    parse i h = do
        debugMessage ("Rereading patch file: "++ showDoc Encode (showPatchInfoUI i))
        (fn, ps) <- fetchFileUsingCache c HashedPatchesDir h
        case readPatch ps of
            Just x -> return x
            Nothing -> fail $ unlines [ "Couldn't parse patch file " ++ fn
                                      , "which is"
                                      , renderString Encode $ showPatchInfoUI i]

    readp h i = do Sealed x <- createHashed h (parse i)
                   return . patchInfoAndPatch i $ unsafeCoerceP x

-- | writeTentativeInventory writes @patchSet@ as the tentative inventory.
writeTentativeInventory :: RepoPatch p => Cache -> Compression
                        -> PatchSet p Origin wX -> IO ()
writeTentativeInventory cache compr patchSet = do
    debugMessage "in writeTentativeInventory..."
    createDirectoryIfMissing False inventoriesDirPath
    beginTedious tediousName
    hsh <- writeInventoryPrivate $ slightlyOptimizePatchset patchSet
    endTedious tediousName
    debugMessage "still in writeTentativeInventory..."
    case hsh of
        Nothing -> writeBinFile (darcsdir </> tentativeHashedInventory) ""
        Just h -> do
            content <- snd <$> fetchFileUsingCache cache HashedInventoriesDir h
            writeAtomicFilePS (darcsdir </> tentativeHashedInventory) content
  where
    tediousName = "Writing inventory"
    writeInventoryPrivate :: RepoPatch p => PatchSet p Origin wX
                          -> IO (Maybe String)
    writeInventoryPrivate (PatchSet NilRL NilRL) = return Nothing
    writeInventoryPrivate (PatchSet ps NilRL) = do
        inventory <- sequence $ mapRL (writePatchIfNecessary cache compr) ps
        let inventorylist = hcat (map pihash $ reverse inventory)
        hash <- writeHashFile cache compr HashedInventoriesDir inventorylist
        return $ Just hash
    writeInventoryPrivate
        (PatchSet x xs@(Tagged t _ _ :<: _)) = do
        resthash <- write_ts xs
        finishedOneIO tediousName $ fromMaybe "" resthash
        inventory <- sequence $ mapRL (writePatchIfNecessary cache compr)
                                    (x +<+ t :<: NilRL)
        let inventorylist = hcat (map pihash $ reverse inventory)
            inventorycontents =
                case resthash of
                    Just h -> text ("Starting with inventory:\n" ++ h) $$
                                  inventorylist
                    Nothing -> inventorylist
        hash <- writeHashFile cache compr HashedInventoriesDir inventorycontents
        return $ Just hash
      where
        -- | write_ts writes out a tagged patchset. If it has already been
        -- written, we'll have the hash, so we can immediately return it.
        write_ts :: RepoPatch p => RL (Tagged p) Origin wX
                 -> IO (Maybe String)
        write_ts (Tagged _ (Just h) _ :<: _) = return (Just h)
        write_ts (Tagged _ Nothing pps :<: tts) =
            writeInventoryPrivate $ PatchSet pps tts
        write_ts NilRL = return Nothing

-- |writeHashIfNecessary writes the patch and returns the resulting info/hash,
-- if it has not already been written. If it has been written, we have the hash
-- in the PatchInfoAnd, so we extract and return the info/hash.
writePatchIfNecessary :: RepoPatch p => Cache -> Compression
                      -> PatchInfoAnd p wX wY -> IO (PatchInfo, String)
writePatchIfNecessary c compr hp = infohp `seq`
    case extractHash hp of
        Right h -> return (infohp, h)
        Left p -> (\h -> (infohp, h)) <$>
                        writeHashFile c compr HashedPatchesDir (showPatch p)
  where
    infohp = info hp

-- |pihash takes an info/hash pair, and renders the info, along with the hash
-- as a Doc.
pihash :: (PatchInfo, String) -> Doc
pihash (pinf, hash) = showPatchInfo pinf $$ text ("hash: " ++ hash ++ "\n")

-- |listInventoriesWith returns a list of the inventories hashes.
-- The function @f@ can be readInventoryPrivate or readInventoryLocalPrivate.
-- The argument @hashedRepoDir@ is the path to the repository,
-- where it's the 'hashed_inventory' file.
-- The argument @darcsDir@ is the path to the directory of inventories files.
listInventoriesWith ::  (String -> String
    -> IO (Maybe String, [(PatchInfo, String)]))
    -> String -> String -> IO [String]
listInventoriesWith f darcsDir hashedRepoDir = do
    mbStartingWithInv <- getStartingWithHash hashedRepoDir hashedInventory
    followStartingWiths mbStartingWithInv
  where
    getStartingWithHash invDir inv =
        fst <$> f invDir inv

    followStartingWiths Nothing = return []
    followStartingWiths (Just startingWith) = do
        mbNextInv <- getStartingWithHash (darcsDir </> inventoriesDir) startingWith
        (startingWith :) <$> followStartingWiths mbNextInv

-- |listInventoriesBucketedWith is similar to listInventoriesWith, but
-- it read the inventory directory under @darcsDir@ in bucketed format.
listInventoriesBucketedWith ::  (String -> String
    -> IO (Maybe String, [(PatchInfo, String)]))
    -> String -> String -> IO [String]
listInventoriesBucketedWith f darcsDir hashedRepoDir = do
    mbStartingWithInv <- getStartingWithHash hashedRepoDir hashedInventory
    followStartingWiths mbStartingWithInv
  where
    getStartingWithHash invDir inv =
        fst <$> f invDir inv

    followStartingWiths Nothing = return []
    followStartingWiths (Just startingWith) = do
        mbNextInv <- getStartingWithHash
            (darcsDir </> inventoriesDir </> bucketFolder startingWith) startingWith
        (startingWith :) <$> followStartingWiths mbNextInv

-- |listInventories returns a list of the inventories hashes.
-- This function attempts to retrieve missing inventory files.
listInventories :: IO [String]
listInventories = listInventoriesWith readInventoryPrivate darcsdir darcsdir

-- |readInventoryLocalPrivate reads the inventory with name @invName@ in @dir@
-- if it exist, otherwise returns an empty inventory.
readInventoryLocalPrivate :: String -> String
    -> IO (Maybe String, [(PatchInfo, String)])
readInventoryLocalPrivate dir invName = do
    b <- doesFileExist (dir </> invName)
    if b then readInventoryPrivate dir invName
        else return (Nothing, [])

-- |listInventoriesLocal returns a list of the inventories hashes.
-- This function does not attempt to retrieve missing inventory files.
listInventoriesLocal :: IO [String]
listInventoriesLocal = listInventoriesWith readInventoryLocalPrivate darcsdir darcsdir

-- |listInventoriesRepoDir returns a list of the inventories hashes.
-- The argument @repoDir@ is the directory of the repository from which
-- we are going to read the "hashed_inventory" file.
-- The rest of hashed files are read from the global cache.
listInventoriesRepoDir :: String -> IO [String]
listInventoriesRepoDir repoDir = do
    gCacheDir' <- globalCacheDir
    let gCacheInvDir = fromJust gCacheDir'
    listInventoriesBucketedWith readInventoryLocalPrivate gCacheInvDir (repoDir </> darcsdir)

-- |listPatchesLocal returns a list of the patches hashes, extracted
-- from inventory files, by following the inventory "chain" of "Starting
-- with inventory" hashes. This function does not attempt to download missing
-- inventory files.
-- The argument @darcsDir@ is the path to the darcs directory (e.g. "_darcs")
-- of the repository from which we're going to read the inventories.
listPatchesLocal :: String -> IO [String]
listPatchesLocal darcsDir = do
    inventory <- readInventoryPrivate darcsDir hashedInventory
    followStartingWiths (fst inventory) (getPatches inventory)
    where
        followStartingWiths Nothing patches = return patches
        followStartingWiths (Just startingWith) patches = do
            inv <- readInventoryLocalPrivate (darcsDir </> inventoriesDir) startingWith
            (patches++) <$> followStartingWiths (fst inv) (getPatches inv)

        getPatches inv = map snd (snd inv)

-- |listPatchesLocalBucketed is similar to listPatchesLocal, but
-- it read the inventory directory under @darcsDir@ in bucketed format.
listPatchesLocalBucketed :: String -> String -> IO [String]
listPatchesLocalBucketed darcsDir hashedRepoDir = do
    inventory <- readInventoryPrivate hashedRepoDir hashedInventory
    followStartingWiths (fst inventory) (getPatches inventory)
    where
        followStartingWiths Nothing patches = return patches
        followStartingWiths (Just startingWith) patches = do
            inv <- readInventoryLocalPrivate
                (darcsDir </> inventoriesDir </> bucketFolder startingWith) startingWith
            (patches++) <$> followStartingWiths (fst inv) (getPatches inv)

        getPatches inv = map snd (snd inv)

-- | 'readPatchIds inventory' parses the content of a hashed_inventory file
-- after the "pristine:" and "Starting with inventory:" header lines have
-- been removed.  The second value in the resulting tuples is the file hash
-- of the associated patch (the "hash:" line).
readPatchIds :: B.ByteString -> [(PatchInfo, String)]
readPatchIds inv | B.null inv = []
readPatchIds inv = case parseStrictly readPatchInfo inv of
                       Nothing -> []
                       Just (pinfo, r) ->
                           case readHash r of
                               Nothing -> []
                               Just (h, r') -> (pinfo, h) : readPatchIds r'
  where
    readHash :: B.ByteString -> Maybe (String, B.ByteString)
    readHash s = let s' = dropSpace s
                     (l, r) = BC.break ('\n' ==) s'
                     (kw, h) = BC.break (' ' ==) l in
                 if kw /= BC.pack "hash:" || B.length h <= 1
                     then Nothing
                     else Just (BC.unpack $ B.tail h, r)

-- |applyToTentativePristine applies a patch @p@ to the tentative pristine
-- tree, and updates the tentative pristine hash
applyToTentativePristine :: (ApplyState p ~ Tree, Patchy p) => p wX wY
                         -> IO ()
applyToTentativePristine p = do
    tentativePristine <- gzReadFilePS tentativePristinePath
    -- Extract the pristine hash from the tentativePristine file, using
    -- inv2pris (this is valid since we normally just extract the hash from the
    -- first line of an inventory file; we can pass in a one-line file that
    -- just contains said hash).
    let tentativePristineHash = inv2pris tentativePristine
    newPristineHash <- applyToHashedPristine tentativePristineHash p
    writeDocBinFile tentativePristinePath $
        pris2inv newPristineHash tentativePristine

-- | copyPristine copies a pristine tree into the current pristine dir,
--   and possibly copies a clean working copy.
--   The target is read from the passed-in dir/inventory name combination.
copyPristine :: Cache -> String -> String -> WithWorkingDir -> IO ()
copyPristine cache dir iname wwd = do
    i <- fetchFilePS (dir ++ "/" ++ iname) Uncachable
    debugMessage $ "Copying hashed pristine tree: " ++ inv2pris i
    let tediousName = "Copying pristine"
    beginTedious tediousName
    copyHashed tediousName cache wwd $ inv2pris i
    endTedious tediousName

-- |copyPartialsPristine copies the pristine entries for a given list of
-- filepaths.
copyPartialsPristine :: FilePathLike fp => Cache -> String
                     -> String -> [fp] -> IO ()
copyPartialsPristine c d iname fps = do
    i <- fetchFilePS (d ++ "/" ++ iname) Uncachable
    copyPartialsHashed c (inv2pris i) fps

-- |pris2inv takes an updated pristine hash and an inventory, and outputs the
-- new pristine hash followed by the original inventory (having skipped the old
-- inventory hash).
pris2inv :: String -> B.ByteString -> Doc
pris2inv h inv = invisiblePS pristineName <> text h $$
                     invisiblePS (skipPristine inv)

-- |inv2pris takes the content of an inventory, and extracts the corresponding
-- pristine hash from the inventory (the hash is prefixed by "pristine:").
inv2pris :: B.ByteString -> String
inv2pris inv = case tryDropPristineName inv of
                   Just rest -> case takeHash rest of
                                    Just (h, _) -> h
                                    Nothing -> error "Bad hash in inventory!"
                   Nothing -> sha256sum B.empty

-- |skipPristine drops the 'pristine: HASH' prefix line, if present.
skipPristine :: B.ByteString -> B.ByteString
skipPristine ps = case tryDropPristineName ps of
    Just rest -> B.drop 1 $ BC.dropWhile (/= '\n') rest
    Nothing -> ps

-- |tryDropPristineName returns the result of dropping the pristineName from
-- the input, if it was present, otherwise it returns Nothing.
tryDropPristineName :: B.ByteString -> Maybe B.ByteString
tryDropPristineName input =
    if prefix == pristineName then Just rest else Nothing
  where
    (prefix, rest) = B.splitAt (B.length pristineName) input
