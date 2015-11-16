{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses #-}

-- | This module contains plain tree indexing code. The index itself is a
-- CACHE: you should only ever use it as an optimisation and never as a primary
-- storage. In practice, this means that when we change index format, the
-- application is expected to throw the old index away and build a fresh
-- index. Please note that tracking index validity is out of scope for this
-- library: this is responsibility of your application. It is advisable that in
-- your validity tracking code, you also check for format validity (see
-- 'indexFormatValid') and scrap and re-create index when needed.
--
-- The index is a binary file that overlays a hashed tree over the working
-- copy. This means that every working file and directory has an entry in the
-- index, that contains its path and hash and validity data. The validity data
-- is a timestamp plus the file size. The file hashes are sha256's of the
-- file's content. It also contains the fileid to track moved files.
--
-- There are two entry types, a file entry and a directory entry. Both have a
-- common binary format (see 'Item'). The on-disk format is best described by
-- the section /Index format/ below.
--
-- For each file, the index has a copy of the file's last modification
-- timestamp taken at the instant when the hash has been computed. This means
-- that when file size and timestamp of a file in working copy matches those in
-- the index, we assume that the hash stored in the index for given file is
-- valid. These hashes are then exposed in the resulting 'Tree' object, and can
-- be leveraged by eg.  'diffTrees' to compare many files quickly.
--
-- You may have noticed that we also keep hashes of directories. These are
-- assumed to be valid whenever the complete subtree has been valid. At any
-- point, as soon as a size or timestamp mismatch is found, the working file in
-- question is opened, its hash (and timestamp and size) is recomputed and
-- updated in-place in the index file (everything lives at a fixed offset and
-- is fixed size, so this isn't an issue). This is also true of directories:
-- when a file in a directory changes hash, this triggers recomputation of all
-- of its parent directory hashes; moreover this is done efficiently -- each
-- directory is updated at most once during an update run.
--
-- /Index format/
--
-- The Index is organised into \"lines\" where each line describes a single
-- indexed item. Cf. 'Item'.
--
-- The first word on the index \"line\" is the length of the file path (which is
-- the only variable-length part of the line). Then comes the path itself, then
-- fixed-length hash (sha256) of the file in question, then three words, one for
-- size, one for "aux", which is used differently for directories and for files, and
-- one for the fileid (inode or fhandle) of the file.
--
-- With directories, this aux holds the offset of the next sibling line in the
-- index, so we can efficiently skip reading the whole subtree starting at a
-- given directory (by just seeking aux bytes forward). The lines are
-- pre-ordered with respect to directory structure -- the directory comes first
-- and after it come all its items. Cf. 'readIndex''.
--
-- For files, the aux field holds a timestamp.

module Storage.Hashed.Index( readIndex, updateIndexFrom, indexFormatValid
                           , updateIndex, listFileIDs, Index, filter
                           , getFileID )
    where

import Prelude hiding ( lookup, readFile, writeFile, filter, catch )
import Storage.Hashed.Utils
import Storage.Hashed.Tree
import Storage.Hashed.AnchoredPath
import Data.Int( Int64, Int32 )

import Bundled.Posix( getFileStatusBS, modificationTime,
                      getFileStatus, fileSize, fileExists, isDirectory )
import System.IO.MMap( mmapFileForeignPtr, mmapFileByteString, Mode(..) )
import System.IO( )
import System.Directory( doesFileExist, getCurrentDirectory, doesDirectoryExist )
#if mingw32_HOST_OS
import System.Directory( renameFile )
import System.FilePath( (<.>) )
#else
import System.Directory( removeFile )
#endif
import System.FilePath( (</>) )
import System.Posix.Types ( FileID )

import Control.Monad( when )
import Control.Exception( catch, SomeException )
import Control.Applicative( (<$>) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Unsafe( unsafeHead, unsafeDrop )
import Data.ByteString.Internal( toForeignPtr, fromForeignPtr, memcpy
                               , nullForeignPtr, c2w )

import Data.IORef( )
import Data.Maybe( fromJust, isJust, fromMaybe )
import Data.Bits( Bits )

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

import Storage.Hashed.Hash( sha256, rawHash )
#ifdef WIN32
import System.Win32.File ( createFile, getFileInformationByHandle, BY_HANDLE_FILE_INFORMATION(..),
                           fILE_SHARE_NONE, fILE_FLAG_BACKUP_SEMANTICS,
                           gENERIC_NONE, oPEN_EXISTING, closeHandle )
#else
import System.PosixCompat ( fileID, getSymbolicLinkStatus )
#endif

--------------------------
-- Indexed trees
--

-- | Description of a a single indexed item. The structure itself does not
-- contain any data, just pointers to the underlying mmap (bytestring is a
-- pointer + offset + length).
--
-- The structure is recursive-ish (as opposed to flat-ish structure, which is
-- used by git...) It turns out that it's hard to efficiently read a flat index
-- with our internal data structures -- we need to turn the flat index into a
-- recursive Tree object, which is rather expensive... As a bonus, we can also
-- efficiently implement subtree queries this way (cf. 'readIndex').
data Item = Item { iBase :: !(Ptr ())
                 , iHashAndDescriptor :: !BS.ByteString
                 } deriving Show

size_magic :: Int
size_magic = 4 -- the magic word, first 4 bytes of the index

size_dsclen, size_hash, size_size, size_aux, size_fileid :: Int
size_size = 8 -- file/directory size (Int64)
size_aux = 8 -- aux (Int64)
size_fileid = 8 -- fileid (inode or fhandle FileID)
size_dsclen = 4 -- this many bytes store the length of the path
size_hash = 32 -- hash representation

off_size, off_aux, off_hash, off_dsc, off_dsclen, off_fileid :: Int
off_size = 0
off_aux = off_size + size_size
off_fileid = off_aux + size_aux
off_dsclen = off_fileid + size_fileid
off_hash = off_dsclen + size_dsclen
off_dsc = off_hash + size_hash

itemAllocSize :: AnchoredPath -> Int
itemAllocSize apath =
    align 4 $ size_hash + size_size + size_aux + size_fileid + size_dsclen + 2 + BS.length (flatten apath)

itemSize, itemNext :: Item -> Int
itemSize i = size_size + size_aux + size_fileid + size_dsclen + (BS.length $ iHashAndDescriptor i)
itemNext i = align 4 (itemSize i + 1)

iPath, iHash, iDescriptor :: Item -> BS.ByteString
iDescriptor = unsafeDrop size_hash . iHashAndDescriptor
iPath = unsafeDrop 1 . iDescriptor
iHash = BS.take size_hash . iHashAndDescriptor

iSize, iAux :: Item -> Ptr Int64
iSize i = plusPtr (iBase i) off_size
iAux i = plusPtr (iBase i) off_aux

iFileID :: Item -> Ptr FileID
iFileID i = plusPtr (iBase i) off_fileid

itemIsDir :: Item -> Bool
itemIsDir i = unsafeHead (iDescriptor i) == c2w 'D'

-- xlatePeek32 = fmap xlate32 . peek
xlatePeek64 :: (Storable a, Num a, Bits a) => Ptr a -> IO a
xlatePeek64 = fmap xlate64 . peek

-- xlatePoke32 ptr v = poke ptr (xlate32 v)
xlatePoke64 :: (Storable a, Num a, Bits a) => Ptr a -> a -> IO ()
xlatePoke64 ptr v = poke ptr (xlate64 v)

-- | Lay out the basic index item structure in memory. The memory location is
-- given by a ForeignPointer () and an offset. The path and type given are
-- written out, and a corresponding Item is given back. The remaining bits of
-- the item can be filled out using 'update'.
createItem :: ItemType -> AnchoredPath -> ForeignPtr () -> Int -> IO Item
createItem typ apath fp off =
 do let dsc = BS.concat [ BSC.singleton $ if typ == TreeType then 'D' else 'F'
                        , flatten apath
                        , BS.singleton 0 ]
        (dsc_fp, dsc_start, dsc_len) = toForeignPtr dsc
    withForeignPtr fp $ \p ->
        withForeignPtr dsc_fp $ \dsc_p ->
            do fileid <- fromMaybe 0 <$> getFileID apath
               pokeByteOff p (off + off_fileid) (xlate64 $ fromIntegral fileid :: Int64)
               pokeByteOff p (off + off_dsclen) (xlate32 $ fromIntegral dsc_len :: Int32)
               memcpy (plusPtr p $ off + off_dsc)
                      (plusPtr dsc_p dsc_start)
                      (fromIntegral dsc_len)
               peekItem fp off

-- | Read the on-disk representation into internal data structure.
--
-- See the module-level section /Index format/ for details on how the index
-- is structured.
peekItem :: ForeignPtr () -> Int -> IO Item
peekItem fp off =
    withForeignPtr fp $ \p -> do
      nl' :: Int32 <- xlate32 `fmap` peekByteOff p (off + off_dsclen)
      when (nl' <= 2) $ fail "Descriptor too short in peekItem!"
      let nl = fromIntegral nl'
          dsc = fromForeignPtr (castForeignPtr fp) (off + off_hash) (size_hash + nl - 1)
      return $! Item { iBase = plusPtr p off
                     , iHashAndDescriptor = dsc }

-- | Update an existing item with new hash and optionally mtime (give Nothing
-- when updating directory entries).
updateItem :: Item -> Int64 -> Hash -> IO ()
updateItem item _ NoHash =
    fail $ "Index.update NoHash: " ++ BSC.unpack (iPath item)
updateItem item size hash =
    do xlatePoke64 (iSize item) size
       unsafePokeBS (iHash item) (rawHash hash)

updateFileID :: Item -> FileID -> IO ()
updateFileID item fileid = xlatePoke64 (iFileID item) $ fromIntegral fileid
updateAux :: Item -> Int64 -> IO ()
updateAux item aux = xlatePoke64 (iAux item) $ aux
updateTime :: forall a.(Enum a) => Item -> a -> IO ()
updateTime item mtime = updateAux item (fromIntegral $ fromEnum mtime)

iHash' :: Item -> Hash
iHash' i = SHA256 (iHash i)

-- | Gives a ForeignPtr to mmapped index, which can be used for reading and
-- updates. The req_size parameter, if non-0, expresses the requested size of
-- the index file. mmapIndex will grow the index if it is smaller than this.
mmapIndex :: forall a. FilePath -> Int -> IO (ForeignPtr a, Int)
mmapIndex indexpath req_size = do
  exist <- doesFileExist indexpath
  act_size <- fromIntegral `fmap` if exist then fileSize `fmap` getFileStatus indexpath
                                           else return 0
  let size = case req_size > 0 of
        True -> req_size
        False | act_size >= size_magic -> act_size - size_magic
              | otherwise -> 0
  case size of
    0 -> return (castForeignPtr nullForeignPtr, size)
    _ -> do (x, _, _) <- mmapFileForeignPtr indexpath
                                            ReadWriteEx (Just (0, size + size_magic))
            return (x, size)

data IndexM m = Index { mmap :: (ForeignPtr ())
                      , basedir :: FilePath
                      , hashtree :: Tree m -> Hash
                      , predicate :: AnchoredPath -> TreeItem m -> Bool }
              | EmptyIndex

type Index = IndexM IO

data State = State { dirlength :: !Int
                   , path :: !AnchoredPath
                   , start :: !Int }

data Result = Result { -- | marks if the item has changed since the last update to the index
                       changed :: !Bool
                       -- | next is the position of the next item, in bytes.
                     , next :: !Int
                       -- | treeitem is Nothing in case of the item doesn't exist in the tree
                       -- or is filtered by a FilterTree. Or a TreeItem otherwise.
                     , treeitem :: !(Maybe (TreeItem IO))
                       -- | resitem is the item extracted.
                     , resitem :: !Item }

data ResultF = ResultF { -- | nextF is the position of the next item, in bytes.
                         nextF :: !Int
                         -- | resitemF is the item extracted.
                       , resitemF :: !Item
                         -- | _fileIDs contains the fileids of the files and folders inside,
                         -- in a folder item and its own fileid for file item).
                       , _fileIDs :: [((AnchoredPath, ItemType), FileID)] }

readItem :: Index -> State -> IO Result
readItem index state = do
  item <- peekItem (mmap index) (start state)
  res' <- if itemIsDir item
              then readDir  index state item
              else readFile index state item
  return res'

readDir :: Index -> State -> Item -> IO Result
readDir index state item =
    do following <- fromIntegral <$> xlatePeek64 (iAux item)
       st <- getFileStatusBS (iPath item)
       let exists = fileExists st && isDirectory st
       fileid <- fromIntegral <$> (xlatePeek64 $ iFileID item)
       fileid' <- fromMaybe fileid <$> (getFileID' $ BSC.unpack $ iPath item)
       when (fileid == 0) $ updateFileID item fileid'
       let name it dirlen = Name $ (BS.drop (dirlen + 1) $ iDescriptor it) -- FIXME MAGIC
           namelength = (BS.length $ iDescriptor item) - (dirlength state)
           myname = name item (dirlength state)
           substate = state { start = start state + itemNext item
                            , path = path state `appendPath` myname
                            , dirlength = if myname == Name (BSC.singleton '.')
                                             then dirlength state
                                             else dirlength state + namelength }

           want = exists && (predicate index) (path substate) (Stub undefined NoHash)
           oldhash = iHash' item

           subs off | off < following = do
             result <- readItem index $ substate { start = off }
             rest <- subs $ next result
             return $! (name (resitem result) $ dirlength substate, result) : rest
           subs coff | coff == following = return []
                     | otherwise = fail $ "Offset mismatch at " ++ show coff ++
                                          " (ends at " ++ show following ++ ")"

       inferiors <- if want then subs $ start substate
                            else return []

       let we_changed = or [ changed x | (_, x) <- inferiors ] || nullleaf
           nullleaf = null inferiors && oldhash == nullsha
           nullsha = SHA256 (BS.replicate 32 0)
           tree' = makeTree [ (n, fromJust $ treeitem s) | (n, s) <- inferiors, isJust $ treeitem s ]
           treehash = if we_changed then hashtree index tree' else oldhash
           tree = tree' { treeHash = treehash }

       when (exists && we_changed) $ updateItem item 0 treehash
       return $ Result { changed = not exists || we_changed
                       , next = following
                       , treeitem = if want then Just $ SubTree tree
                                            else Nothing
                       , resitem = item }

readFile :: Index -> State -> Item -> IO Result
readFile index state item =
    do st <- getFileStatusBS (iPath item)
       mtime <- fromIntegral <$> (xlatePeek64 $ iAux item)
       size <- xlatePeek64 $ iSize item
       fileid <- fromIntegral <$> (xlatePeek64 $ iFileID item)
       fileid' <- fromMaybe fileid <$> (getFileID' $ BSC.unpack $ iPath item)
       let mtime' = modificationTime st
           size' = fromIntegral $ fileSize st
           readblob = readSegment (basedir index </> BSC.unpack (iPath item), Nothing)
           exists = fileExists st && not (isDirectory st)
           we_changed = mtime /= mtime' || size /= size'
           hash = iHash' item
       when (exists && we_changed) $
            do hash' <- sha256 `fmap` readblob
               updateItem item size' hash'
               updateTime item mtime'
               when (fileid == 0) $ updateFileID item fileid'
       return $ Result { changed = not exists || we_changed
                       , next = start state + itemNext item
                       , treeitem = if exists then Just $ File $ Blob readblob hash else Nothing
                       , resitem = item }

updateIndex :: Index -> IO (Tree IO)
updateIndex EmptyIndex = return emptyTree
updateIndex index =
    do let initial = State { start = size_magic
                           , dirlength = 0
                           , path = AnchoredPath [] }
       res <- readItem index initial
       case treeitem res of
         Just (SubTree tree) -> return $ filter (predicate index) tree
         _ -> fail "Unexpected failure in updateIndex!"

-- | Return a list containing all the file/folder names in an index, with
-- their respective ItemType and FileID.
listFileIDs :: Index -> IO ([((AnchoredPath, ItemType), FileID)])
listFileIDs EmptyIndex = return []
listFileIDs index =
    do let initial = State { start = size_magic
                           , dirlength = 0
                           , path = AnchoredPath [] }
       res <- readItemFileIDs index initial
       return $ _fileIDs res

readItemFileIDs :: Index -> State -> IO ResultF
readItemFileIDs index state = do
  item <- peekItem (mmap index) (start state)
  res' <- if itemIsDir item
              then readDirFileIDs  index state item
              else readFileFileID index state item
  return res'

readDirFileIDs :: Index -> State -> Item -> IO ResultF
readDirFileIDs index state item =
    do fileid <- fromIntegral <$> (xlatePeek64 $ iFileID item)
       following <- fromIntegral <$> xlatePeek64 (iAux item)
       let name it dirlen = Name $ (BS.drop (dirlen + 1) $ iDescriptor it) -- FIXME MAGIC
           namelength = (BS.length $ iDescriptor item) - (dirlength state)
           myname = name item (dirlength state)
           substate = state { start = start state + itemNext item
                            , path = path state `appendPath` myname
                            , dirlength = if myname == Name (BSC.singleton '.')
                                             then dirlength state
                                             else dirlength state + namelength }
           subs off | off < following = do
             result <- readItemFileIDs index $ substate { start = off }
             rest <- subs $ nextF result
             return $! (name (resitemF result) $ dirlength substate, result) : rest
           subs coff | coff == following = return []
                     | otherwise = fail $ "Offset mismatch at " ++ show coff ++
                                          " (ends at " ++ show following ++ ")"
       inferiors <- subs $ start substate
       return $ ResultF { nextF = following
                        , resitemF = item
                        , _fileIDs = (((path substate, TreeType), fileid):concatMap (_fileIDs . snd) inferiors) }

readFileFileID :: Index -> State -> Item -> IO ResultF
readFileFileID _ state item =
    do fileid' <- fromIntegral <$> (xlatePeek64 $ iFileID item)
       let name it dirlen = Name $ (BS.drop (dirlen + 1) $ iDescriptor it)
           myname = name item (dirlength state)
       return $ ResultF { nextF = start state + itemNext item
                        , resitemF = item
                        , _fileIDs = [((path state `appendPath` myname, BlobType), fileid')] }


-- | Read an index and build up a 'Tree' object from it, referring to current
-- working directory. The initial Index object returned by readIndex is not
-- directly useful. However, you can use 'Tree.filter' on it. Either way, to
-- obtain the actual Tree object, call update.
--
-- The usual use pattern is this:
--
-- > do (idx, update) <- readIndex
-- >    tree <- update =<< filter predicate idx
--
-- The resulting tree will be fully expanded.
readIndex :: FilePath -> (Tree IO -> Hash) -> IO Index
readIndex indexpath ht = do
  (mmap_ptr, mmap_size) <- mmapIndex indexpath 0
  base <- getCurrentDirectory
  return $ if mmap_size == 0 then EmptyIndex
                             else Index { mmap = mmap_ptr
                                        , basedir = base
                                        , hashtree = ht
                                        , predicate = \_ _ -> True }

formatIndex :: ForeignPtr () -> Tree IO -> Tree IO -> IO ()
formatIndex mmap_ptr old reference =
    do _ <- create (SubTree reference) (AnchoredPath []) size_magic
       unsafePokeBS magic (BSC.pack "HSI5")
    where magic = fromForeignPtr (castForeignPtr mmap_ptr) 0 4
          create (File _) path' off =
               do i <- createItem BlobType path' mmap_ptr off
                  let flatpath = BSC.unpack $ flatten path'
                  case find old path' of
                    Nothing -> return ()
                    -- TODO calling getFileStatus here is both slightly
                    -- inefficient and slightly race-prone
                    Just ti -> do st <- getFileStatus flatpath
                                  let hash = itemHash ti
                                      mtime = modificationTime st
                                      size = fileSize st
                                  updateItem i (fromIntegral size) hash
                                  updateTime i mtime
                  return $ off + itemNext i
          create (SubTree s) path' off =
               do i <- createItem TreeType path' mmap_ptr off
                  case find old path' of
                    Nothing -> return ()
                    Just ti | itemHash ti == NoHash -> return ()
                            | otherwise -> updateItem i 0 $ itemHash ti
                  let subs [] = return $ off + itemNext i
                      subs ((name,x):xs) = do
                        let path'' = path' `appendPath` name
                        noff <- subs xs
                        create x path'' noff
                  lastOff <- subs (listImmediate s)
                  xlatePoke64 (iAux i) (fromIntegral lastOff)
                  return lastOff
          create (Stub _ _) path' _ =
               fail $ "Cannot create index from stubbed Tree at " ++ show path'

-- | Will add and remove files in index to make it match the 'Tree' object
-- given (it is an error for the 'Tree' to contain a file or directory that
-- does not exist in a plain form in current working directory).
updateIndexFrom :: FilePath -> (Tree IO -> Hash) -> Tree IO -> IO Index
updateIndexFrom indexpath hashtree' ref =
    do old_idx <- updateIndex =<< readIndex indexpath hashtree'
       reference <- expand ref
       let len_root = itemAllocSize anchoredRoot
           len = len_root + sum [ itemAllocSize p | (p, _) <- list reference ]
       exist <- doesFileExist indexpath
#if mingw32_HOST_OS
       when exist $ renameFile indexpath (indexpath <.> "old")
#else
       when exist $ removeFile indexpath -- to avoid clobbering oldidx
#endif
       (mmap_ptr, _) <- mmapIndex indexpath len
       formatIndex mmap_ptr old_idx reference
       readIndex indexpath hashtree'

-- | Check that a given file is an index file with a format we can handle. You
-- should remove and re-create the index whenever this is not true.
indexFormatValid :: FilePath -> IO Bool
indexFormatValid path' =
    do magic <- mmapFileByteString path' (Just (0, size_magic))
       return $ case BSC.unpack magic of
                  "HSI5" -> True
                  _ -> False
    `catch` \(_::SomeException) -> return False

instance FilterTree IndexM IO where
    filter _ EmptyIndex = EmptyIndex
    filter p index = index { predicate = \a b -> predicate index a b && p a b }


-- | For a given file or folder path, get the corresponding fileID from the
-- filesystem.
getFileID :: AnchoredPath -> IO (Maybe FileID)
getFileID = getFileID' . anchorPath ""

getFileID' :: FilePath -> IO (Maybe FileID)
getFileID' fp = do file_exists <- doesFileExist fp
                   dir_exists <- doesDirectoryExist fp
                   if file_exists || dir_exists
#ifdef WIN32
                      then do h <- createFile fp gENERIC_NONE fILE_SHARE_NONE Nothing oPEN_EXISTING fILE_FLAG_BACKUP_SEMANTICS Nothing
                              fhnumber <- (Just . fromIntegral . bhfiFileIndex) <$> getFileInformationByHandle h
                              closeHandle h
                              return fhnumber
#else
                      then (Just . fileID) <$> getSymbolicLinkStatus fp
#endif
                      else return Nothing
