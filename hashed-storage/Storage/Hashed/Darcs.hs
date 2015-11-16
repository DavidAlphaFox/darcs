{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

-- | A few darcs-specific utility functions. These are used for reading and
-- writing darcs and darcs-compatible hashed trees.
module Storage.Hashed.Darcs where

import Prelude hiding ( lookup, catch )
import System.FilePath ( (</>) )

import System.Directory( doesFileExist )
import Codec.Compression.GZip( decompress, compress )
import Control.Applicative( (<$>) )
import Control.Exception( catch, IOException )

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Data.List( sortBy )
import Data.Char( chr, ord, isSpace )
import Data.Maybe( fromJust, isJust )
import Control.Monad.State.Strict

import Storage.Hashed.Tree hiding ( lookup )
import Storage.Hashed.AnchoredPath
import Storage.Hashed.Utils
import Storage.Hashed.Hash
import Storage.Hashed.Packed
import Storage.Hashed.Monad

---------------------------------------------------------------------
-- Utilities for coping with the darcs directory format.
--

-- | 'darcsDecodeWhite' interprets the Darcs-specific \"encoded\" filenames
--   produced by 'darcsEncodeWhite'
--
--   > darcsDecodeWhite "hello\32\there" == "hello there"
--   > darcsDecodeWhite "hello\92\there" == "hello\there"
--   > darcsDecodeWhite "hello\there"    == error "malformed filename"
darcsDecodeWhite :: String -> FilePath
darcsDecodeWhite ('\\':cs) =
    case break (=='\\') cs of
    (theord, '\\':rest) ->
        chr (read theord) : darcsDecodeWhite rest
    _ -> error "malformed filename"
darcsDecodeWhite (c:cs) = c: darcsDecodeWhite cs
darcsDecodeWhite "" = ""

-- | 'darcsEncodeWhite' translates whitespace in filenames to a darcs-specific
--   format (backslash followed by numerical representation according to 'ord').
--   Note that backslashes are also escaped since they are used in the encoding.
--
--   > darcsEncodeWhite "hello there" == "hello\32\there"
--   > darcsEncodeWhite "hello\there" == "hello\92\there"
darcsEncodeWhite :: FilePath -> String
darcsEncodeWhite (c:cs) | isSpace c || c == '\\' =
    '\\' : (show $ ord c) ++ "\\" ++ darcsEncodeWhite cs
darcsEncodeWhite (c:cs) = c : darcsEncodeWhite cs
darcsEncodeWhite [] = []

darcsEncodeWhiteBS :: BS8.ByteString -> BS8.ByteString
darcsEncodeWhiteBS = BS8.pack . darcsEncodeWhite . BS8.unpack

decodeDarcsHash :: BS8.ByteString -> Hash
decodeDarcsHash bs = case BS8.split '-' bs of
                       [s, h] | BS8.length s == 10 -> decodeBase16 h
                       _ -> decodeBase16 bs

decodeDarcsSize :: BS8.ByteString -> Maybe Int
decodeDarcsSize bs = case BS8.split '-' bs of
                       [s, _] | BS8.length s == 10 ->
                                  case reads (BS8.unpack s) of
                                    [(x, _)] -> Just x
                                    _ -> Nothing
                       _ -> Nothing

darcsLocation :: FilePath -> (Maybe Int, Hash) -> FileSegment
darcsLocation dir (s,h) = case hash of
                            "" -> error "darcsLocation: invalid hash"
                            _ -> (dir </> prefix s ++ hash, Nothing)
    where prefix Nothing = ""
          prefix (Just s') = formatSize s' ++ "-"
          formatSize s' = let n = show s' in replicate (10 - length n) '0' ++ n
          hash = BS8.unpack (encodeBase16 h)

----------------------------------------------
-- Darcs directory format.
--

darcsFormatDir :: Tree m -> Maybe BL8.ByteString
darcsFormatDir t = BL8.fromChunks <$> concat <$>
                       mapM string (sortBy cmp $ listImmediate t)
    where cmp (Name a, _) (Name b, _) = compare a b
          string (Name name, item) =
              do header <- case item of
                             File _ -> Just $ BS8.pack "file:\n"
                             _ -> Just $ BS8.pack "directory:\n"
                 hash <- case itemHash item of
                           NoHash -> Nothing
                           x -> Just $ encodeBase16 x
                 return $ [ header
                          , darcsEncodeWhiteBS name
                          , BS8.singleton '\n'
                          , hash, BS8.singleton '\n' ]

darcsParseDir :: BL8.ByteString -> [(ItemType, Name, Maybe Int, Hash)]
darcsParseDir content = parse (BL8.split '\n' content)
    where
      parse (t:n:h':r) = (header t,
                          Name $ BS8.pack $ darcsDecodeWhite (BL8.unpack n),
                          decodeDarcsSize hash,
                          decodeDarcsHash hash) : parse r
          where hash = BS8.concat $ BL8.toChunks h'
      parse _ = []
      header x
          | x == BL8.pack "file:" = BlobType
          | x == BL8.pack "directory:" = TreeType
          | otherwise = error $ "Error parsing darcs hashed dir: " ++ BL8.unpack x

----------------------------------------
-- Utilities.
--

-- | Compute a darcs-compatible hash value for a tree-like structure.
darcsTreeHash :: Tree m -> Hash
darcsTreeHash t = case darcsFormatDir t of
                    Nothing -> NoHash
                    Just x -> sha256 x

-- The following two are mostly for experimental use in Packed.

darcsUpdateDirHashes :: Tree m -> Tree m
darcsUpdateDirHashes = updateSubtrees update
    where update t = t { treeHash = darcsTreeHash t }

darcsUpdateHashes :: (Monad m, Functor m) => Tree m -> m (Tree m)
darcsUpdateHashes = updateTree update
    where update (SubTree t) = return . SubTree $ t { treeHash = darcsTreeHash t }
          update (File blob@(Blob con _)) =
              do hash <- sha256 <$> readBlob blob
                 return $ File (Blob con hash)
          update stub = return stub

darcsHash :: (Monad m, Functor m) => TreeItem m -> m Hash
darcsHash (SubTree t) = return $ darcsTreeHash t
darcsHash (File blob) = sha256 <$> readBlob blob
darcsHash _ = return NoHash

darcsAddMissingHashes :: (Monad m, Functor m) => Tree m -> m (Tree m)
darcsAddMissingHashes = addMissingHashes darcsHash

-------------------------------------------
-- Reading darcs pristine data
--

-- | Read and parse a darcs-style hashed directory listing from a given @dir@
-- and with a given @hash@.
readDarcsHashedDir :: FilePath -> (Maybe Int, Hash) -> IO [(ItemType, Name, Maybe Int, Hash)]
readDarcsHashedDir dir h = do
  exist <- doesFileExist $ fst (darcsLocation dir h)
  unless exist $ fail $ "error opening " ++ fst (darcsLocation dir h)
  compressed <- readSegment $ darcsLocation dir h
  let content = decompress compressed
  return $ if BL8.null compressed
              then []
              else darcsParseDir content

-- | Read in a darcs-style hashed tree. This is mainly useful for reading
-- \"pristine.hashed\". You need to provide the root hash you are interested in
-- (found in _darcs/hashed_inventory).
readDarcsHashed' :: Bool -> FilePath -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed' _ _ (_, NoHash) = fail "Cannot readDarcsHashed NoHash"
readDarcsHashed' sizefail dir root@(_, hash) = do
  items' <- readDarcsHashedDir dir root
  subs <- sequence [
           do when (sizefail && isJust s) $
                fail ("Unexpectedly encountered size-prefixed hash in " ++ dir)
              case tp of
                BlobType -> return (d, File $
                                       Blob (readBlob' (s, h)) h)
                TreeType ->
                  do let t = readDarcsHashed dir (s, h)
                     return (d, Stub t h)
           | (tp, d, s, h) <- items' ]
  return $ makeTreeWithHash subs hash
    where readBlob' = fmap decompress . readSegment . darcsLocation dir

readDarcsHashed :: FilePath -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed = readDarcsHashed' False

readDarcsHashedNosize :: FilePath -> Hash -> IO (Tree IO)
readDarcsHashedNosize dir hash = readDarcsHashed' True dir (Nothing, hash)

----------------------------------------------------
-- Writing darcs-style hashed trees.
--

-- | Write a Tree into a darcs-style hashed directory.
writeDarcsHashed :: Tree IO -> FilePath -> IO Hash
writeDarcsHashed tree' dir =
    do t <- darcsUpdateDirHashes <$> expand tree'
       sequence_ [ dump =<< readBlob b | (_, File b) <- list t ]
       let dirs = darcsFormatDir t : [ darcsFormatDir d | (_, SubTree d) <- list t ]
       _ <- mapM dump $ map fromJust dirs
       return $ darcsTreeHash t
    where dump bits =
              do let name = dir </> BS8.unpack (encodeBase16 $ sha256 bits)
                 exist <- doesFileExist name
                 unless exist $ BL.writeFile name (compress bits)

-- | Create a hashed file from a 'FilePath' and content. In case the file exists
-- it is kept untouched and is assumed to have the right content. XXX Corrupt
-- files should be probably renamed out of the way automatically or something
-- (probably when they are being read though).
fsCreateHashedFile :: FilePath -> BL8.ByteString -> TreeIO ()
fsCreateHashedFile fn content =
    liftIO $ do
      exist <- doesFileExist fn
      unless exist $ BL.writeFile fn content

-- | Run a 'TreeIO' @action@ in a hashed setting. The @initial@ tree is assumed
-- to be fully available from the @directory@, and any changes will be written
-- out to same. Please note that actual filesystem files are never removed.
hashedTreeIO :: TreeIO a -- ^ action
             -> Tree IO -- ^ initial
             -> FilePath -- ^ directory
             -> IO (a, Tree IO)
hashedTreeIO action t dir =
    do runTreeMonad action $ initialState t darcsHash updateItem
    where updateItem _ (File b) = File <$> updateFile b
          updateItem _ (SubTree s) = SubTree <$> updateSub s
          updateItem _ x = return x

          updateFile b@(Blob _ !h) = do
            content <- liftIO $ readBlob b
            let fn = dir </> BS8.unpack (encodeBase16 h)
                nblob = Blob (decompress <$> rblob) h
                rblob = BL.fromChunks <$> return <$> BS.readFile fn
                newcontent = compress content
            fsCreateHashedFile fn newcontent
            return nblob
          updateSub s = do
            let !hash = treeHash s
                Just dirdata = darcsFormatDir s
                fn = dir </> BS8.unpack (encodeBase16 hash)
            fsCreateHashedFile fn (compress dirdata)
            return s

--------------------------------------------------------------
-- Reading and writing packed pristine. EXPERIMENTAL.
----

-- | Read a Tree in the darcs hashed format from an object storage. This is
-- basically the same as readDarcsHashed from Storage.Hashed, but uses an
-- object storage instead of traditional darcs filesystem layout. Requires the
-- tree root hash as a starting point.
readPackedDarcsPristine :: OS -> Hash -> IO (Tree IO)
readPackedDarcsPristine os root =
    do items' <- darcsParseDir <$> grab root
       subs <- sequence [
                case tp of
                  BlobType -> return (d, File $ file h)
                  TreeType -> let t = readPackedDarcsPristine os h
                               in return (d, Stub t h)
                | (tp, d, _, h) <- items' ]
       return $ makeTreeWithHash subs root
    where file h = Blob (grab h) h
          grab hash = do maybeseg <- lookup os hash
                         case maybeseg of
                           Nothing -> fail $ "hash " ++ BS8.unpack (encodeBase16 hash) ++ " not available"
                           Just seg -> readSegment seg

-- | Write a Tree into an object storage, using the darcs-style directory
-- formatting (and therefore darcs-style hashes). Gives back the object storage
-- and the root hash of the stored Tree. NB. The function expects that the Tree
-- comes equipped with darcs-style hashes already!
writePackedDarcsPristine :: Tree IO -> OS -> IO (OS, Hash)
writePackedDarcsPristine tree' os =
    do t <- darcsUpdateDirHashes <$> expand tree'
       files <- sequence [ readBlob b | (_, File b) <- list t ]
       let dirs = darcsFormatDir t : [ darcsFormatDir d | (_, SubTree d) <- list t ]
       os' <- hatch os $ files ++ (map fromJust dirs)
       return (os', darcsTreeHash t)

storePackedDarcsPristine :: Tree IO -> OS -> IO (OS, Hash)
storePackedDarcsPristine tree' os =
    do (os', root) <- writePackedDarcsPristine tree' os
       return $ (os' { roots = root : roots os'
                     -- FIXME we probably don't want to override the references
                     -- thing completely here...
                     , references = darcsPristineRefs }, root)

darcsPristineRefs :: FileSegment -> IO [Hash]
darcsPristineRefs fs = do
  con <- (darcsParseDir <$> readSegment fs) `catch` \(_ :: IOException) -> return []
  return $! [ hash | (_, _, _, hash) <- con, valid hash ]
    where valid NoHash = False
          valid _ = True

darcsCheckExpand :: Tree IO
            -> IO (Either [(FilePath, Hash, Maybe Hash)] (Tree IO))
darcsCheckExpand t = do
  problemsOrTree <- checkExpand darcsHash t
  case problemsOrTree of
    Left problems -> return . Left $ map render problems
    Right tree' -> return . Right $ tree'
  where
    render (path, h, h') = (anchorPath "." path, h, h')