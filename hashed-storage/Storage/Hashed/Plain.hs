-- | The plain format implementation resides in this module. The plain format
-- does not use any hashing and basically just wraps a normal filesystem tree
-- in the hashed-storage API.
--
-- NB. The 'read' function on Blobs coming from a plain tree is susceptible to
-- file content changes. Since we use mmap in 'read', this will break
-- referential transparency and produce unexpected results. Please always make
-- sure that all parallel access to the underlying filesystem tree never
-- mutates files. Unlink + recreate is fine though (in other words, the
-- 'writePlainTree' and 'plainTreeIO' implemented in this module are safe in
-- this respect).
module Storage.Hashed.Plain( readPlainTree, writePlainTree,
                             plainTreeIO  -- (obsolete?  if so remove implementation!)
                           ) where

import Data.Maybe( catMaybes )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import System.FilePath( (</>) )
import System.Directory( getDirectoryContents
                       , createDirectoryIfMissing )
import Bundled.Posix( getFileStatus, isDirectory, isRegularFile, FileStatus )

import Storage.Hashed.AnchoredPath
import Storage.Hashed.Utils
import Storage.Hashed.Hash( Hash( NoHash) )
import Storage.Hashed.Tree( Tree(), TreeItem(..)
                          , Blob(..), makeTree
                          , list, readBlob, expand )
import Storage.Hashed.Monad( TreeIO, runTreeMonad, initialState )
import Control.Monad.State( liftIO )

readPlainDir :: FilePath -> IO [(FilePath, FileStatus)]
readPlainDir dir =
    withCurrentDirectory dir $ do
      items <- getDirectoryContents "."
      sequence [ do st <- getFileStatus s
                    return (s, st)
                 | s <- items, s `notElem` [ ".", ".." ] ]

readPlainTree :: FilePath -> IO (Tree IO)
readPlainTree dir = do
  items <- readPlainDir dir
  let subs = catMaybes [
       let name = Name (BS8.pack name')
        in case status of
             _ | isDirectory status -> Just (name, Stub (readPlainTree (dir </> name')) NoHash)
             _ | isRegularFile status -> Just (name, File $ Blob (readBlob' name) NoHash)
             _ -> Nothing
            | (name', status) <- items ]
  return $ makeTree subs
    where readBlob' (Name name) = readSegment (dir </> BS8.unpack name, Nothing)

-- | Write out /full/ tree to a plain directory structure. If you instead want
-- to make incremental updates, refer to "Storage.Hashed.Monad".
writePlainTree :: Tree IO -> FilePath -> IO ()
writePlainTree t dir = do
  createDirectoryIfMissing True dir
  expand t >>= mapM_ write . list
    where write (p, File b) = write' p b
          write (p, SubTree _) =
              createDirectoryIfMissing True (anchorPath dir p)
          write _ = return ()
          write' p b = readBlob b >>= BL.writeFile (anchorPath dir p)

-- | Run a 'TreeIO' action in a plain tree setting. Writes out changes to the
-- plain tree every now and then (after the action is finished, the last tree
-- state is always flushed to disk). XXX Modify the tree with filesystem
-- reading and put it back into st (ie. replace the in-memory Blobs with normal
-- ones, so the memory can be GCd).
plainTreeIO :: TreeIO a -> Tree IO -> FilePath -> IO (a, Tree IO)
plainTreeIO action t _ = runTreeMonad action $ initialState t (\_ -> return NoHash) updatePlain
    where updatePlain path (File b) =
            do liftIO $ createDirectoryIfMissing True (anchorPath "" $ parent path)
               liftIO $ readBlob b >>= BL.writeFile (anchorPath "" path)
               return $ File $ Blob (BL.readFile $ anchorPath "" path) NoHash
          updatePlain _ x = return x

