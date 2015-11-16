{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeSynonymInstances, UndecidableInstances, FlexibleInstances #-}

-- | An experimental monadic interface to Tree mutation. The main idea is to
-- simulate IO-ish manipulation of real filesystem (that's the state part of
-- the monad), and to keep memory usage down by reasonably often dumping the
-- intermediate data to disk and forgetting it. The monad interface itself is
-- generic, and a number of actual implementations can be used. This module
-- provides just 'virtualTreeIO' that never writes any changes, but may trigger
-- filesystem reads as appropriate.
module Storage.Hashed.Monad
    ( virtualTreeIO, virtualTreeMonad
    , readFile, writeFile, createDirectory, rename, copy, unlink
    , fileExists, directoryExists, exists, withDirectory
    , currentDirectory
    , tree, TreeState, TreeMonad, TreeIO, runTreeMonad
    , initialState, replaceItem
    , findM, findFileM, findTreeM
    , TreeRO, TreeRW
    ) where

import Prelude hiding ( readFile, writeFile )

import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree

import Control.Applicative( (<$>) )

import Data.List( sortBy )
import Data.Int( Int64 )
import Data.Maybe( isNothing, isJust )

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.RWS.Strict
import qualified Data.Map as M

type Changed = M.Map AnchoredPath (Int64, Int64) -- size, age

-- | Internal state of the 'TreeIO' monad. Keeps track of the current Tree
-- content, unsync'd changes and a current working directory (of the monad).
data TreeState m = TreeState { tree :: !(Tree m)
                             , changed :: !Changed
                             , changesize :: !Int64
                             , maxage :: !Int64
                             , updateHash :: TreeItem m -> m Hash
                             , update :: AnchoredPath -> TreeItem m -> TreeMonad m (TreeItem m) }

-- | A 'TreeIO' monad. A sort of like IO but it keeps a 'TreeState' around as well,
-- which is a sort of virtual filesystem. Depending on how you obtained your
-- 'TreeIO', the actions in your virtual filesystem get somehow reflected in the
-- actual real filesystem. For 'virtualTreeIO', nothing happens in real
-- filesystem, however with 'plainTreeIO', the plain tree will be updated every
-- now and then, and with 'hashedTreeIO' a darcs-style hashed tree will get
-- updated.
type TreeMonad m = RWST AnchoredPath () (TreeState m) m
type TreeIO = TreeMonad IO

class (Functor m, Monad m) => TreeRO m where
    currentDirectory :: m AnchoredPath
    withDirectory :: AnchoredPath -> m a -> m a
    expandTo :: AnchoredPath -> m AnchoredPath
    -- | Grab content of a file in the current Tree at the given path.
    readFile :: AnchoredPath -> m BL.ByteString
    -- | Check for existence of a node (file or directory, doesn't matter).
    exists :: AnchoredPath -> m Bool
    -- | Check for existence of a directory.
    directoryExists ::AnchoredPath -> m Bool
    -- | Check for existence of a file.
    fileExists :: AnchoredPath -> m Bool

class TreeRO m => TreeRW m where
    -- | Change content of a file at a given path. The change will be
    -- eventually flushed to disk, but might be buffered for some time.
    writeFile :: AnchoredPath -> BL.ByteString -> m ()
    createDirectory :: AnchoredPath -> m ()
    unlink :: AnchoredPath -> m ()
    rename :: AnchoredPath -> AnchoredPath -> m ()
    copy   :: AnchoredPath -> AnchoredPath -> m ()

initialState :: Tree m -> (TreeItem m -> m Hash)
                -> (AnchoredPath -> TreeItem m -> TreeMonad m (TreeItem m)) -> TreeState m
initialState t uh u = TreeState { tree = t
                                , changed = M.empty
                                , changesize = 0
                                , updateHash = uh
                                , maxage = 0
                                , update = u }

flush :: (Functor m, Monad m) => TreeMonad m ()
flush = do changed' <- map fst <$> M.toList <$> gets changed
           dirs' <- gets tree >>= \t -> return [ path | (path, SubTree _) <- list t ]
           modify $ \st -> st { changed = M.empty, changesize = 0 }
           forM_ (changed' ++ dirs' ++ [AnchoredPath []]) flushItem

runTreeMonad' :: (Functor m, Monad m) => TreeMonad m a -> TreeState m -> m (a, Tree m)
runTreeMonad' action initial = do
  (out, final, _) <- runRWST action (AnchoredPath []) initial
  return (out, tree final)

runTreeMonad :: (Functor m, Monad m) => TreeMonad m a -> TreeState m -> m (a, Tree m)
runTreeMonad action initial = do
  let action' = do x <- action
                   flush
                   return x
  runTreeMonad' action' initial

-- | Run a TreeIO action without storing any changes. This is useful for
-- running monadic tree mutations for obtaining the resulting Tree (as opposed
-- to their effect of writing a modified tree to disk). The actions can do both
-- read and write -- reads are passed through to the actual filesystem, but the
-- writes are held in memory in a form of modified Tree.
virtualTreeMonad :: (Functor m, Monad m) => TreeMonad m a -> Tree m -> m (a, Tree m)
virtualTreeMonad action t = runTreeMonad' action $
                               initialState t (\_ -> return NoHash) (\_ x -> return x)

virtualTreeIO :: TreeIO a -> Tree IO -> IO (a, Tree IO)
virtualTreeIO = virtualTreeMonad

-- | Modifies an item in the current Tree. This action keeps an account of the
-- modified data, in changed and changesize, for subsequent flush
-- operations. Any modifications (as in "modifyTree") are allowed.
modifyItem :: (Functor m, Monad m)
            => AnchoredPath -> Maybe (TreeItem m) -> TreeMonad m ()
modifyItem path item = do
  path' <- (`catPaths` path) `fmap` currentDirectory
  age <- gets maxage
  changed' <- gets changed
  let getsize (Just (File b)) = lift (BL.length `fmap` readBlob b)
      getsize _ = return 0
  size <- getsize item
  let change = case M.lookup path' changed' of
        Nothing -> size
        Just (oldsize, _) -> size - oldsize

  modify $ \st -> st { tree = modifyTree (tree st) path' item
                     , changed = M.insert path' (size, age) (changed st)
                     , maxage = age + 1
                     , changesize = (changesize st + change) }

renameChanged :: (Functor m, Monad m)
               => AnchoredPath -> AnchoredPath -> TreeMonad m ()
renameChanged from to = modify $ \st -> st { changed = rename' $ changed st }
  where rename' = M.fromList . map renameone . M.toList
        renameone (x, d) | from `isPrefix` x = (to `catPaths` relative from x, d)
                         | otherwise = (x, d)
        relative (AnchoredPath from') (AnchoredPath x) = AnchoredPath $ drop (length from') x

-- | Replace an item with a new version without modifying the content of the
-- tree. This does not do any change tracking. Ought to be only used from a
-- 'sync' implementation for a particular storage format. The presumed use-case
-- is that an existing in-memory Blob is replaced with a one referring to an
-- on-disk file.
replaceItem :: (Functor m, Monad m)
            => AnchoredPath -> Maybe (TreeItem m) -> TreeMonad m ()
replaceItem path item = do
  path' <- (`catPaths` path) `fmap` currentDirectory
  modify $ \st -> st { tree = modifyTree (tree st) path' item }

flushItem :: forall m. (Monad m, Functor m) => AnchoredPath -> TreeMonad m ()
flushItem path =
  do current <- gets tree
     case find current path of
       Nothing -> return () -- vanished, do nothing
       Just x -> do y <- fixHash x
                    new <- gets update >>= ($ y) . ($ path)
                    replaceItem path (Just new)
    where fixHash :: TreeItem m -> TreeMonad m (TreeItem m)
          fixHash f@(File (Blob con NoHash)) = do
            hash <- gets updateHash >>= \x -> lift $ x f
            return $ File $ Blob con hash
          fixHash (SubTree s) | treeHash s == NoHash =
            gets updateHash >>= \f -> SubTree <$> lift (addMissingHashes f s)
          fixHash x = return x


-- | If buffers are becoming large, sync, otherwise do nothing.
flushSome :: (Monad m, Functor m) => TreeMonad m ()
flushSome = do x <- gets changesize
               when (x > megs 100) $ do
                 remaining <- go =<< sortBy age <$> M.toList <$> gets changed
                 modify $ \s -> s { changed = M.fromList remaining }
  where go [] = return []
        go ((path, (size, _)):chs) = do
          x <- (\s -> s - size) <$> gets changesize
          flushItem path
          modify $ \s -> s { changesize = x }
          if (x > megs 50) then go chs
                           else return $ chs
        megs = (* (1024 * 1024))
        age (_, (_, a)) (_, (_, b)) = compare a b

instance (Functor m, Monad m) => TreeRO (TreeMonad m) where
    expandTo p =
        do t <- gets tree
           p' <- (`catPaths` p) `fmap` ask
           t' <- lift $ expandPath t p'
           modify $ \st -> st { tree = t' }
           return p'

    fileExists p =
        do p' <- expandTo p
           (isJust . (flip findFile p')) `fmap` gets tree

    directoryExists p =
        do p' <- expandTo p
           (isJust . (flip findTree p')) `fmap` gets tree

    exists p =
        do p' <- expandTo p
           (isJust . (flip find p')) `fmap` gets tree

    readFile p =
        do p' <- expandTo p
           t <- gets tree
           let f = findFile t p'
           case f of
             Nothing -> fail $ "No such file " ++ show p'
             Just x -> lift (readBlob x)

    currentDirectory = ask
    withDirectory dir act = do
      dir' <- expandTo dir
      local (\_ -> dir') act

instance (Functor m, Monad m) => TreeRW (TreeMonad m) where
    writeFile p con =
        do _ <- expandTo p
           modifyItem p (Just blob)
           flushSome
        where blob = File $ Blob (return con) hash
              hash = NoHash -- we would like to say "sha256 con" here, but due
                            -- to strictness of Hash in Blob, this would often
                            -- lead to unnecessary computation which would then
                            -- be discarded anyway; we rely on the sync
                            -- implementation to fix up any NoHash occurrences

    createDirectory p =
        do _ <- expandTo p
           modifyItem p $ Just $ SubTree emptyTree

    unlink p =
        do _ <- expandTo p
           modifyItem p Nothing

    rename from to =
        do from' <- expandTo from
           to' <- expandTo to
           tr <- gets tree
           let item = find tr from'
               found_to = find tr to'
           unless (isNothing found_to) $
                  fail $ "Error renaming: destination " ++ show to ++ " exists."
           unless (isNothing item) $ do
                  modifyItem from Nothing
                  modifyItem to item
                  renameChanged from to

    copy from to =
        do from' <- expandTo from
           _ <- expandTo to
           tr <- gets tree
           let item = find tr from'
           unless (isNothing item) $ modifyItem to item

findM' :: forall m a. (Monad m, Functor m)
       => (Tree m -> AnchoredPath -> a) -> Tree m -> AnchoredPath -> m a
findM' what t path = fst <$> virtualTreeMonad (look path) t
  where look :: AnchoredPath -> TreeMonad m a
        look = expandTo >=> \p' -> flip what p' <$> gets tree

findM :: (Monad m, Functor m) => Tree m -> AnchoredPath -> m (Maybe (TreeItem m))
findM = findM' find

findTreeM :: (Monad m, Functor m) => Tree m -> AnchoredPath -> m (Maybe (Tree m))
findTreeM = findM' findTree

findFileM :: (Monad m, Functor m) => Tree m -> AnchoredPath -> m (Maybe (Blob m))
findFileM = findM' findFile
