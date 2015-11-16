-- Copyright (C) 2013 Jose Neder
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE CPP #-}
module Darcs.Repository.Util
    ( getReplaces
    , floatSubPath
    , maybeApplyToTree
    , defaultToks
    , getMovesPs
    , patchSetfMap
    , getRecursiveDarcsRepos
    ) where

import Prelude hiding ( catch )
import Control.Applicative ( (<$>) )
import Control.Monad ( foldM, forM )
import Control.Exception ( catch, IOException )
import qualified Data.ByteString as B ( null, concat )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.ByteString.Lazy as BL ( toChunks )
import Data.Maybe ( isJust, fromJust, catMaybes )
import Data.Ord ( comparing )
import Data.List ( sortBy )
#ifdef USE_LOCAL_DATA_MAP_STRICT
import qualified Darcs.Data.Map.Strict as M ( Map, lookup, fromList, insert, map,
                                        empty, assocs, size, findWithDefault, delete )
#else
import qualified Data.Map.Strict as M ( Map, lookup, fromList, insert, map,
                                        empty, assocs, size, findWithDefault, delete )
#endif

import Storage.Hashed( floatPath, readPlainTree )
import Storage.Hashed.Tree ( Tree, emptyTree, expand, ItemType(..), itemType,
                             readBlob, modifyTree, findFile, TreeItem(..),
                             makeBlobBS, expandPath )
import Storage.Hashed.AnchoredPath ( AnchoredPath, anchorPath, parents,
                                     replacePrefixPath, anchoredRoot )
import qualified Storage.Hashed.Tree as T ( list )
import Storage.Hashed.Index ( listFileIDs, getFileID )
import System.Posix.Types ( FileID )
import System.Directory ( getDirectoryContents, doesDirectoryExist )
import System.FilePath.Posix ( (</>) )
import Darcs.Patch ( RepoPatch, PrimPatch, PrimOf, primIsHunk, applyToTree,
                     tokreplace, forceTokReplace, move )
import Darcs.Patch.Set ( newset2RL, PatchSet(..) )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Patchy ( Apply )
import Darcs.Patch.Prim.V1.Core ( FilePatchType( Hunk ), Prim(..) )
import Darcs.Patch.Prim.Class ( PrimConstruct, PrimCanonize )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Patch.TokenReplace ( breakOutToken )
import Darcs.Patch.Witnesses.Ordered ( FL(..), reverseRL, reverseFL, (:>)(..),
                                       foldlFL, concatFL, toFL, (+>+), mapRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft, mapSeal, freeGap,
                                      emptyGap, joinGap, FreeLeft, Gap(..) )
import Darcs.Repository
    ( Repository
    , readUnrecorded
    , readRecordedAndPending
    , maybeIdentifyRepository
    )
import Darcs.Repository.Internal ( IdentifyRepo(..) )
import Darcs.Repository.InternalTypes ( Repository(..), Pristine(..) )
import Darcs.Repository.Diff( treeDiff )
import Darcs.Repository.Flags ( UseIndex(..), ScanKnown, DiffAlgorithm(..), UseCache(..) )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.State ( TreeFilter(..), applyTreeFilter,
                                restrictSubpaths, readWorking, restrictBoring,
                                readIndex )
import Darcs.Util.Path( fn2fp, SubPath, toFilePath, simpleSubPath, normPath,
                        floatSubPath )

getMovesPs :: forall p wR wU wB prim.
              (PrimConstruct prim, PrimCanonize prim, RepoPatch p,
               ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
           => Repository p wR wU wR
           -> Maybe [SubPath]
           -> IO (FL prim wB wB)
getMovesPs repository files = mkMovesFL <$> getMovedFiles repository files
  where
    mkMovesFL [] = NilFL
    mkMovesFL ((a,b,_):xs) = move (anchorPath "" a) (anchorPath "" b) :>: mkMovesFL xs

    getMovedFiles :: (RepoPatch p, ApplyState p ~ Tree,
                              ApplyState (PrimOf p) ~ Tree) =>
                     Repository p wR wU wR ->
                     Maybe [SubPath] ->
                     IO [(AnchoredPath, AnchoredPath, ItemType)]
    getMovedFiles repo fs = do
        old <- sortBy (comparing snd) <$> (listFileIDs =<< readIndex repo)
        nonboring <- restrictBoring emptyTree
        new <- sortBy (comparing snd) <$>
                 (addFileIDs . (map (\(a,b) -> (a, itemType b)) . T.list)  =<<
                   expand =<<
                   applyTreeFilter nonboring <$> readPlainTree ".")
        let movedfiles = matchFileLists old new
            fmovedfiles = case fs of
                            Nothing -> movedfiles
                            Just subpath -> filter (\(old',new',_) -> old' `elem` selfiles
                                                                   || new' `elem` selfiles) movedfiles
                                               where selfiles = map (floatPath . toFilePath) subpath
        return (resolveMoves fmovedfiles)

    resolveMoves :: [(AnchoredPath, AnchoredPath, ItemType)]
                 -> [(AnchoredPath, AnchoredPath, ItemType)]
    resolveMoves xs = changePaths $ resolveDeps 0 (M.size movesMap) visited movesMap movesDepsMap
      where
        changePaths [] = []
        changePaths (y:ys) | fst' y == snd' y = changePaths $ map replacepp ys
                           | isPath y = y:changePaths (map replacepp ys)
                           | otherwise = y:changePaths ys
            where replacepp i | nfst == anchoredRoot = i
                              | otherwise = (nfst, snd' i, thd' i)
                      where nfst = replacePrefixPath (fst' y) (snd' y) (fst' i)

        -- sort and index moves
        movesMap = M.fromList $ zip [0..] $ sortBy (comparing thd') xs

        movesIDMap :: M.Map (AnchoredPath,AnchoredPath,ItemType) Int
        movesIDMap = M.fromList $ zip (sortBy (comparing thd') $ xs) [0..]

        -- establish a relation of dependencies between moves (destination or parent of destination is moved again)
        movesDepsMap :: M.Map Int [Int]
        movesDepsMap = M.map (getMoveDeps (M.fromList (map (\x -> (fst' x,x)) xs))
                                          (M.fromList (map (\x -> (snd' x,x)) xs))) movesMap

        getMoveDeps :: M.Map AnchoredPath (AnchoredPath, AnchoredPath, ItemType) -- source to move
                    -> M.Map AnchoredPath (AnchoredPath, AnchoredPath, ItemType) -- destin to move
                    -> (AnchoredPath, AnchoredPath, ItemType)                    -- some move
                    -> [Int]
        getMoveDeps am bm y = catMaybes $
                                map (`M.lookup` movesIDMap) $ -- retrieve mode ID of deps
                                  catMaybes $
                                    byname ++ map (`M.lookup` bm) (parents $ snd' y) -- see if current move is moved to moved dir
                            where byname | fst' y == snd' y = []
                                         | otherwise = [M.lookup (snd' y) am] -- see if current move is moved again

        fst' (a,_,_) = a
        snd' (_,a,_) = a
        thd' (_,_,a) = a

        resolveDeps :: Int -> Int -> M.Map Int (Int,Bool)
                    -> M.Map Int (AnchoredPath, AnchoredPath, ItemType)
                    -> M.Map Int [Int]
                    -> [(AnchoredPath, AnchoredPath, ItemType)]
        resolveDeps n end v mm mdm
          | n == end = reverse $
                               catMaybes $
                                 map (flip M.lookup mm . abs) $
                                   getMoves (map fst (filter (\(_,(_,f)) -> f) $
                                     sortBy (comparing (fst . snd)) (M.assocs v))) mdm
          | M.lookup n v /= Nothing = resolveDeps (n+1) end v mm mdm
          | otherwise = resolveDeps (n+1) end nv nmm nmdm
                    where (nv, nmm, nmdm) = walk True n n v mm mdm

        getMoves [] _ = []
        getMoves (r:roots) mdm = [r]++bds r++getMoves roots mdm
            where bds r' = lookupList r' mdm ++ concatMap bds (map abs $ lookupList r' mdm)

        lookupList x mdm = M.findWithDefault [] x mdm

        walk b n x v mm mdm
          | x < 0 = (v, mm, mdm)
          | Just n == (fst <$> M.lookup x v) = resolveClashName n x v mm mdm
          | otherwise = foldl (\(v',mm', mdm') dep ->
                                  walk False n dep v' mm' mdm')
                              (M.insert x (n,b) v, mm, mdm)
                              (lookupList x mdm)

        -- Ignore swap moves
        -- Currently, handling them would involve introducing intermediate file names.
        -- When darcs has swapmove primitive hunk we may fix this.
        resolveClashName n x v mm mdm = (v', mm', mdm')
                  where v' = M.insert x (n,False) $
                             foldl addvisited v (lookupList x mdm)
                        mm' = M.delete x mm  -- forget about x
                        mdm' = M.insert x [] mdm   -- remove dependencies for x
                        addvisited nv k | (fst <$> M.lookup k nv) /= Just n = foldl addvisited (M.insert k (n, False) nv) (lookupList k mdm)
                                        | otherwise = nv

        visited = M.empty :: M.Map Int (Int, Bool)

        isPath (_, _, TreeType) = True
        isPath _ = False

    addFileIDs :: [(AnchoredPath, ItemType)] -> IO [((AnchoredPath, ItemType),FileID)]
    addFileIDs = foldM (\xs (apath, it)-> do fid <- getFileID apath
                                             return $ case fid of
                                                        Nothing -> xs
                                                        Just fileid -> ((apath, it), fileid):xs) []

    matchFileLists :: [((AnchoredPath, ItemType),FileID)]
                   -> [((AnchoredPath, ItemType),FileID)]
                   -> [(AnchoredPath, AnchoredPath, ItemType)]
    matchFileLists [] _ = []
    matchFileLists _ [] = []
    matchFileLists (x:xs) (y:ys) | snd x > snd y = matchFileLists (x:xs) ys
                                 | snd x < snd y = matchFileLists xs (y:ys)
                                 | snd (fst x) /= snd (fst y) = matchFileLists xs ys
                                 | otherwise = (fst (fst x), fst (fst y), snd (fst x)):matchFileLists xs ys


-- | Search for possible replaces between the recordedAndPending state
-- and the unrecorded (or working) state. Return a Sealed FL list of
-- replace patches to be applied to the recordedAndPending state.
getReplaces :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree,
                          ApplyState (PrimOf p) ~ Tree, wX ~ wR)
                       => (UseIndex, ScanKnown, DiffAlgorithm)
                       -> Repository p wR wU wT
                       -> Maybe [SubPath]
                       -> IO (Sealed (FL (PrimOf p) wX))
getReplaces (useindex, _, dopts) repo files = do
    relevant <- maybe (return $ TreeFilter id) (restrictSubpaths repo) files
    working <- applyTreeFilter relevant <$> case useindex of
                  UseIndex -> readUnrecorded repo Nothing
                  IgnoreIndex -> readWorking
    pending <- applyTreeFilter relevant <$> readRecordedAndPending repo
    ftf <- filetypeFunction

    Sealed changes <- unFreeLeft <$> treeDiff dopts ftf pending working
    _ :> hunks <- return $ partitionRL primIsHunk $ reverseFL changes
    let unfilteredReplaces =  foldlFL modifiedTokens [] (reverseRL hunks)
        replaces = filterInvalidReplaces unfilteredReplaces
    mapSeal concatFL . toFL <$>
        mapM (\(f,a,b) -> doReplace defaultToks pending
                            (fromJust $ simpleSubPath $ fn2fp $ normPath f)
                            (BC.unpack a) (BC.unpack b)) replaces
  where -- get individual tokens that have been modified
        modifiedTokens xs (FP f (Hunk _ old new)) =
          (map (\(a,b) -> (f, a, b)) $ concatMap checkForReplaces $
             filter (\(a,b) -> length a == length b)
                  $ zip (map breakToTokens old) (map breakToTokens new)) ++xs
        modifiedTokens _ _ = error "modifiedTokens: Not Hunk patch"

        -- from a pair of token lists, create a pair of modified token lists
        checkForReplaces ([],[]) = []
        checkForReplaces ((a:as),(b:bs)) | a == b = checkForReplaces (as,bs)
                                         | otherwise = (a,b):checkForReplaces (as,bs)
        checkForReplaces _ = error "checkForReplaces: Lists are not of the same length"

        -- keep tokens that have been consistently replaced
        filterInvalidReplaces [] = []
        filterInvalidReplaces ((f,old,new):rs)
          | any (\(f',a,b) -> f' == f && old == a && b /= new) rs =
              filterInvalidReplaces $ filter (\(f'',a',_) -> f'' == f && a' /= old) rs
        filterInvalidReplaces (r:rs) = r:filterInvalidReplaces (filter (/=r) rs)

        -- break a single bytestring into tokens
        breakToTokens input | B.null input = []
        breakToTokens input =
          let (_, tok, remaining) = breakOutToken defaultToks input in
            tok : breakToTokens remaining

        doReplace toks pend f old new = do
            let maybeReplace p = isJust <$> maybeApplyToTree replacePatch p
            pendReplaced <- maybeReplace pend
            if pendReplaced
                then return $ joinGap (:>:) (freeGap replacePatch) gapNilFL
                else getForceReplace f toks pend old new
          where
            gapNilFL = emptyGap NilFL
            fp = toFilePath f
            replacePatch = tokreplace fp toks old new

        getForceReplace :: PrimPatch prim => SubPath -> String -> Tree IO -> String -> String
                        -> IO (FreeLeft (FL prim))
        getForceReplace f toks tree old new = do
            let path = floatSubPath f
            -- It would be nice if we could fuse the two traversals here, that is,
            -- expandPath and findFile. OTOH it is debatable whether adding a new
            -- effectful version of findFile to Storage.Hashed.Tree is justified.
            expandedTree <- expandPath tree path
            content <- case findFile expandedTree path of
              Just blob -> readBlob blob
              Nothing -> do
                error $ "getForceReplace: not in tree: " ++ show path
            let newcontent = forceTokReplace toks (BC.pack new) (BC.pack old)
                                (B.concat $ BL.toChunks content)
                tree' = modifyTree expandedTree path . Just . File $ makeBlobBS newcontent
            ftf <- filetypeFunction
            normaliseNewTokPatch <- treeDiff dopts ftf expandedTree tree'
            return . joinGap (+>+) normaliseNewTokPatch $ freeGap $
                tokreplace (toFilePath f) toks old new :>: NilFL


maybeApplyToTree :: (Apply p, ApplyState p ~ Tree) => p wX wY -> Tree IO
                 -> IO (Maybe (Tree IO))
maybeApplyToTree patch tree =
    (Just `fmap` applyToTree patch tree) `catch` (\(_ :: IOException) -> return Nothing)

patchSetfMap:: (forall wW wZ . PatchInfoAnd p wW wZ -> IO a) -> PatchSet p wW' wZ' -> IO [a]
patchSetfMap f = sequence . mapRL f . newset2RL

defaultToks :: String
defaultToks = "A-Za-z_0-9"

-- |getRecursiveDarcsRepos returns all paths to repositories under topdir.
getRecursiveDarcsRepos :: FilePath -> IO [FilePath]
getRecursiveDarcsRepos topdir = do
  isDir <- doesDirectoryExist topdir
  if isDir
    then do
      status <- maybeIdentifyRepository NoUseCache topdir
      case status of
        GoodRepository (Repo _ _ pris _)  ->
                                case pris of
                                  HashedPristine -> return [topdir]
                                  _ -> return [] -- old fashioned or broken repo
        _                 -> getRecursiveDarcsRepos' topdir
    else return []

  where
    getRecursiveDarcsRepos' d = do
      names <- getDirectoryContents d
      let properNames = filter (\x -> head x /= '.') names
      paths <- forM properNames $ \name -> do
        let path = d </> name
        getRecursiveDarcsRepos path
      return (concat paths)
