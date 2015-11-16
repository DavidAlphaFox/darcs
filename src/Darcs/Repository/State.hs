{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}

-- Copyright (C) 2009 Petr Rockai
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

module Darcs.Repository.State
    ( restrictSubpaths, restrictBoring, TreeFilter(..), restrictDarcsdir
    -- * Diffs.
    , unrecordedChanges, unrecordedChangesWithPatches, readPending
    -- * Trees.
    , readRecorded, readUnrecorded, readRecordedAndPending, readWorking
    , readPendingAndWorking
    -- * Index.
    , readIndex, updateIndex, invalidateIndex, UseIndex(..), ScanKnown(..)
    -- * Utilities
    , filterOutConflicts ) where

import Prelude hiding ( filter, catch )
import Control.Monad( when )
import Control.Applicative( (<$>) )
import Control.Exception ( catch, IOException )
import Data.Maybe( isJust )
import Data.List( union )
import Text.Regex( matchRegex )

import System.Directory( removeFile, doesFileExist, doesDirectoryExist, renameFile )
import System.FilePath ( (</>) )
import qualified Data.ByteString as BS
    ( readFile, drop, writeFile, empty )
import qualified Data.ByteString.Char8 as BSC
    ( pack, split )

import Darcs.Patch ( RepoPatch, PrimOf, sortCoalesceFL, fromPrim, fromPrims
                   , effect, anonymous )
import Darcs.Patch.Apply ( ApplyState, applyToTree, effectOnFilePaths )
import Darcs.Patch.Witnesses.Ordered ( RL(..), FL(..), (+>+), mapFL_FL
                                     , (:>)(..) )
import Darcs.Patch.Witnesses.Eq ( EqCheck(IsEq, NotEq) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, unFreeLeft, mapSeal )
import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( commuterIdRL )
import Darcs.Patch.Permutations ( partitionConflictingFL )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )

import Darcs.Repository.Flags ( UseIndex(..), ScanKnown(..), DiffAlgorithm(..) )
import Darcs.Util.Global ( darcsdir )

import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository.Format(formatHas, RepoProperty(NoWorkingDir))
import qualified Darcs.Repository.LowLevel as LowLevel
import Darcs.Repository.Prefs ( filetypeFunction, boringRegexps )
import Darcs.Repository.Diff ( treeDiff )

import Darcs.Util.Path( AnchoredPath(..), anchorPath, floatPath, Name(..), fn2fp,
                   SubPath, sp2fn, filterPaths )
import Storage.Hashed.Tree( Tree, restrict, FilterTree, expand, filter, emptyTree, overlay, find )
import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Darcs( darcsTreeHash, readDarcsHashed, decodeDarcsHash, decodeDarcsSize )
import Storage.Hashed.Hash( Hash( NoHash ) )
import qualified Storage.Hashed.Index as I
import qualified Storage.Hashed.Tree as Tree


newtype TreeFilter m = TreeFilter { applyTreeFilter :: forall tr . FilterTree tr m => tr m -> tr m }

-- TODO: We wrap the pending patch inside RepoPatch here, to avoid the
-- requirement to propagate an (ApplyState (PrimOf p) ~ ApplyState p)
-- constraint everywhere. When we have GHC 7.2 as a minimum requirement, we can
-- lift this constraint into RepoPatch superclass context and remove this hack.
readPendingLL :: (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT -> IO (Sealed ((FL p) wT))
readPendingLL repo = mapSeal (mapFL_FL fromPrim) `fmap` LowLevel.readPending repo

-- | From a repository and a list of SubPath's, construct a filter that can be
-- used on a Tree (recorded or unrecorded state) of this repository. This
-- constructed filter will take pending into account, so the subpaths will be
-- translated correctly relative to pending move patches.
restrictSubpaths :: forall p m wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT -> [SubPath]
                 -> IO (TreeFilter m)
restrictSubpaths repo subpaths = do
  Sealed pending <- readPendingLL repo
  let paths = map (fn2fp . sp2fn) subpaths
      paths' = paths `union` effectOnFilePaths pending paths
      anchored = map floatPath paths'
      restrictPaths :: FilterTree tree m => tree m -> tree m
      restrictPaths = filter (filterPaths anchored)
  return (TreeFilter restrictPaths)

-- |Is the given path in (or equal to) the _darcs metadata directory?
inDarcsDir :: AnchoredPath -> Bool
inDarcsDir (AnchoredPath (Name x:_)) | x == BSC.pack darcsdir = True
inDarcsDir _ = False

-- | Construct a Tree filter that removes any boring files the Tree might have
-- contained. Additionally, you should (in most cases) pass an (expanded) Tree
-- that corresponds to the recorded content of the repository. This is
-- important in the cases when the repository contains files that would be
-- boring otherwise. (If you pass emptyTree instead, such files will simply be
-- discarded by the filter, which is usually not what you want.)
--
-- This function is most useful when you have a plain Tree corresponding to the
-- full working copy of the repository, including untracked
-- files. Cf. whatsnew, record --look-for-adds.  NB. Assumes that our CWD is
-- the repository root.
restrictBoring :: forall m . Tree m -> IO (TreeFilter m)
restrictBoring guide = do
  boring <- boringRegexps
  let boring' p | inDarcsDir p = False
      boring' p = not $ any (\rx -> isJust $ matchRegex rx p') boring
          where p' = anchorPath "" p
      restrictTree :: FilterTree t m => t m -> t m
      restrictTree = filter $ \p _ -> case find guide p of
                                        Nothing -> boring' p
                                        _ -> True
  return (TreeFilter restrictTree)

-- | Construct a Tree filter that removes any darcs metadata files the
-- Tree might have contained.
restrictDarcsdir :: forall m . TreeFilter m
restrictDarcsdir = TreeFilter $ filter $ \p _ -> not (inDarcsDir p)

-- | For a repository and an optional list of paths (when Nothing, take
-- everything) compute a (forward) list of prims (i.e. a patch) going from the
-- recorded state of the repository (pristine) to the unrecorded state of the
-- repository (the working copy + pending). When a list of paths is given, at
-- least the files that live under any of these paths in either recorded or
-- unrecorded will be included in the resulting patch. NB. More patches may be
-- included in this list, eg. the full contents of the pending patch. This is
-- usually not a problem, since selectChanges will properly filter the results
-- anyway.
--
-- This also depends on the options given: with LookForAdds, we will include
-- any non-boring files (i.e. also those that do not exist in the "recorded"
-- state) in the working in the "unrecorded" state, and therefore they will
-- show up in the patches as addfiles.
--
-- The IgnoreTimes option disables index usage completely -- for each file, we
-- read both the unrecorded and the recorded copy and run a diff on them. This
-- is very inefficient, although in extremely rare cases, the index could go
-- out of sync (file is modified, index is updated and file is modified again
-- within a single second).
unrecordedChanges :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
                  => (UseIndex, ScanKnown, DiffAlgorithm) -> Repository p wR wU wT
                  -> Maybe [SubPath] -> IO (FL (PrimOf p) wT wU)
unrecordedChanges opts r paths = do
    (pending :> working) <- readPendingAndWorking opts r paths
    return $ sortCoalesceFL (pending +>+ working)

unrecordedChangesWithPatches :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
                  => (UseIndex, ScanKnown, DiffAlgorithm) -> Repository p wR wU wT
                  -> Maybe [SubPath]
                  -> FL (PrimOf p) wX wT -- look-for-moves patches
                  -> FL (PrimOf p) wT wT -- look-for-replaces patches
                  -> IO (FL (PrimOf p) wT wU)
unrecordedChangesWithPatches opts r paths movesPs replacesPs = do
    (pending :> working) <- readPendingAndWorkingWithPatches opts r paths movesPs replacesPs
    return $ sortCoalesceFL (pending +>+ unsafeCoerceP (movesPs +>+ replacesPs) +>+ working)

-- | Mostly a helper function to 'unrecordedChangesWithPatches', returning the pending
--   patch plus `patches` and the subsequent diff from working as two different patches
readPendingAndWorkingWithPatches :: forall p wR wU wT wZ. (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree)
                                 => (UseIndex, ScanKnown, DiffAlgorithm)
                                 -> Repository p wR wU wT
                                 -> Maybe [SubPath]
                                 -> FL (PrimOf p) wZ wT  -- look-for-moves patches
                                 -> FL (PrimOf p) wT wT  -- look-for-replaces patches
                                 -> IO ((FL (PrimOf p) :> FL (PrimOf p)) wT wU)
readPendingAndWorkingWithPatches _ r@(Repo _ rf _ _) _ _ _ | (formatHas NoWorkingDir rf) = do
    IsEq <- return $ workDirLessRepoWitness r
    return (NilFL :> NilFL)
readPendingAndWorkingWithPatches (useidx', scan, dflag) repo mbpaths movesPs replacesPs = do
  let allPatches = movesPs +>+ replacesPs
  let useidx = case allPatches of
                 NilFL -> useidx'
                 _ -> IgnoreIndex
  (all_current, Sealed (pending :: FL p wT wX)) <- readPending repo
  all_current_with_patches <- applyToTree allPatches all_current

  relevant <- maybe (return $ TreeFilter id) (restrictSubpaths repo) mbpaths
  let getIndex = applyToTree movesPs =<< I.updateIndex =<< (applyTreeFilter relevant <$> readIndex repo)
      current = applyTreeFilter relevant all_current_with_patches

  index <- getIndex
  working <- applyTreeFilter restrictDarcsdir <$> case scan of
    ScanKnown -> case useidx of
      UseIndex -> getIndex
      IgnoreIndex -> do
        guide <- expand current
        applyTreeFilter relevant <$> restrict guide <$> readPlainTree "."
    ScanAll -> do
      nonboring <- restrictBoring index
      plain <- applyTreeFilter relevant <$> applyTreeFilter nonboring <$> readPlainTree "."
      return $ case useidx of
        UseIndex -> plain `overlay` index
        IgnoreIndex -> plain
    ScanBoring -> do
      plain <- applyTreeFilter relevant <$> readPlainTree "."
      return $ case useidx of
        UseIndex -> plain `overlay` index
        IgnoreIndex -> plain

  ft <- filetypeFunction
  Sealed (diff :: FL (PrimOf p) wX wY) <- (unFreeLeft `fmap` treeDiff dflag ft current working) :: IO (Sealed (FL (PrimOf p) wX))
  IsEq <- return (unsafeCoerceP IsEq) :: IO (EqCheck wY wU)
  return (effect pending :> diff)

-- | Mostly a helper function to 'unrecordedChanges', returning the pending
--   patch and the subsequent diff from working as two different patches
readPendingAndWorking :: forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
                      => (UseIndex, ScanKnown, DiffAlgorithm)
                      -> Repository p wR wU wT
                      -> Maybe [SubPath]
                      -> IO ((FL (PrimOf p) :> FL (PrimOf p)) wT wU)
readPendingAndWorking _ r@(Repo _ rf _ _) _ | (formatHas NoWorkingDir rf) = do
    IsEq <- return $ workDirLessRepoWitness r
    return (NilFL :> NilFL)
readPendingAndWorking (useidx, scan, dflag) repo mbpaths = do
  (all_current, Sealed (pending :: FL p wT wX)) <- readPending repo

  relevant <- maybe (return $ TreeFilter id) (restrictSubpaths repo) mbpaths
  let getIndex = I.updateIndex =<< (applyTreeFilter relevant <$> readIndex repo)
      current = applyTreeFilter relevant all_current

  index <- getIndex
  working <- applyTreeFilter restrictDarcsdir <$> case scan of
    ScanKnown -> case useidx of
      UseIndex -> getIndex
      IgnoreIndex -> do
        guide <- expand current
        applyTreeFilter relevant <$> restrict guide <$> readPlainTree "."
    ScanAll -> do
      nonboring <- restrictBoring index
      plain <- applyTreeFilter relevant <$> applyTreeFilter nonboring <$> readPlainTree "."
      return $ case useidx of
        UseIndex -> plain `overlay` index
        IgnoreIndex -> plain
    ScanBoring -> do
      plain <- applyTreeFilter relevant <$> readPlainTree "."
      return $ case useidx of
        UseIndex -> plain `overlay` index
        IgnoreIndex -> plain

  ft <- filetypeFunction
  Sealed (diff :: FL (PrimOf p) wX wY) <- (unFreeLeft `fmap` treeDiff dflag ft current working) :: IO (Sealed (FL (PrimOf p) wX))
  IsEq <- return (unsafeCoerceP IsEq) :: IO (EqCheck wY wU)
  return (effect pending :> diff)

-- | Witnesses the fact that in the absence of a working directory, we
-- pretend that the working dir updates magically to the tentative state.
workDirLessRepoWitness :: Repository p wR wU wT -> EqCheck wU wT
workDirLessRepoWitness (Repo _ rf _ _)
 | formatHas NoWorkingDir rf = unsafeCoerceP IsEq
 | otherwise                 = NotEq

-- | Obtains a Tree corresponding to the "recorded" state of the repository:
-- this is the same as the pristine cache, which is the same as the result of
-- applying all the repository's patches to an empty directory.
--
-- Handles the plain and hashed pristine cases. Currently does not handle the
-- no-pristine case, as that requires replaying patches. Cf. 'readDarcsHashed'
-- and 'readPlainTree' in hashed-storage that are used to do the actual 'Tree'
-- construction.
readRecorded :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO (Tree IO)
readRecorded _repo = do
  let h_inventory = darcsdir </> "hashed_inventory"
  hashed <- doesFileExist h_inventory
  if hashed
     then do inv <- BS.readFile h_inventory
             let linesInv = BSC.split '\n' inv
             case linesInv of
               [] -> return emptyTree
               (pris_line:_) -> do
                          let hash = decodeDarcsHash $ BS.drop 9 pris_line
                              size = decodeDarcsSize $ BS.drop 9 pris_line
                          when (hash == NoHash) $ fail $ "Bad pristine root: " ++ show pris_line
                          readDarcsHashed (darcsdir </> "pristine.hashed") (size, hash)
     else do have_pristine <- doesDirectoryExist $ darcsdir </> "pristine"
             have_current <- doesDirectoryExist $ darcsdir </> "current"
             case (have_pristine, have_current) of
               (True, _) -> readPlainTree $ darcsdir </> "pristine"
               (False, True) -> readPlainTree $ darcsdir </> "current"
               (_, _) -> fail "No pristine tree is available!"

-- | Obtains a Tree corresponding to the "unrecorded" state of the repository:
-- the working tree plus the "pending" patch. The optional list of paths allows
-- to restrict the query to a subtree.
--
-- Limiting the query may be more efficient, since hashes on the uninteresting
-- parts of the index do not need to go through an up-to-date check (which
-- involves a relatively expensive lstat(2) per file.
readUnrecorded :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository p wR wU wT -> Maybe [SubPath] -> IO (Tree IO)
readUnrecorded repo mbpaths = do
  relevant <- maybe (return $ TreeFilter id) (restrictSubpaths repo) mbpaths
  readIndex repo >>= I.updateIndex . applyTreeFilter relevant

-- | Obtains a Tree corresponding to the working copy of the
-- repository. NB. Almost always, using readUnrecorded is the right
-- choice. This function is only useful in not-completely-constructed
-- repositories.
readWorking :: IO (Tree IO)
readWorking = expand =<< (nodarcs `fmap` readPlainTree ".")
  where nodarcs = Tree.filter (\(AnchoredPath (Name x:_)) _ -> x /= BSC.pack darcsdir)

readRecordedAndPending :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository p wR wU wT -> IO (Tree IO)
readRecordedAndPending repo = fst `fmap` readPending repo

readPending :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository p wR wU wT -> IO (Tree IO, Sealed (FL p wT))
readPending repo =
  do Sealed pending <- readPendingLL repo
     pristine <- readRecorded repo
     catch ((\t -> (t, seal pending)) `fmap` applyToTree pending pristine) $ \ (err :: IOException) -> do
       putStrLn $ "Yikes, pending has conflicts! " ++ show err
       putStrLn "Stashing the buggy pending as _darcs/patches/pending_buggy"
       renameFile (darcsdir </> "patches" </> "pending")
                  (darcsdir </> "patches" </> "pending_buggy")
       return (pristine, seal NilFL)

-- | Mark the existing index as invalid. This has to be called whenever the
-- listing of pristine changes and will cause darcs to update the index next
-- time it tries to read it. (NB. This is about files added and removed from
-- pristine: changes to file content in either pristine or working are handled
-- transparently by the index reading code.)
invalidateIndex :: t -> IO ()
invalidateIndex _ = BS.writeFile (darcsdir </> "index_invalid") BS.empty

readIndex :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO I.Index
readIndex repo = do
  invalid <- doesFileExist $ darcsdir </> "index_invalid"
  exists <- doesFileExist $ darcsdir </> "index"
  formatValid <- if exists
                     then I.indexFormatValid $ darcsdir </> "index"
                     else return True
  when (exists && not formatValid) $
#if mingw32_HOST_OS
       renameFile (darcsdir </> "index") (darcsdir </> "index.old")
#else
       removeFile $ darcsdir </> "index"
#endif
  if not exists || invalid || not formatValid
     then do pris <- readRecordedAndPending repo
             idx <- I.updateIndexFrom (darcsdir </> "index") darcsTreeHash pris
             when invalid $ removeFile $ darcsdir </> "index_invalid"
             return idx
     else I.readIndex (darcsdir </> "index") darcsTreeHash

updateIndex :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
updateIndex repo = do
    invalid <- doesFileExist $ darcsdir </> "index_invalid"
    exists <- doesFileExist $ darcsdir </> "index"
    formatValid <- if exists
                     then I.indexFormatValid $ darcsdir </> "index"
                     else return True
    when (exists && not formatValid) $
#if mingw32_HOST_OS
       renameFile (darcsdir </> "index") (darcsdir </> "index.old")
#else
       removeFile $ darcsdir </> "index"
#endif
    pris <- readRecordedAndPending repo
    _ <- I.updateIndexFrom (darcsdir </> "index") darcsTreeHash pris
    when invalid $ removeFile $ darcsdir </> "index_invalid"

-- |Remove any patches (+dependencies) from a sequence that
-- conflict with the recorded or unrecorded changes in a repo
filterOutConflicts :: (RepoPatch p, ApplyState p ~ Tree)
     => RL (PatchInfoAnd p) wX wT                  -- ^Recorded patches from repository, starting from
                                                   -- same context as the patches to filter
     -> Repository p wR wU wT                      -- ^Repository itself, used for grabbing unrecorded changes
     -> FL (PatchInfoAnd p) wX wZ                  -- ^Patches to filter
     -> IO (Bool, Sealed (FL (PatchInfoAnd p) wX)) -- ^(True iff any patches were removed, possibly filtered patches)
filterOutConflicts us repository them
     = do let commuter = commuterIdRL selfCommuter
          unrec <- fmap n2pia . anonymous . fromPrims
                     =<< unrecordedChanges (UseIndex, ScanKnown, MyersDiff) repository Nothing
          them' :> rest <- return $ partitionConflictingFL commuter them (unrec :<: us)
          return (check rest, Sealed them')
  where check :: FL p wA wB -> Bool
        check NilFL = False
        check _ = True
