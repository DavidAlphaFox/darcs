{-# LANGUAGE CPP, PatternGuards #-}

module Darcs.Repository.Repair ( replayRepository, checkIndex,
                                 replayRepositoryInTemp,
                                 RepositoryConsistency(..) )
       where

import Prelude hiding ( catch )

import Control.Monad ( when, unless )
import Control.Monad.Trans ( liftIO )
import Control.Applicative( (<$>) )
import Control.Exception ( catch, finally, IOException )
import Data.Maybe ( catMaybes )
import Data.List ( sort, (\\) )
import System.Directory ( createDirectoryIfMissing, getCurrentDirectory,
                          setCurrentDirectory )
import System.FilePath ( (</>) )
import Darcs.Util.Path( anchorPath, AbsolutePath, ioAbsolute, toFilePath )
import Darcs.Patch.PatchInfoAnd ( hopefully, PatchInfoAnd, info, winfo, WPatchInfo, unWPatchInfo, compareWPatchInfo )

import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), lengthFL, reverseFL,
    mapRL, nullFL, (:||:)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), Sealed(..), unFreeLeft )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Repair ( Repair(applyAndTryToFix) )
import Darcs.Patch.Info ( showPatchInfoUI )
import Darcs.Patch.Set ( Origin, PatchSet(..), newset2FL, newset2RL )
import Darcs.Patch ( RepoPatch, PrimOf, isInconsistent )
import Darcs.Patch.Named ( patchcontents )

import Darcs.Repository.Flags
    ( Verbosity(..), Compression, DiffAlgorithm )
import Darcs.Repository.Format ( identifyRepoFormat,
                                 RepoProperty ( HashedInventory ), formatHas )
import Darcs.Repository.Cache ( HashedDir( HashedPristineDir ) )
import Darcs.Repository.HashedIO ( cleanHashdir )
import Darcs.Repository.HashedRepo ( readHashedPristineRoot, writeAndReadPatch )
import Darcs.Repository.InternalTypes ( Repository(..), extractCache )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.Internal ( readRepo )
import Darcs.Repository.State
    ( readRecorded
    , readIndex
    , readRecordedAndPending
    )
import Darcs.Repository.Diff( treeDiff )

import Darcs.Util.Progress ( debugMessage, beginTedious, endTedious, tediousSize, finishedOneIO )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Global ( darcsdir )
import Darcs.Repository.Lock( rmRecursive, withTempDir )
import Darcs.Util.Printer ( Doc, putDocLn, text, RenderMode(..) )
import Darcs.Util.Printer.Color ( showDoc )

import Storage.Hashed.Monad( TreeIO )
import Storage.Hashed.Darcs( darcsUpdateHashes, hashedTreeIO )
import Storage.Hashed.Hash( Hash(NoHash), encodeBase16 )
import Storage.Hashed.Tree( Tree, emptyTree, list, restrict, expand, itemHash, zipTrees )
import Storage.Hashed.Index( updateIndex )
import Storage.Hashed( readPlainTree )

import qualified Data.ByteString.Char8 as BS

#include "impossible.h"

replaceInFL :: FL (PatchInfoAnd a) wX wY
            -> [Sealed2 (WPatchInfo :||: PatchInfoAnd a)]
            -> FL (PatchInfoAnd a) wX wY
replaceInFL orig [] = orig
replaceInFL NilFL _ = impossible
replaceInFL (o:>:orig) ch@(Sealed2 (o':||:c):ch_rest)
    | IsEq <- winfo o `compareWPatchInfo` o' = c:>:replaceInFL orig ch_rest
    | otherwise = o:>:replaceInFL orig ch

applyAndFix :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
            => Repository p wR wU wT -> Compression -> FL (PatchInfoAnd p) Origin wR
            -> TreeIO (FL (PatchInfoAnd p) Origin wR, Bool)
applyAndFix _ _ NilFL = return (NilFL, True)
applyAndFix r@(Repo r' _ _ c) compr psin =
    do liftIO $ beginTedious k
       liftIO $ tediousSize k $ lengthFL psin
       (repaired, ok) <- aaf psin
       liftIO $ endTedious k
       orig <- liftIO $ newset2FL `fmap` readRepo r
       return (replaceInFL orig repaired, ok)
    where k = "Replaying patch"
          aaf :: FL (PatchInfoAnd p) wW wZ -> TreeIO ([Sealed2 (WPatchInfo :||: PatchInfoAnd p)], Bool)
          aaf NilFL = return ([], True)
          aaf (p:>:ps) = do
            mp' <- applyAndTryToFix p
            case isInconsistent . patchcontents . hopefully $ p of
              Just err -> liftIO $ putDocLn err
              Nothing -> return ()
            let !winfp = winfo p -- assure that 'p' can be garbage collected.
            liftIO $ finishedOneIO k $ showDoc Encode $ showPatchInfoUI $ unWPatchInfo winfp
            (ps', restok) <- aaf ps
            case mp' of
              Nothing -> return (ps', restok)
              Just (e,pp) -> liftIO $ do putStrLn e
                                         p' <- withCurrentDirectory r' $ writeAndReadPatch c compr pp
                                         return (Sealed2 (winfp :||: p'):ps', False)

data RepositoryConsistency p wX =
    RepositoryConsistent
  | BrokenPristine (Tree IO)
  | BrokenPatches (Tree IO) (PatchSet p Origin wX)

checkUniqueness :: (RepoPatch p, ApplyState p ~ Tree)
                => (Doc -> IO ()) -> (Doc -> IO ()) -> Repository p wR wU wT -> IO ()
checkUniqueness putVerbose putInfo repository =
    do putVerbose $ text "Checking that patch names are unique..."
       r <- readRepo repository
       case hasDuplicate $ mapRL info $ newset2RL r of
         Nothing -> return ()
         Just pinf -> do putInfo $ text "Error! Duplicate patch name:"
                         putInfo $ showPatchInfoUI pinf
                         fail "Duplicate patches found."

hasDuplicate :: Ord a => [a] -> Maybe a
hasDuplicate li = hd $ sort li
    where hd [_] = Nothing
          hd [] = Nothing
          hd (x1:x2:xs) | x1 == x2 = Just x1
                        | otherwise = hd (x2:xs)
replayRepository' ::
    forall p wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
               => DiffAlgorithm -> AbsolutePath -> Repository p wR wU wT -> Compression -> Verbosity -> IO (RepositoryConsistency p wR)
replayRepository' dflag whereToReplay' repo compr verbosity = do
  let whereToReplay = toFilePath whereToReplay'
      putVerbose s = when (verbosity == Verbose) $ putDocLn s
      putInfo s = unless (verbosity == Quiet) $ putDocLn s
  checkUniqueness putVerbose putInfo repo
  createDirectoryIfMissing False whereToReplay
  putVerbose $ text "Reading recorded state..."
  pris <- readRecorded repo `catch` \(_ :: IOException) -> return emptyTree
  putVerbose $ text "Applying patches..."
  patches <- readRepo repo
  debugMessage "Fixing any broken patches..."
  let psin = newset2FL patches
      repair = applyAndFix repo compr psin

  ((ps, patches_ok), newpris) <- hashedTreeIO repair emptyTree whereToReplay
  debugMessage "Done fixing broken patches..."
  let newpatches = PatchSet (reverseFL ps) NilRL

  debugMessage "Checking pristine against slurpy"
  ftf <- filetypeFunction
  is_same <- do Sealed diff <- unFreeLeft `fmap` treeDiff dflag ftf pris newpris :: IO (Sealed (FL (PrimOf p) wR))
                return $ nullFL diff
              `catchall` return False
  -- TODO is the latter condition needed? Does a broken patch imply pristine
  -- difference? Why, or why not?
  return (if is_same && patches_ok
     then RepositoryConsistent
     else if patches_ok
            then BrokenPristine newpris
            else BrokenPatches newpris newpatches)

cleanupRepositoryReplay :: Repository p wR wU wT -> IO ()
cleanupRepositoryReplay r = do
  let c = extractCache r
  rf <- identifyRepoFormat "."
  unless (formatHas HashedInventory rf) $
         rmRecursive $ darcsdir ++ "/pristine.hashed"
  when (formatHas HashedInventory rf) $ do
       current <- readHashedPristineRoot r
       cleanHashdir c HashedPristineDir $ catMaybes [current]

replayRepositoryInTemp :: (RepoPatch p, ApplyState p ~ Tree)
                       => DiffAlgorithm -> Repository p wR wU wT -> Compression -> Verbosity
                          -> IO (RepositoryConsistency p wR)
replayRepositoryInTemp dflag r compr verb = do
  repodir <- getCurrentDirectory
  withTempDir "darcs-check" $ \tmpDir -> do
    setCurrentDirectory repodir
    replayRepository' dflag tmpDir r compr verb

replayRepository :: (RepoPatch p, ApplyState p ~ Tree)
                 => DiffAlgorithm -> Repository p wR wU wT -> Compression -> Verbosity
                 -> (RepositoryConsistency p wR -> IO a) -> IO a
replayRepository dflag r compr verb f =
  run `finally` cleanupRepositoryReplay r
    where run = do
            createDirectoryIfMissing False $ darcsdir </> "pristine.hashed"
            hashedPristine <- ioAbsolute $ darcsdir </> "pristine.hashed"
            st <- replayRepository' dflag hashedPristine r compr verb
            f st

checkIndex :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> Bool -> IO Bool
checkIndex repo quiet = do
  index <- updateIndex =<< readIndex repo
  pristine <- expand =<< readRecordedAndPending repo
  working <- expand =<< restrict pristine <$> readPlainTree "."
  working_hashed <- darcsUpdateHashes working
  let index_paths = [ p | (p, _) <- list index ]
      working_paths = [ p | (p, _) <- list working ]
      index_extra = index_paths \\ working_paths
      working_extra = working_paths \\ index_paths
      gethashes p (Just i1) (Just i2) = (p, itemHash i1, itemHash i2)
      gethashes p (Just i1) Nothing   = (p, itemHash i1, NoHash)
      gethashes p   Nothing (Just i2) = (p,      NoHash, itemHash i2)
      gethashes p   Nothing Nothing   = error $ "Bad case at " ++ show p
      mismatches = [ miss | miss@(_, h1, h2) <- zipTrees gethashes index working_hashed, h1 /= h2 ]

      format paths = unlines $ map (("  " ++) . anchorPath "") paths
      mismatches_disp = unlines [ anchorPath "" p ++
                                    "\n    index: " ++ BS.unpack (encodeBase16 h1) ++
                                    "\n  working: " ++ BS.unpack (encodeBase16 h2)
                                  | (p, h1, h2) <- mismatches ]
  unless (quiet || null index_extra) $
         putStrLn $ "Extra items in index!\n" ++ format index_extra
  unless (quiet || null working_extra) $
         putStrLn $ "Missing items in index!\n" ++ format working_extra
  unless (quiet || null mismatches) $
         putStrLn $ "Hash mismatch(es)!\n" ++ mismatches_disp
  return $ null index_extra && null working_extra && null mismatches

