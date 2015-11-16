{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Storage.Hashed.Test( tests ) where

import Prelude hiding ( filter, readFile, writeFile, lookup )
import qualified Prelude
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Control.Exception( finally )
import System.Directory( doesFileExist, removeFile, doesDirectoryExist )
import System.FilePath( (</>) )
import Control.Monad.Identity
import Control.Monad.Trans( lift )
import Control.Applicative( (<$>) )

import Data.Maybe
import Data.Word
import Data.List( sort, intercalate, nub, intersperse )

import Storage.Hashed
import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree hiding ( lookup )
import Storage.Hashed.Index
import Storage.Hashed.Utils
import Storage.Hashed.Darcs
import Storage.Hashed.Packed hiding ( lookup )
import Storage.Hashed.Hash
import Storage.Hashed.Monad hiding ( tree )

import System.Mem( performGC )

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Bundled.Posix as Posix
    ( getFileStatus, getSymbolicLinkStatus, fileSize, fileExists )

import Test.HUnit hiding ( path )
import Test.Framework( testGroup )
import qualified Test.Framework as TF ( Test )
import Test.QuickCheck

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

------------------------
-- Test Data
--

blobs :: [(AnchoredPath, BL.ByteString)]
blobs = [ (floatPath "foo_a", BL.pack "a\n")
        , (floatPath "foo_dir/foo_a", BL.pack "a\n")
        , (floatPath "foo_dir/foo_b", BL.pack "b\n")
        , (floatPath "foo_dir/foo_subdir/foo_a", BL.pack "a\n")
        , (floatPath "foo space/foo\nnewline", BL.pack "newline\n")
        , (floatPath "foo space/foo\\backslash", BL.pack "backslash\n")
        , (floatPath "foo space/foo_a", BL.pack "a\n") ]

files :: [AnchoredPath]
files = map fst blobs

dirs :: [AnchoredPath]
dirs = [ floatPath "foo_dir"
       , floatPath "foo_dir/foo_subdir"
       , floatPath "foo space" ]

emptyStub :: TreeItem IO
emptyStub = Stub (return emptyTree) NoHash

testTree :: Tree IO
testTree =
    makeTree [ (makeName "foo", emptyStub)
             , (makeName "subtree", SubTree sub)
             , (makeName "substub", Stub getsub NoHash) ]
    where sub = makeTree [ (makeName "stub", emptyStub)
                         , (makeName "substub", Stub getsub2 NoHash)
                         , (makeName "x", SubTree emptyTree) ]
          getsub = return sub
          getsub2 = return $ makeTree [ (makeName "file", File emptyBlob)
                                      , (makeName "file2",
                                         File $ Blob (return $ BL.pack "foo") NoHash) ]

equals_testdata :: Tree IO -> IO ()
equals_testdata t = sequence_ [
                     do isJust (findFile t p) @? show p ++ " in tree"
                        ours <- readBlob (fromJust $ findFile t p)
                        ours @?= stored
                     | (p, stored) <- blobs ] >>
                    sequence_ [ isJust (Prelude.lookup p blobs) @? show p ++ " extra in tree"
                                | (p, File _) <- list t ]

---------------------------
-- Test list
--

tests :: [TF.Test]
tests = [ testGroup "Bundled.Posix" posix
        , testGroup "Storage.Hashed.Utils" utils
        , testGroup "Storage.Hashed.Hash" hash
        , testGroup "Storage.Hashed.Tree" tree
        , testGroup "Storage.Hashed.Index" index
        , testGroup "Storage.Hashed.Packed" packed
        , testGroup "Storage.Hashed.Monad" monad
        , testGroup "Storage.Hashed" hashed ]

--------------------------
-- Tests
--

hashed :: [TF.Test]
hashed = [ testCase "plain has all files" have_files
         , testCase "pristine has all files" have_pristine_files
         , testCase "pristine has no extras" pristine_no_extra
         , testCase "pristine file contents match" pristine_contents
         , testCase "plain file contents match" plain_contents
         , testCase "writePlainTree works" write_plain ]
    where
      check_file t f = assertBool
                       ("path " ++ show f ++ " is missing in tree " ++ show t)
                       (isJust $ find t f)
      check_files = forM_ files . check_file

      pristine_no_extra = do
        t <- readDarcsPristine "." >>= expand
        forM_ (list t) $ \(path,_) -> assertBool (show path ++ " is extraneous in tree")
                                                 (path `elem` (dirs ++ files))
      have_files = readPlainTree "." >>= expand >>= check_files
      have_pristine_files =
         readDarcsPristine "." >>= expand >>= check_files

      pristine_contents = do
        t <- readDarcsPristine "." >>= expand
        equals_testdata t

      plain_contents = do
        t <- expand =<< filter nondarcs `fmap` readPlainTree "."
        equals_testdata t

      write_plain = do
        orig <- readDarcsPristine "." >>= expand
        writePlainTree orig "_darcs/plain"
        t <- expand =<< readPlainTree "_darcs/plain"
        equals_testdata t

index :: [TF.Test]
index = [ testCase "index versioning" check_index_versions
        , testCase "index listing" check_index
        , testCase "index content" check_index_content ]
    where pristine = readDarcsPristine "." >>= expand
          build_index =
            do x <- pristine
               exist <- doesFileExist "_darcs/index"
               performGC -- required in win32 to trigger file close
               when exist $ removeFile "_darcs/index"
               idx <- updateIndex =<< updateIndexFrom "_darcs/index" darcsTreeHash x
               return (x, idx)
          check_index =
            do (pris, idx) <- build_index
               (sort $ map fst $ list idx) @?= (sort $ map fst $ list pris)
          check_blob_pair p x y =
              do a <- readBlob x
                 b <- readBlob y
                 assertEqual ("content match on " ++ show p) a b
          check_index_content =
            do (_, idx) <- build_index
               plain <- readPlainTree "."
               x <- sequence $ zipCommonFiles check_blob_pair plain idx
               assertBool "files match" (length x > 0)
          check_index_versions =
            do performGC -- required in win32 to trigger file close
               Prelude.writeFile "_darcs/index" "nonsense index... do not crash!"
               valid <- indexFormatValid "_darcs/index"
               assertBool "index format invalid" $ not valid

tree :: [TF.Test]
tree = [ testCase "modifyTree" check_modify
       , testCase "complex modifyTree" check_modify_complex
       , testCase "modifyTree removal" check_modify_remove
       , testCase "expand" check_expand
       , testCase "expandPath" check_expand_path
       , testCase "expandPath of sub" check_expand_path_sub
       , testCase "diffTrees" check_diffTrees
       , testCase "diffTrees identical" check_diffTrees_ident
       , testProperty "expandPath" prop_expandPath
       , testProperty "shapeEq" prop_shape_eq
       , testProperty "expandedShapeEq" prop_expanded_shape_eq
       , testProperty "expand is identity" prop_expand_id
       , testProperty "filter True is identity" prop_filter_id
       , testProperty "filter False is empty" prop_filter_empty
       , testProperty "restrict both ways keeps shape" prop_restrict_shape_commutative
       , testProperty "restrict is a subtree of both" prop_restrict_subtree
       , testProperty "overlay keeps shape" prop_overlay_shape
       , testProperty "overlay is superset of over" prop_overlay_super ]
    where blob x = File $ Blob (return (BL.pack x)) (sha256 $ BL.pack x)
          name = Name . BS.pack
          check_modify =
              let t = makeTree [(name "foo", blob "bar")]
                  modify = modifyTree t (floatPath "foo") (Just $ blob "bla")
               in do x <- readBlob $ fromJust $ findFile t (floatPath "foo")
                     y <- readBlob $ fromJust $ findFile modify (floatPath "foo")
                     assertEqual "old version" x (BL.pack "bar")
                     assertEqual "new version" y (BL.pack "bla")
                     assertBool "list has foo" $
                                isJust (Prelude.lookup (floatPath "foo") $ list modify)
                     length (list modify) @?= 1
          check_modify_complex =
              let t = makeTree [ (name "foo", blob "bar")
                               , (name "bar", SubTree t1) ]
                  t1 = makeTree [ (name "foo", blob "bar") ]
                  modify = modifyTree t (floatPath "bar/foo") (Just $ blob "bla")
               in do foo <- readBlob $ fromJust $ findFile t (floatPath "foo")
                     foo' <- readBlob $ fromJust $ findFile modify (floatPath "foo")
                     bar_foo <- readBlob $ fromJust $
                                findFile t (floatPath "bar/foo")
                     bar_foo' <- readBlob $ fromJust $
                                 findFile modify (floatPath "bar/foo")
                     assertEqual "old foo" foo (BL.pack "bar")
                     assertEqual "old bar/foo" bar_foo (BL.pack "bar")
                     assertEqual "new foo" foo' (BL.pack "bar")
                     assertEqual "new bar/foo" bar_foo' (BL.pack "bla")
                     assertBool "list has bar/foo" $
                                isJust (Prelude.lookup (floatPath "bar/foo") $ list modify)
                     assertBool "list has foo" $
                                isJust (Prelude.lookup (floatPath "foo") $ list modify)
                     length (list modify) @?= length (list t)
          check_modify_remove =
              let t1 = makeTree [(name "foo", blob "bar")]
                  t2 :: Tree Identity = makeTree [ (name "foo", blob "bar")
                                                 , (name "bar", SubTree t1) ]
                  modify1 = modifyTree t1 (floatPath "foo") Nothing
                  modify2 = modifyTree t2 (floatPath "bar") Nothing
                  file = findFile modify1 (floatPath "foo")
                  subtree = findTree modify2 (floatPath "bar")
               in do assertBool "file is gone" (isNothing file)
                     assertBool "subtree is gone" (isNothing subtree)

          no_stubs t = null [ () | (_, Stub _ _) <- list t ]
          path = floatPath "substub/substub/file"
          badpath = floatPath "substub/substub/foo"
          check_expand = do
            x <- expand testTree
            assertBool "no stubs in testTree" $ not (no_stubs testTree)
            assertBool "stubs in expanded tree" $ no_stubs x
            assertBool "path reachable" $ path `elem` (map fst $ list x)
            assertBool "badpath not reachable" $
                       badpath `notElem` (map fst $ list x)
          check_expand_path = do
            test_exp <- expand testTree
            t <- expandPath testTree path
            t' <- expandPath test_exp path
            t'' <- expandPath testTree $ floatPath "substub/x"
            assertBool "path not reachable in testTree" $ path `notElem` (map fst $ list testTree)
            assertBool "path reachable in t" $ path `elem` (map fst $ list t)
            assertBool "path reachable in t'" $ path `elem` (map fst $ list t')
            assertBool "path reachable in t (with findFile)" $
                       isJust $ findFile t path
            assertBool "path reachable in t' (with findFile)" $
                       isJust $ findFile t' path
            assertBool "path not reachable in t''" $ path `notElem` (map fst $ list t'')
            assertBool "badpath not reachable in t" $
                       badpath `notElem` (map fst $ list t)
            assertBool "badpath not reachable in t'" $
                       badpath `notElem` (map fst $ list t')

          check_expand_path_sub = do
            t <- expandPath testTree $ floatPath "substub"
            t' <- expandPath testTree $ floatPath "substub/stub"
            t'' <- expandPath testTree $ floatPath "subtree/stub"
            assertBool "leaf is not a Stub" $
                isNothing (findTree testTree $ floatPath "substub")
            assertBool "leaf is not a Stub" $ isJust (findTree t $ floatPath "substub")
            assertBool "leaf is not a Stub (2)" $ isJust (findTree t' $ floatPath "substub/stub")
            assertBool "leaf is not a Stub (3)" $ isJust (findTree t'' $ floatPath "subtree/stub")

          check_diffTrees =
            flip finally (Prelude.writeFile "foo_dir/foo_a" "a\n") $
                 do Prelude.writeFile "foo_dir/foo_a" "b\n"
                    working_plain <- filter nondarcs `fmap` readPlainTree "."
                    working <- updateIndex =<<
                                 updateIndexFrom "_darcs/index" darcsTreeHash working_plain
                    pristine <- readDarcsPristine "."
                    (working', pristine') <- diffTrees working pristine
                    let foo_work = findFile working' (floatPath "foo_dir/foo_a")
                        foo_pris = findFile pristine' (floatPath "foo_dir/foo_a")
                    working' `shapeEq` pristine'
                             @? show working' ++ " `shapeEq` " ++ show pristine'
                    assertBool "foo_dir/foo_a is in working'" $ isJust foo_work
                    assertBool "foo_dir/foo_a is in pristine'" $ isJust foo_pris
                    foo_work_c <- readBlob (fromJust foo_work)
                    foo_pris_c <- readBlob (fromJust foo_pris)
                    BL.unpack foo_work_c @?= "b\n"
                    BL.unpack foo_pris_c @?= "a\n"
                    assertEqual "working' tree is minimal" 2 (length $ list working')
                    assertEqual "pristine' tree is minimal" 2 (length $ list pristine')

          check_diffTrees_ident = do
            pristine <- readDarcsPristine "."
            (t1, t2) <- diffTrees pristine pristine
            assertBool "t1 is empty" $ null (list t1)
            assertBool "t2 is empty" $ null (list t2)

          prop_shape_eq x = no_stubs x ==> x `shapeEq` x
              where _types = x :: Tree Identity
          prop_expanded_shape_eq x = runIdentity $ expandedShapeEq x x
              where _types = x :: Tree Identity
          prop_expand_id x = no_stubs x ==> runIdentity (expand x) `shapeEq` x
              where _types = x :: Tree Identity
          prop_filter_id x = runIdentity $ expandedShapeEq x $ filter (\_ _ -> True) x
              where _types = x :: Tree Identity
          prop_filter_empty x = runIdentity $ expandedShapeEq emptyTree $ filter (\_ _ -> False) x
              where _types = x :: Tree Identity
          prop_restrict_shape_commutative (t1, t2) =
              no_stubs t1 && no_stubs t2 && not (restrict t1 t2 `shapeEq` emptyTree) ==>
                  restrict t1 t2 `shapeEq` restrict t2 t1
              where _types = (t1 :: Tree Identity, t2 :: Tree Identity)
          prop_restrict_subtree (t1, t2) =
              no_stubs t1 && not (restrict t1 t2 `shapeEq` emptyTree) ==>
                  let restricted = S.fromList (map fst $ list $ restrict t1 t2)
                      orig1 = S.fromList (map fst $ list t1)
                      orig2 = S.fromList (map fst $ list t2)
                   in and [restricted `S.isSubsetOf` orig1, restricted `S.isSubsetOf` orig2]
              where _types = (t1 :: Tree Identity, t2 :: Tree Identity)
          prop_overlay_shape (t1 :: Tree Identity, t2) =
              (Just LT == runIdentity (t2 `cmpExpandedShape` t1)) ==>
              runIdentity $ (t1 `overlay` t2) `expandedShapeEq` t1
          prop_overlay_super (t1 :: Tree Identity, t2) =
              (Just LT == runIdentity (t2 `cmpExpandedShape` t1)) && no_stubs t2 ==>
              Just EQ == (runIdentity $ restrict t2 (t1 `overlay` t2) `cmpTree` t2)
          prop_expandPath (TreeWithPath t p) =
              notStub $ find (runIdentity $ expandPath t p) p
            where notStub (Just (Stub _ _)) = False
                  notStub Nothing = error "Did not exist."
                  notStub _ = True

packed :: [TF.Test]
packed = [ testCase "loose pristine tree" check_loose
         , testCase "load" check_load
         , testCase "live" check_live
         , testCase "compact" check_compact ]
    where root_hash = treeHash <$> get_pristine
          get_pristine = darcsUpdateDirHashes <$> (expand =<< readDarcsPristine ".")
          check_loose = do x <- readDarcsPristine "."
                           os <- create "_darcs/loose" Loose
                           (os', root) <- writePackedDarcsPristine x os
                           y <- expand =<< readPackedDarcsPristine os' root
                           equals_testdata y
          check_load = do os <- load "_darcs/loose"
                          format (hatchery os) @?= Loose
                          root <- root_hash
                          y <- expand =<< readPackedDarcsPristine os root
                          equals_testdata y
          check_live = do os <- load "_darcs/loose"
                          x <- get_pristine
                          root <- root_hash
                          alive <- live (os { roots = [ root ]
                                            , references = darcsPristineRefs }) [hatchery os]
                          sequence_ [ assertBool (show hashValue ++ " is alive") $
                                                 hashValue `S.member` M.keysSet alive
                                      | hashValue <- map (itemHash . snd) $ list x ]
                          length (M.toList alive) @?= 1 + length (nub $ map snd blobs) + length dirs
          check_compact = do os <- load "_darcs/loose"
                             x <- darcsUpdateDirHashes `fmap` (expand =<< readDarcsPristine ".")
                             (os', root) <- storePackedDarcsPristine x os
                             hatch_root_old <- blockLookup (hatchery os') root
                             assertBool "bits in the old hatchery" $ isJust hatch_root_old

                             os'' <- compact os'
                             length (mature os'') @?= 1
                             hatch_root <- blockLookup (hatchery os'') root
                             mature_root <- blockLookup (head $ mature os'') root
                             assertBool "bits no longer in hatchery" $ isNothing hatch_root
                             assertBool "bits now in the mature space" $ isJust mature_root
                             mature_root_con <- readSegment (fromJust mature_root)
                             Just mature_root_con @?= darcsFormatDir x

                             y <- expand =<< readPackedDarcsPristine os'' root
                             equals_testdata y

utils :: [TF.Test]
utils = [ testProperty "xlate32" prop_xlate32
        , testProperty "xlate64" prop_xlate64
        , testProperty "align bounded" prop_align_bounded
        , testProperty "align aligned" prop_align_aligned
        , testProperty "reachable is a subset" prop_reach_subset
        , testProperty "roots are reachable" prop_reach_roots
        , testProperty "nonexistent roots are not reachable" prop_reach_nonroots
        , testCase "an example for reachable" check_reachable
        , testCase "fixFrom" check_fixFrom
        , testCase "mmap empty file" check_mmapEmpty ]
    where prop_xlate32 x = (xlate32 . xlate32) x == x where _types = x :: Word32
          prop_xlate64 x = (xlate64 . xlate64) x == x where _types = x :: Word64
          prop_align_bounded (bound, x) =
              bound > 0 && bound < 1024 && x >= 0 ==>
                    align bound x >= x && align bound x < x + bound
              where _types = (bound, x) :: (Int, Int)
          prop_align_aligned (bound, x) =
              bound > 0 && bound < 1024 && x >= 0 ==>
                    align bound x `rem` bound == 0
              where _types = (bound, x) :: (Int, Int)

          check_fixFrom = let f 0 = 0
                              f n = f (n - 1) in fixFrom f 5 @?= (0 :: Integer)

          check_mmapEmpty = flip finally (removeFile "test_empty") $ do
                              Prelude.writeFile "test_empty" ""
                              x <- readSegment ("test_empty", Nothing)
                              x @?= BL.empty

          reachable' ref look rootsSet = runIdentity $ reachable ref look rootsSet

          check_reachable = let refs 0 = [1, 2]
                                refs 1 = [2]
                                refs 2 = [0, 4]
                                refs 3 = [4, 6, 7]
                                refs 4 = [0, 1]
                                refs _ = error "internal error in check_reachable"
                                set = S.fromList [1, 2]
                                mp = M.fromList [ (n, refs n) | n <- [0..10] :: [Int] ]
                                reach = reachable' return (lookup mp) set
                             in do M.keysSet reach @?= S.fromList [0, 1, 2, 4]

          prop_reach_subset (set :: S.Set Int, mp :: M.Map Int [Int]) =
              M.keysSet (reachable' return (lookup mp) set)
                   `S.isSubsetOf` M.keysSet mp
          prop_reach_roots (set :: S.Set Int, mp :: M.Map Int [Int]) =
              set `S.isSubsetOf` M.keysSet mp
                      ==> set `S.isSubsetOf`
                            M.keysSet (reachable' return (lookup mp) set)

          prop_reach_nonroots (set :: S.Set Int, mp :: M.Map Int [Int]) =
              set `S.intersection` M.keysSet mp
                      == M.keysSet (reachable' (return . const [])
                                   (lookup mp) set)

          lookup :: (Ord a) => M.Map a [a] -> a -> Identity (Maybe (a, [a]))
          lookup m k = return $ case M.lookupIndex k m of
                                  Nothing -> Nothing
                                  Just i -> Just $ M.elemAt i m

hash :: [TF.Test]
hash = [ testProperty "decodeBase16 . encodeBase16 == id" prop_base16
       , testProperty "decodeBase64u . encodeBase64u == id" prop_base64u ]
    where prop_base16 x = (decodeBase16 . encodeBase16) x == x
          prop_base64u x = (decodeBase64u . encodeBase64u) x == x

monad :: [TF.Test]
monad = [ testCase "path expansion" check_virtual
        , testCase "rename" check_rename ]
    where check_virtual = virtualTreeMonad run testTree >> return ()
              where run = do file <- readFile (floatPath "substub/substub/file")
                             file2 <- readFile (floatPath "substub/substub/file2")
                             lift $ BL.unpack file @?= ""
                             lift $ BL.unpack file2 @?= "foo"
          check_rename = do (_, t) <- virtualTreeMonad run testTree
                            t' <- darcsAddMissingHashes =<< expand t
                            forM_ [ (p, i) | (p, i) <- list t' ] $ \(p,i) ->
                               assertBool ("have hash: " ++ show p) $ itemHash i /= NoHash
              where run = do rename (floatPath "substub/substub/file") (floatPath "substub/file2")

posix :: [TF.Test]
posix = [ testCase "getFileStatus" $ check_stat Posix.getFileStatus
        , testCase "getSymbolicLinkStatus" $ check_stat Posix.getSymbolicLinkStatus ]
    where check_stat fun = flip finally (removeFile "test_empty") $ do
            x <- Posix.fileSize `fmap` fun "foo_a"
            Prelude.writeFile "test_empty" ""
            y <- Posix.fileSize `fmap` fun "test_empty"
            exist_nonexistent <- Posix.fileExists `fmap` fun "test_does_not_exist"
            exist_existent <- Posix.fileExists `fmap` fun "test_empty"
            assertEqual "file size" x 2
            assertEqual "file size" y 0
            assertBool "existence check" $ not exist_nonexistent
            assertBool "existence check" exist_existent

----------------------------------
-- Arbitrary instances
--

instance (Arbitrary a, Ord a) => Arbitrary (S.Set a)
    where arbitrary = S.fromList `fmap` arbitrary

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (M.Map k v)
    where arbitrary = M.fromList `fmap` arbitrary

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack `fmap` arbitrary

instance Arbitrary Hash where
    arbitrary = sized hash'
        where hash' 0 = return NoHash
              hash' _ = do
                tag <- oneof [return False, return True]
                case tag of
                  False -> SHA256 . BS.pack <$> sequence [ arbitrary | _ <- [1..32] :: [Int] ]
                  True -> SHA1 . BS.pack <$> sequence [ arbitrary | _ <- [1..20] :: [Int] ]

instance (Monad m) => Arbitrary (TreeItem m) where
  arbitrary = sized tree'
    where tree' 0 = oneof [ return (File emptyBlob), return (SubTree emptyTree) ]
          tree' n = oneof [ file n, subtree n ]
          file 0 = return (File emptyBlob)
          file _ = do content <- arbitrary
                      return (File $ Blob (return content) NoHash)
          subtree n = do branches <- choose (1, n)
                         let sub name = do t <- tree' ((n - 1) `div` branches)
                                           return (makeName $ show name, t)
                         sublist <- mapM sub [0..branches]
                         oneof [ tree' 0
                               , return (SubTree $ makeTree sublist)
                               , return $ (Stub $ return (makeTree sublist)) NoHash ]

instance (Monad m) => Arbitrary (Tree m) where
  arbitrary = do item <- arbitrary
                 case item of
                   File _ -> arbitrary
                   Stub _ _ -> arbitrary
                   SubTree t -> return t

data TreeWithPath = TreeWithPath (Tree Identity) AnchoredPath deriving (Show)

instance Arbitrary TreeWithPath where
  arbitrary = do t <- arbitrary
                 p <- oneof $ return (AnchoredPath []) :
                             (map (return . fst) $ list (runIdentity $ expand t))
                 return $ TreeWithPath t p

---------------------------
-- Other instances
--

instance Show (Blob m) where
    show (Blob _ h) = "Blob " ++ show h

instance Show (TreeItem m) where
    show (File f) = "File (" ++ show f ++ ")"
    show (Stub _ h) = "Stub _ " ++ show h
    show (SubTree s) = "SubTree (" ++ show s ++ ")"

instance Show (Tree m) where
    show t = "Tree " ++ show (treeHash t) ++ " { " ++
             (concat . intersperse ", " $ itemstrs) ++ " }"
        where itemstrs = map show $ listImmediate t

instance Show (Int -> Int) where
    show f = "[" ++ intercalate ", " (map val [1..20]) ++ " ...]"
        where val x = show x ++ " -> " ++ show (f x)

-----------------------
-- Test utilities
--

shapeEq :: Tree m -> Tree m -> Bool
shapeEq a b = Just EQ == cmpShape a b

expandedShapeEq :: (Monad m, Functor m) => Tree m -> Tree m -> m Bool
expandedShapeEq a b = (Just EQ ==) <$> cmpExpandedShape a b

cmpcat :: [Maybe Ordering] -> Maybe Ordering
cmpcat (x:y:rest) | x == y = cmpcat (x:rest)
                  | x == Just EQ = cmpcat (y:rest)
                  | y == Just EQ = cmpcat (x:rest)
                  | otherwise = Nothing
cmpcat [x] = x
cmpcat [] = Just EQ -- empty things are equal

cmpTree :: (Monad m, Functor m) => Tree m -> Tree m -> m (Maybe Ordering)
cmpTree x y = do x' <- expand x
                 y' <- expand y
                 con <- contentsEq x' y'
                 return $ cmpcat [cmpShape x' y', con]
    where contentsEq a b = cmpcat <$> sequence (zipTrees cmp a b)
          cmp _ (Just (File a)) (Just (File b)) = do a' <- readBlob a
                                                     b' <- readBlob b
                                                     return $ Just (compare a' b')
          cmp _ _ _ = return (Just EQ) -- neutral

cmpShape :: Tree m -> Tree m -> Maybe Ordering
cmpShape t r = cmpcat $ zipTrees cmp t r
    where cmp _ (Just a) (Just b) = a `item` b
          cmp _ Nothing (Just _) = Just LT
          cmp _ (Just _) Nothing = Just GT
          cmp _ Nothing Nothing = Just EQ
          item (File _) (File _) = Just EQ
          item (SubTree s) (SubTree p) = s `cmpShape` p
          item _ _ = Nothing

cmpExpandedShape :: (Monad m) => Tree m -> Tree m -> m (Maybe Ordering)
cmpExpandedShape a b = do x <- expand a
                          y <- expand b
                          return $ x `cmpShape` y

nondarcs :: AnchoredPath -> TreeItem m -> Bool
nondarcs (AnchoredPath (Name x:_)) _ | x == BS.pack "_darcs" = False
                                     | otherwise = True
nondarcs (AnchoredPath []) _ = True

readDarcsPristine :: FilePath -> IO (Tree IO)
readDarcsPristine dir = do
  let darcs = dir </> "_darcs"
      h_inventory = darcs </> "hashed_inventory"
  repo <- doesDirectoryExist darcs
  unless repo $ fail $ "Not a darcs repository: " ++ dir
  isHashed <- doesFileExist h_inventory
  if isHashed
     then do inv <- BS.readFile h_inventory
             let thelines = BS.split '\n' inv
             case thelines of
               [] -> return emptyTree
               (pris_line:_) -> do
                          let thehash = decodeDarcsHash $ BS.drop 9 pris_line
                              thesize = decodeDarcsSize $ BS.drop 9 pris_line
                          when (thehash == NoHash) $ fail $ "Bad pristine root: " ++ show pris_line
                          readDarcsHashed (darcs </> "pristine.hashed") (thesize, thehash)
     else do have_pristine <- doesDirectoryExist $ darcs </> "pristine"
             have_current <- doesDirectoryExist $ darcs </> "current"
             case (have_pristine, have_current) of
               (True, _) -> readPlainTree $ darcs </> "pristine"
               (False, True) -> readPlainTree $ darcs </> "current"
               (_, _) -> fail "No pristine tree is available!"
