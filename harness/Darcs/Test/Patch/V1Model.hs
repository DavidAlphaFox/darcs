{-# LANGUAGE CPP #-}


-- | Repository model
module Darcs.Test.Patch.V1Model
  ( module Storage.Hashed.AnchoredPath
  , V1Model, repoTree
  , RepoItem, File, Dir, Content
  , makeRepo, emptyRepo
  , makeFile, emptyFile
  , emptyDir
  , nullRepo
  , isFile, isDir
  , fileContent, dirContent
  , isEmpty
  , root
  , filterFiles, filterDirs
  , find
  , list
  , ap2fp
  , aFilename, aDirname
  , aLine, aContent
  , aFile, aDir
  , aRepo
  ) where


import Darcs.Test.Util.QuickCheck ( alpha, uniques, bSized )
import Darcs.Test.Patch.RepoModel

import Darcs.Patch.Apply( applyToTree )
import Darcs.Patch.Witnesses.Sealed ( Sealed, seal )
import Darcs.Patch.Witnesses.Show

import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree( Tree, TreeItem )
import Storage.Hashed.Darcs ( darcsUpdateHashes )
import qualified Storage.Hashed.Tree as T

import Control.Applicative ( (<$>) )
import Control.Arrow ( second )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List ( intercalate )
import qualified Data.Map as M
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen, choose, vectorOf, frequency )

#include "impossible.h"


----------------------------------------------------------------------
-- * Model definition

-- | A repository is an abstraction build in top of a 'Tree'.
-- NB: Repository preferences are not supported yet.
newtype V1Model wX = V1Model {
                           repoTree :: Tree Fail
                         }

-- | Repository items may be text files or directories.
-- NB: Binary files are not supported yet.
newtype RepoItem = RepoItem {
                      treeItem :: TreeItem Fail
                   }

type File = RepoItem
type Dir  = RepoItem

type Content = [B.ByteString]

----------------------------------------
-- Instances

instance Show (V1Model wX) where
  show repo = "V1Model{ "
            ++ intercalate " " (map showEntry $ list repo)
            ++ " }"
    where
        showPath = show . flatten
        showContent content = "[" ++ intercalate " " (map show content) ++ "]"
        showEntry (path,item)
          | isDir item  = showPath path
          | isFile item = showPath path ++ showContent (fileContent item)
        showEntry _ = impossible

instance Show1 V1Model where
  showDict1 = ShowDictClass

----------------------------------------
-- Utils

bs2lbs :: B.ByteString -> BL.ByteString
bs2lbs bs = BL.fromChunks [bs]

lbs2bs :: BL.ByteString -> B.ByteString
lbs2bs = B.concat . BL.toChunks

content2lbs :: Content -> BL.ByteString
content2lbs = BLC.unlines . map bs2lbs

lbs2content :: BL.ByteString -> Content
lbs2content = map lbs2bs . BLC.lines

----------------------------------------------------------------------
-- ** Path conversion

ap2fp :: AnchoredPath -> FilePath
ap2fp = anchorPath ""

----------------------------------------------------------------------
-- * Constructors

makeRepo :: [(Name, RepoItem)] -> V1Model wX
makeRepo = V1Model . T.makeTree . map (second treeItem)

emptyRepo :: V1Model wX
emptyRepo = V1Model T.emptyTree

makeFile :: Content -> File
makeFile = RepoItem . T.File . T.makeBlob . content2lbs

emptyFile :: File
emptyFile = RepoItem $ T.File T.emptyBlob

emptyDir :: Dir
emptyDir = RepoItem $ T.SubTree T.emptyTree

----------------------------------------------------------------------
-- * Queries

nullRepo :: V1Model wX -> Bool
nullRepo = M.null . T.items . repoTree

isFile :: RepoItem -> Bool
isFile (RepoItem (T.File _)) = True
isFile _other                = False

isDir :: RepoItem -> Bool
isDir (RepoItem (T.SubTree _)) = True
isDir _other                   = False

fileContent :: File -> Content
fileContent (RepoItem (T.File blob))
  = lbs2content $ unFail $ T.readBlob blob
fileContent _other
  = error "fileContent: Not a file."

dirContent :: Dir -> [(Name, RepoItem)]
dirContent (RepoItem (T.SubTree subtree))
  = map (second RepoItem) $ M.toList $ T.items subtree
dirContent _other
  = error "dirContent: Not a directory."

-- | @isEmpty file@ <=> file content is empty
--   @isEmpty dir@  <=> dir has no child
isEmpty :: RepoItem -> Bool
isEmpty item
  | isFile item = null $ fileContent item
  | isDir item  = null $ dirContent item
  | otherwise   = undefined

-- | The root directory of a repository.
root :: V1Model wX -> Dir
root = RepoItem . T.SubTree . repoTree

find :: V1Model wX -> AnchoredPath -> Maybe RepoItem
find (V1Model tree) path = RepoItem <$> T.find tree path

-- | List repository items.
-- NB: It does not include the root directory.
list :: V1Model wX -> [(AnchoredPath, RepoItem)]
list (V1Model tree) = map (second RepoItem) $ T.list tree

----------------------------------------------------------------------
-- ** Filtering

filterFiles :: [(n, RepoItem)] -> [(n, File)]
filterFiles = filter (isFile . snd)

filterDirs :: [(n, RepoItem)] -> [(n, Dir)]
filterDirs = filter (isDir . snd)

----------------------------------------------------------------------
-- * Comparing repositories

diffRepos :: V1Model wX -> V1Model wY -> (V1Model wU, V1Model wV)
diffRepos repo1 repo2 =
  let (diff1,diff2) = unFail $ T.diffTrees hashedTree1 hashedTree2
    in (V1Model diff1, V1Model diff2)
  where
      hashedTree1, hashedTree2 :: Tree Fail
      hashedTree1 = unFail $ darcsUpdateHashes $ repoTree repo1
      hashedTree2 = unFail $ darcsUpdateHashes $ repoTree repo2


----------------------------------------------------------------------
-- * Patch application

----------------------------------------------------------------------
-- * QuickCheck generators

-- Testing code assumes that aFilename and aDirname generators
-- will always be able to generate a unique name given a list of
-- existing names. This should be OK as long as the number of possible
-- file/dirnames is much bigger than the number of files/dirs per repository.

-- 'Arbitrary' 'V1Model' instance is based on the 'aSmallRepo' generator.


-- | Files are distinguish by ending their names with ".txt".
aFilename :: Gen Name
aFilename = do len <- choose (1,maxLength)
               name <- vectorOf len alpha
               return $ makeName (name ++ ".txt")
  where
      maxLength = 3

aDirname :: Gen Name
aDirname = do len <- choose (1,maxLength)
              name <- vectorOf len alpha
              return $ makeName name
  where
      maxLength = 3

aWord :: Gen B.ByteString
aWord = do c <- alpha
           return $ BC.pack[c]

aLine :: Gen B.ByteString
aLine = do wordsNo <- choose (1,2)
           ws <- vectorOf wordsNo aWord
           return $ BC.unwords ws

aContent :: Gen Content
aContent = bSized 0 0.5 80 $ \k ->
             do n <- choose (0,k)
                vectorOf n aLine

aFile :: Gen File
aFile = makeFile <$> aContent

-- | See 'aRepo', the same applies for 'aDir'.
aDir :: Int                -- ^ Maximum number of files
        -> Int             -- ^ Maximum number of directories
        -> Gen Dir
aDir filesL dirL = root <$> aRepo filesL dirL

-- | @aRepo filesNo dirsNo@ produces repositories with *at most*
-- @filesNo@ files and @dirsNo@ directories.
-- The structure of the repository is aleatory.
aRepo :: Int                -- ^ Maximum number of files
        -> Int              -- ^ Maximum number of directories
        -> Gen (V1Model wX)
aRepo maxFiles maxDirs
  = do let minFiles = if maxDirs == 0 && maxFiles > 0 then 1 else 0
       filesNo <- choose (minFiles,maxFiles)
       let minDirs = if filesNo == 0 && maxDirs > 0 then 1 else 0
       dirsNo <- choose (minDirs,maxDirs)
            -- NB: Thanks to laziness we don't need to care about division by zero
            -- since if dirsNo == 0 then neither filesPerDirL nor subdirsPerDirL will
            -- be evaluated.
       let filesPerDirL   = (maxFiles-filesNo) `div` dirsNo
           subdirsPerDirL = (maxDirs-dirsNo) `div` dirsNo
       files <- vectorOf filesNo aFile
       filenames <- uniques filesNo aFilename
       dirs <- vectorOf dirsNo (aDir filesPerDirL subdirsPerDirL)
       dirnames <- uniques dirsNo aDirname
       return $ makeRepo (filenames `zip` files ++ dirnames `zip` dirs)

-- | Generate small repositories.
-- Small repositories help generating (potentially) conflicting patches.
instance RepoModel V1Model where
  type RepoState V1Model = Tree
  showModel m = show m
  aSmallRepo = do filesNo <- frequency [(3, return 1), (1, return 2)]
                  dirsNo <- frequency [(3, return 1), (1, return 0)]
                  aRepo filesNo dirsNo
  repoApply (V1Model tree) patch = V1Model <$> applyToTree patch tree
  eqModel repo1 repo2 = let (diff1,diff2) = diffRepos repo1 repo2
                         in nullRepo diff1 && nullRepo diff2


instance Arbitrary (Sealed V1Model) where
  arbitrary = seal <$> aSmallRepo
