{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-orphans #-}
{-# LANGUAGE CPP, OverloadedStrings, MultiParamTypeClasses, StandaloneDeriving #-}


-- | Repository model
module Darcs.Test.Patch.V3Model
  ( module Storage.Hashed.AnchoredPath
  , V3Model
  , Object(..)
  , repoApply
  , emptyFile
  , emptyDir
  , nullRepo
  , isEmpty
  , root, repoObjects
  , aFilename, aDirname
  , aLine, aContent
  , aFile, aDir
  , aRepo
  , anUUID
  ) where


import Darcs.Test.Util.QuickCheck ( alpha, uniques, bSized )
import Darcs.Test.Patch.RepoModel

import Darcs.Patch.Apply( Apply(..), applyToState )
import Darcs.Patch.ApplyMonad( ApplyMonad(..) )
import Darcs.Patch.Prim.V3.Core( UUID(..), Hunk(..), Prim(..), Object(..) )
import Darcs.Patch.Prim.V3.Apply( ObjectMap(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed, seal )
import Darcs.Patch.Witnesses.Show

import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree( Tree, TreeItem )
import Storage.Hashed.Darcs ( darcsUpdateHashes )
import Storage.Hashed.Hash( Hash(..) )
import qualified Storage.Hashed.Tree as T

import Control.Applicative ( (<$>) )
import Control.Arrow ( second )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List ( intercalate, sort )
import qualified Data.Map as M
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen, choose, vectorOf, frequency, oneof )

#include "impossible.h"


----------------------------------------------------------------------
-- * Model definition

newtype V3Model wX = V3Model { repoMap :: ObjectMap Fail }

----------------------------------------
-- Instances

instance Show (Object Fail) where
  show (Directory l) = show l
  show (Blob c _) = show c

deriving instance Eq (Object Fail)

instance Show (V3Model x) where
  show = showModel

instance Show1 V3Model where
  showDict1 = ShowDictClass

----------------------------------------------------------------------
-- * Constructors

objectMap :: (Monad m) => M.Map UUID (Object m) -> ObjectMap m
objectMap map = ObjectMap { getObject = get, putObject = put, listObjects = list }
  where list = return $ M.keys map
        put k o = return $ objectMap (M.insert k o map)
        get k = return $ M.lookup k map

emptyRepo :: V3Model wX
emptyRepo = V3Model (objectMap M.empty)

emptyFile :: (Monad m) => Object m
emptyFile = Blob (return BS.empty) NoHash

emptyDir :: Object m
emptyDir = Directory M.empty

----------------------------------------------------------------------
-- * Queries

nullRepo :: V3Model wX -> Bool
nullRepo = null . repoObjects

-- | @isEmpty file@ <=> file content is empty
--   @isEmpty dir@  <=> dir has no child
isEmpty :: Object Fail -> Bool
isEmpty (Directory d) = M.null d
isEmpty (Blob f _) = BS.null $ unFail f

-- | The root directory of a repository.
root :: V3Model wX -> Object Fail
root (V3Model repo) = fromJust $ unFail $ getObject repo (UUID "ROOT")

repoObjects :: V3Model wX -> [(UUID, Object Fail)]
repoObjects (V3Model repo) = [ (id, obj id) |
                               id <- unFail $ listObjects repo, not $ isEmpty $ obj id ]
  where obj id = fromJust $ unFail $ getObject repo id

----------------------------------------------------------------------
-- * Comparing repositories

----------------------------------------------------------------------
-- * QuickCheck generators

-- Testing code assumes that aFilename and aDirname generators 
-- will always be able to generate a unique name given a list of
-- existing names. This should be OK as long as the number of possible
-- file/dirnames is much bigger than the number of files/dirs per repository.

-- 'Arbitrary' 'V3Model' instance is based on the 'aSmallRepo' generator.


-- | Files are distinguish by ending their names with ".txt".
aFilename :: Gen BS.ByteString
aFilename = do len <- choose (1,maxLength)
               name <- vectorOf len alpha
               return $ BC.pack $ name ++ ".txt"
  where
      maxLength = 3

aDirname :: Gen BS.ByteString
aDirname = do len <- choose (1,maxLength)
              BC.pack <$> vectorOf len alpha
  where
      maxLength = 3

aWord :: Gen BS.ByteString
aWord = do c <- alpha
           return $ BC.pack[c]

aLine :: Gen BS.ByteString
aLine = do wordsNo <- choose (1,2)
           ws <- vectorOf wordsNo aWord
           return $ BC.unwords ws

aContent :: Gen BS.ByteString
aContent = bSized 0 0.5 80 $ \k ->
             do n <- choose (0,k)
                BC.intercalate "\n" <$> vectorOf n aLine

aFile :: (Monad m) => Gen (Object m)
aFile = aContent >>= \c -> return $ Blob (return c) NoHash

aDir :: (Monad m) => [UUID] -> [UUID] -> Gen [(UUID, Object m)]
aDir [] _ = return []
aDir (dirid:dirids) fileids =
  do dirsplit <- choose (1, length dirids)
     filesplit <- choose (1, length fileids)
     let ids = take filesplit fileids
         rem = drop filesplit fileids
     files <- vectorOf filesplit aFile
     names <- vectorOf filesplit aFilename
     dirnames <- vectorOf dirsplit aDirname
     dirs <- subdirs (take dirsplit dirids)
                     (drop dirsplit dirids)
                     (drop filesplit fileids)
     return $ (dirid, Directory $ M.fromList $ names `zip` ids ++ dirnames `zip` dirids)
            : (fileids `zip` files) ++ dirs
  where subdirs [] _ _ = return []
        subdirs tomake dirs files = do
          dirsplit <- choose (1, length dirs)
          filesplit <- choose (1, length files)
          dir <- aDir (head tomake : take dirsplit dirs) (take filesplit files)
          rem <- subdirs (tail tomake) (drop dirsplit dirs) (drop filesplit files)
          return $ dir ++ rem


anUUID :: Gen UUID
anUUID = UUID . BC.pack <$> vectorOf 32 (oneof $ map return "0123456789")

-- | @aRepo filesNo dirsNo@ produces repositories with *at most* 
-- @filesNo@ files and @dirsNo@ directories. 
-- The structure of the repository is aleatory.
aRepo :: Int                -- ^ Maximum number of files
        -> Int              -- ^ Maximum number of directories
        -> Gen (V3Model wX)
aRepo maxFiles maxDirs
  = do let minFiles = if maxDirs == 0 && maxFiles > 0 then 1 else 0
       filesNo <- choose (minFiles,maxFiles)
       let minDirs = if filesNo == 0 && maxDirs > 0 then 1 else 0
       dirsNo <- choose (minDirs,maxDirs)
       dirids <- (UUID "ROOT":) <$> uniques dirsNo anUUID
       fileids <- uniques filesNo anUUID
       objectmap <- aDir dirids fileids
       return $ V3Model $ objectMap $ M.fromList objectmap

-- | Generate small repositories.
-- Small repositories help generating (potentially) conflicting patches.
instance RepoModel V3Model where
  type RepoState V3Model = ObjectMap
  aSmallRepo = do filesNo <- frequency [(3, return 1), (1, return 2)]
                  dirsNo <- frequency [(3, return 1), (1, return 0)]
                  aRepo filesNo dirsNo
  repoApply (V3Model state) patch = V3Model <$> applyToState patch state
  showModel model = "V3Model{\n" ++ unlines (map entry $ repoObjects model) ++ "}"
    where entry (id, obj) = show id ++ " -> " ++ show obj
  eqModel r1 r2 = repoObjects r1 == repoObjects r2

instance Arbitrary (Sealed V3Model) where
  arbitrary = seal <$> aSmallRepo
