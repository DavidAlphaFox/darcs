-- Copyright (C) 2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}


module Darcs.Repository.HashedIO ( HashedIO,
                                   copyHashed, copyPartialsHashed,
                                   cleanHashdir, getHashedFiles,
                                   RW(RW) -- only exported to make warning go away
                                 , pathsAndContents
                                 ) where

import Darcs.Util.Global ( darcsdir )
import qualified Data.Set as Set
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Monad.State ( StateT, runStateT, modify, get, put, gets, lift, evalStateT )
import Control.Monad ( when, void, unless )
import Control.Applicative ( (<$>) )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Repository.Cache ( Cache(..), fetchFileUsingCache, writeFileUsingCache,
                                peekInCache, speculateFileUsingCache,
                                okayHash, cleanCachesWithHint, HashedDir(..), hashedDir )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Repository.Flags ( Compression( .. ), WithWorkingDir (..) )
import Darcs.Repository.Lock ( writeAtomicFilePS, removeFileMayNotExist )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Progress ( debugMessage, tediousSize, finishedOneIO )
import Darcs.Util.Path
    ( FileName
    , normPath
    , fp2fn
    , fn2fp
    , fn2niceps
    , niceps2fn
    , breakOnDir
    , ownName
    , superName
    , FilePathLike
    , toFilePath
    , isMaliciousSubPath
    )

import Darcs.Util.ByteString ( linesPS, unlinesPS )
import qualified Data.ByteString       as B  (ByteString, length, empty)
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Storage.Hashed.Darcs( readDarcsHashedDir, darcsLocation,
                             decodeDarcsHash, decodeDarcsSize )
import Storage.Hashed.Tree( ItemType(..), Tree )

-- | @readHashFile c subdir hash@ reads the file with hash @hash@ in dir subdir,
-- fetching it from 'Cache' @c@ if needed.
readHashFile :: Cache -> HashedDir -> String -> IO (String,B.ByteString)
readHashFile c subdir hash =
    do debugMessage $ "Reading hash file "++hash++" from "++hashedDir subdir++"/"
       fetchFileUsingCache c subdir hash

data HashDir r p = HashDir { permissions :: !r, cache :: !Cache,
                             rootHash :: !String }
type HashedIO p = StateT (HashDir RW p) IO

data RW = RW
{-
class Readable r where
    isRO :: r -> Bool
    isRO = const False
instance Readable RW
instance Readable RO where
    isRO RO = True
-}

mWithCurrentDirectory :: FileName -> HashedIO p a -> HashedIO p a
mWithCurrentDirectory fn j
    | fn' == fp2fn "" = j
    | otherwise =
        case breakOnDir fn' of
        Nothing -> do c <- readroot
                      case geta D fn' c of
                        Nothing -> fail "dir doesn't exist in mWithCurrentDirectory..."
                        Just h -> do (h',x) <- withh h j
                                     writeroot $ seta D fn' h' c
                                     return x
        Just (d,fn'') -> do c <- readroot
                            case geta D d c of
                              Nothing -> fail "dir doesn't exist..."
                              Just h -> do (h',x) <- withh h $ mWithCurrentDirectory fn'' j
                                           writeroot $ seta D d h' c
                                           return x
    where fn' = normPath fn

mInCurrentDirectory :: FileName -> HashedIO p a -> HashedIO p a
mInCurrentDirectory fn j | fn' == fp2fn "" = j
                         | otherwise =
                             case breakOnDir fn' of
                             Nothing -> do c <- readroot
                                           case geta D fn' c of
                                             Nothing -> fail "dir doesn't exist mInCurrentDirectory..."
                                             Just h -> inh h j
                             Just (d,fn'') -> do c <- readroot
                                                 case geta D d c of
                                                   Nothing -> fail "dir doesn't exist..."
                                                   Just h -> inh h $ mInCurrentDirectory fn'' j
    where fn' = normPath fn

instance ApplyMonad (HashedIO p) Tree where
    type ApplyMonadBase (HashedIO p) = IO
    mDoesDirectoryExist fn = do thing <- identifyThing fn
                                case thing of Just (D,_) -> return True
                                              _ -> return False
    mReadFilePS fn = mInCurrentDirectory (superName fn) $ do
                                          c <- readroot
                                          case geta F (ownName fn) c of
                                            Nothing -> fail $ " file don't exist... "++ fn2fp fn
                                            Just h -> readhash h
    mCreateDirectory fn = do h <- writeHashFile B.empty
                             exists <- isJust `fmap` identifyThing fn
                             when exists $ fail "can't mCreateDirectory over an existing object."
                             makeThing fn (D,h)
    mRename o n = do nexists <- isJust `fmap` identifyThing n
                     when nexists $ fail "mRename failed..."
                     mx <- identifyThing o
                     -- for backwards compatibility accept rename of nonexistent files.
                     case mx of Nothing -> return ()
                                Just x -> do rmThing o
                                             makeThing n x
    mRemoveDirectory = rmThing
    mRemoveFile f = do x <- mReadFilePS f
                       when (B.length x /= 0) $
                            fail $ "Cannot remove non-empty file "++fn2fp f
                       rmThing f

identifyThing :: FileName -> HashedIO p (Maybe (ObjType,String))
identifyThing fn | fn' == fp2fn "" = do h <- gets rootHash
                                        return $ Just (D, h)
                 | otherwise = case breakOnDir fn' of
                               Nothing -> getany fn' `fmap` readroot
                               Just (d,fn'') -> do c <- readroot
                                                   case geta D d c of
                                                     Nothing -> return Nothing
                                                     Just h -> inh h $ identifyThing fn''
        where fn' = normPath fn

makeThing :: FileName -> (ObjType,String) -> HashedIO p ()
makeThing fn (o,h) = mWithCurrentDirectory (superName $ normPath fn) $
                     seta o (ownName $ normPath fn) h `fmap` readroot >>= writeroot

rmThing :: FileName -> HashedIO p ()
rmThing fn = mWithCurrentDirectory (superName $ normPath fn) $
             do c <- readroot
                let c' = filter (\(_,x,_)->x/= ownName (normPath fn)) c
                if length c' == length c - 1
                  then writeroot c'
                  else fail "obj doesn't exist in rmThing"

readhash :: String -> HashedIO p B.ByteString
readhash h = do c <- gets cache
                z <- lift $ unsafeInterleaveIO $ readHashFile c HashedPristineDir h
                let (_,out) = z
                return out

withh :: String -> HashedIO p a -> HashedIO p (String,a)
withh h j = do hd <- get
               put $ hd { rootHash = h }
               x <- j
               h' <- gets rootHash
               put hd
               return (h',x)

inh :: String -> HashedIO p a -> HashedIO p a
inh h j = snd `fmap` withh h j

readroot :: HashedIO p [(ObjType, FileName, String)]
readroot = do haveitalready <- peekroot
              cc <- gets rootHash >>= readdir
              unless haveitalready $ speculate cc
              return cc
    where speculate :: [(a,b,String)] -> HashedIO q ()
          speculate c = do cac <- gets cache
                           mapM_ (\(_,_,z) -> lift $ speculateFileUsingCache cac HashedPristineDir z) c
          peekroot :: HashedIO p Bool
          peekroot = do HashDir _ c h <- get
                        lift $ peekInCache c HashedPristineDir h

writeroot :: [(ObjType, FileName, String)] -> HashedIO p ()
writeroot c = do h <- writedir c
                 modify $ \hd -> hd { rootHash = h }

data ObjType = F | D deriving Eq

-- | @geta objtype name stuff@ tries to get an object of type @objtype@ named @name@
-- in @stuff@.
geta :: ObjType -> FileName -> [(ObjType, FileName, String)] -> Maybe String
geta o f c = do (o',h) <- getany f c
                if o == o' then Just h else Nothing

getany :: FileName -> [(ObjType, FileName, String)] -> Maybe (ObjType,String)
getany _ [] = Nothing
getany f ((o,f',h):_) | f == f' = Just (o,h)
getany f (_:r) = getany f r

seta :: ObjType -> FileName -> String -> [(ObjType, FileName, String)] -> [(ObjType, FileName, String)]
seta o f h [] = [(o,f,h)]
seta o f h ((_,f',_):r) | f == f' = (o,f,h):r
seta o f h (x:xs) = x : seta o f h xs

readdir :: String -> HashedIO p [(ObjType, FileName, String)]
readdir hash = (parsed . linesPS) `fmap` readhash hash
    where parsed (t:n:h:rest) | t == dir = (D, niceps2fn n, BC.unpack h) : parsed rest
                              | t == file = (F, niceps2fn n, BC.unpack h) : parsed rest
          parsed _ = []
dir :: B.ByteString
dir = BC.pack "directory:"
file :: B.ByteString
file = BC.pack "file:"


writedir :: [(ObjType, FileName, String)] -> HashedIO p String
writedir c = writeHashFile cps
    where cps = unlinesPS $ concatMap (\ (o,d,h) -> [showO o,fn2niceps d,BC.pack h]) c++[B.empty]
          showO D = dir
          showO F = file

writeHashFile :: B.ByteString -> HashedIO p String
writeHashFile ps = do c <- gets cache
                      -- pristine files are always compressed
                      lift $ writeFileUsingCache c GzipCompression HashedPristineDir ps


-- | Grab a whole pristine tree from a hash, and, if asked,
--   write files in the working copy.
copyHashed :: String -> Cache -> WithWorkingDir -> String -> IO ()
copyHashed k c wwd z = void . runStateT cph $ HashDir { permissions = RW, cache = c, rootHash = z }
    where cph = do cc <- readroot
                   lift $ tediousSize k (length cc)
                   mapM_ cp cc
          cp (F,n,h) = do
              ps <- readhash h
              lift $ finishedOneIO k (fn2fp n)
              case wwd of
                WithWorkingDir -> lift $ writeAtomicFilePS (fn2fp n) ps
                NoWorkingDir   -> ps `seq` return ()
                                  -- force evaluation of ps to actually copy hashed file
          cp (D,n,h) =
              if isMaliciousSubPath (fn2fp n)
                 then fail ("Caught malicious path: " ++ fn2fp n)
                 else do
                 lift $ finishedOneIO k (fn2fp n)
                 case wwd of
                   WithWorkingDir -> do
                     lift $ createDirectoryIfMissing False (fn2fp n)
                     lift $ withCurrentDirectory (fn2fp n) $ copyHashed k c WithWorkingDir h
                   NoWorkingDir ->
                     lift $ copyHashed k c NoWorkingDir h

-- | Returns a list of pairs (FilePath, (strict) ByteString) of
--   the pristine tree starting with the hash @root@.
--   @path@ should be either "." or end with "/"
--   Separator "/" is used since this function is used to generate
--   zip archives from pristine trees.
pathsAndContents :: FilePath -> Cache ->  String -> IO [(FilePath,B.ByteString)]
pathsAndContents path c root = evalStateT cph $ HashDir { permissions = RW, cache = c, rootHash = root }
    where cph = do cc <- readroot
                   pacs <- concat <$> mapM cp cc
                   let current = if path == "." then [] else [(path ++ "/" , B.empty)]
                   return $ current ++ pacs
          cp (F,n,h) = do
              ps <- readhash h
              let p = (if path == "." then "" else path ++ "/") ++ fn2fp n
              return [(p,ps)]
          cp (D,n,h) = do
              let p = (if path == "." then "" else path) ++ fn2fp n ++ "/"
              lift $ pathsAndContents p c h

copyPartialsHashed :: FilePathLike fp =>
                      Cache -> String -> [fp] -> IO ()
copyPartialsHashed c root = mapM_ (copyPartialHashed c root)

copyPartialHashed :: FilePathLike fp => Cache -> String -> fp -> IO ()
copyPartialHashed c root ff =
    do createDirectoryIfMissing True (basename $ toFilePath ff)
       void $ runStateT (cp $ fp2fn $ toFilePath ff)
                 HashDir { permissions = RW, cache = c,
                           rootHash = root }
 where basename = reverse . dropWhile ('/' /=) . dropWhile ('/' ==) . reverse
       cp f = do mt <- identifyThing f
                 case mt of
                   Just (D,h) -> do lift $ createDirectoryIfMissing True (fn2fp f)
                                    lift $ withCurrentDirectory (fn2fp f) $ copyHashed "" c WithWorkingDir h
                   Just (F,h) -> do ps <- readhash h
                                    lift $ writeAtomicFilePS (fn2fp f) ps
                   Nothing -> return ()

cleanHashdir :: Cache -> HashedDir -> [String] -> IO ()
cleanHashdir c dir_ hashroots =
   do -- we'll remove obsolete bits of "dir"
      debugMessage $ "Cleaning out " ++ hashedDir dir_ ++ "..."
      let hashdir = darcsdir ++ "/" ++ hashedDir dir_ ++ "/"
      hs <- set <$> getHashedFiles hashdir hashroots
      fs <- set . filter okayHash <$> getDirectoryContents hashdir
      mapM_ (removeFileMayNotExist . (hashdir++)) (unset $ fs `Set.difference` hs)
      -- and also clean out any global caches.
      debugMessage "Cleaning out any global caches..."
      cleanCachesWithHint c dir_ (unset $ fs `Set.difference` hs)
   where set = Set.fromList . map BC.pack
         unset = map BC.unpack . Set.toList

-- | getHashedFiles returns all hash files targeted by files in hashroots in
-- the hashdir directory.
getHashedFiles :: String -> [String] -> IO [String]
getHashedFiles hashdir hashroots = do
      let listone h = do let size = decodeDarcsSize $ BC.pack h
                             hash = decodeDarcsHash $ BC.pack h
                         x <- readDarcsHashedDir hashdir (size, hash)
                         let subs = [ fst $ darcsLocation "" (s, h') | (TreeType, _, s, h') <- x ]
                             hashes = h : [ fst $ darcsLocation "" (s, h') | (_, _, s, h') <- x ]
                         (hashes++) . concat <$> mapM listone subs
      hs <- concat <$> mapM listone hashroots
      return hs