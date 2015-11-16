-- Copyright (C) 2007 Eric Kow
-- Copyright (C) 2010 Petr Rockai
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
module Darcs.Util.Path
    ( module Storage.Hashed.AnchoredPath
    , FileName( )
    , fp2fn
    , fn2fp
    , fn2ps
    , ps2fn
    , niceps2fn
    , fn2niceps
    , breakOnDir
    , normPath
    , ownName
    , superName
    , movedirfilename
    , encodeWhite
    , decodeWhite
    , isParentOrEqOf
    -- * AbsolutePath
    , AbsolutePath
    , makeAbsolute
    , ioAbsolute
    , rootDirectory
    -- * AbsolutePathOrStd
    , AbsolutePathOrStd
    , makeAbsoluteOrStd
    , ioAbsoluteOrStd
    , useAbsoluteOrStd
    , stdOut
    -- * AbsoluteOrRemotePath
    , AbsoluteOrRemotePath
    , ioAbsoluteOrRemote
    , isRemote
    -- * SubPath
    , SubPath
    , makeSubPathOf
    , simpleSubPath
    , isSubPathOf
    , floatSubPath
    -- * Miscellaneous
    , sp2fn
    , FilePathOrURL(..)
    , FilePathLike(toFilePath)
    , getCurrentDirectory
    , setCurrentDirectory
    , getUniquePathName
    , doesPathExist
    -- * Check for malicious paths
    , isMaliciousPath
    , isMaliciousSubPath
    -- * Tree filtering.
    , filterFilePaths
    , filterPaths
    ) where

import Storage.Hashed.AnchoredPath

import Control.Applicative ( (<$>) )
import Data.List
    ( isPrefixOf
    , isSuffixOf
    , stripPrefix
    , intersect
    )
import Data.Char ( isSpace, chr, ord )
import Control.Exception ( tryJust, bracket_ )
import Control.Monad ( when )
import System.IO.Error ( isDoesNotExistError )

import qualified Darcs.Util.Workaround as Workaround ( getCurrentDirectory )
import qualified System.Directory ( setCurrentDirectory )
import System.Directory ( doesDirectoryExist, doesFileExist )
import qualified System.FilePath.Posix as FilePath ( normalise )
import qualified System.FilePath as NativeFilePath ( takeFileName, takeDirectory )
import System.FilePath ( splitDirectories )
import System.Posix.Files ( isDirectory, getSymbolicLinkStatus )

import Darcs.Util.ByteString ( packStringToUTF8, unpackPSFromUTF8 )
import qualified Data.ByteString.Char8 as BC (unpack, pack)
import qualified Data.ByteString       as B  (ByteString)
import Data.Binary

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isAbsolute, isRelative, isSshNopath )

#include "impossible.h"

-- | FileName is an abstract type intended to facilitate the input and output of
-- unicode filenames.
newtype FileName = FN FilePath deriving ( Eq, Ord )

instance Show FileName where
   showsPrec d (FN fp) = showParen (d > appPrec) $ showString "fp2fn " . showsPrec (appPrec + 1) fp
      where appPrec = 10

instance Binary FileName where
  put (FN h) = put h
  get = FN `fmap` get

{-# INLINE fp2fn #-}
fp2fn :: FilePath -> FileName
fp2fn = FN

{-# INLINE fn2fp #-}
fn2fp :: FileName -> FilePath
fn2fp (FN fp) = fp

{-# INLINE niceps2fn #-}
niceps2fn :: B.ByteString -> FileName
niceps2fn = FN . decodeWhite . BC.unpack

{-# INLINE fn2niceps #-}
fn2niceps :: FileName -> B.ByteString
fn2niceps (FN fp) = BC.pack $ encodeWhite fp

{-# INLINE fn2ps #-}
fn2ps :: FileName -> B.ByteString
fn2ps (FN fp) = packStringToUTF8 $ encodeWhite fp

{-# INLINE ps2fn #-}
ps2fn :: B.ByteString -> FileName
ps2fn ps = FN $ decodeWhite $ unpackPSFromUTF8 ps

{-# INLINE sp2fn #-}
sp2fn :: SubPath -> FileName
sp2fn = fp2fn . toFilePath

-- | 'encodeWhite' translates whitespace in filenames to a darcs-specific
--   format (numerical representation according to 'ord' surrounded by
--   backslashes).  Note that backslashes are also escaped since they are used
--   in the encoding.
--
--   > encodeWhite "hello there" == "hello\32\there"
--   > encodeWhite "hello\there" == "hello\92\there"
encodeWhite :: FilePath -> String
encodeWhite (c:cs) | isSpace c || c == '\\' =
    '\\' : show (ord c) ++ "\\" ++ encodeWhite cs
encodeWhite (c:cs) = c : encodeWhite cs
encodeWhite [] = []

-- | 'decodeWhite' interprets the Darcs-specific \"encoded\" filenames
--   produced by 'encodeWhite'
--
--   > decodeWhite "hello\32\there"  == "hello there"
--   > decodeWhite "hello\92\there"  == "hello\there"
--   > decodeWhite "hello\there"   == error "malformed filename"
decodeWhite :: String -> FilePath
decodeWhite cs_ = go cs_ [] False
 where go "" acc True  = reverse acc -- if there was a replace, use new string
       go "" _   False = cs_         -- if not, use input string
       go ('\\':cs) acc _ =
         case break (=='\\') cs of
           (theord, '\\':rest) ->
             go rest (chr (read theord) :acc) True
           _ -> error "malformed filename"
       go (c:cs) acc modified = go cs (c:acc) modified

ownName :: FileName -> FileName
ownName (FN f) =  case breakLast '/' f of Nothing -> FN f
                                          Just (_,f') -> FN f'
superName :: FileName -> FileName
superName fn = case normPath fn of
                FN f -> case breakLast '/' f of
                        Nothing -> FN "."
                        Just (d,_) -> FN d
breakOnDir :: FileName -> Maybe (FileName,FileName)
breakOnDir (FN p) = case breakFirst '/' p of
                      Nothing -> Nothing
                      Just (d,f) | d == "." -> breakOnDir $ FN f
                                 | otherwise -> Just (FN d, FN f)

-- | convert a path string into a sequence of directories strings
--   "/", "." and ".." are generally interpreted as expected.
--   Behaviour with too many '..' is to leave them.
--
--   Examples:
--     Splitting:
--       "aa/bb/cc"       -> ["aa","bb","cc"]
--     Ignoring "." and extra "/":
--       "aa/./bb"        -> ["aa","bb"]
--       "aa//bb"         -> ["aa","bb"]
--       "/aa/bb/"        -> ["aa","bb"]
--     Handling "..":
--       "aa/../bb/cc"    -> ["bb","cc"]
--       "aa/bb/../../cc" -> ["cc"]
--       "aa/../bb/../cc" -> ["cc"]
--       "../cc"          -> ["..","cc"]
normPath :: FileName -> FileName
normPath (FN p) = FN $ norm p

norm :: String -> String
norm ('.':'/':s) = norm s
norm ('/':s)     = norm s
norm "."         = ""
norm s = go s [] False
 where go "" _   False = s           -- no modification
       go "" acc True  = reverse acc
       go ('/':r)         acc _ | sep r = go r acc True
       go ('/':'.':r)     acc _ | sep r = go r acc True
       go ('/':'.':'.':r) acc _ | sep r = go r (doDotDot acc) True
       go (c:s') acc changed = go s' (c:acc) changed
       -- remove last path or add "/.." if impossible
       doDotDot ""                       = ".."
       doDotDot acc@('.':'.':r) | sep r  = '.':'.':'/':acc
       doDotDot acc = let a' = dropWhile (/='/') acc in -- eat dir
                       if null a' then "" else tail a'
       -- check if is a path separator
       sep ('/':_) = True
       sep []      = True -- end of string is considered separator
       sep _       = False

breakFirst :: Char -> String -> Maybe (String,String)
breakFirst c = bf []
    where bf a (r:rs) | r == c = Just (reverse a,rs)
                      | otherwise = bf (r:a) rs
          bf _ [] = Nothing
breakLast :: Char -> String -> Maybe (String,String)
breakLast c l = case breakFirst c (reverse l) of
                Nothing -> Nothing
                Just (a,b) -> Just (reverse b, reverse a)

isParentOrEqOf :: FileName -> FileName -> Bool
isParentOrEqOf fn1 fn2 = case stripPrefix (fn2fp fn1) (fn2fp fn2) of
    Just ('/' : _) -> True
    Just [] -> True
    _ -> False

movedirfilename :: FileName -> FileName -> FileName -> FileName
movedirfilename old new name =
    if name' == old'
        then new
        else case stripPrefix old' name' of
            Just rest@('/':_) -> fp2fn $ "./" ++ new' ++ rest
            _ -> name
        where old' = fn2fp $ normPath old
              new' = fn2fp $ normPath new
              name' = fn2fp $ normPath name


class FilePathOrURL a where
 toPath :: a -> String

class FilePathOrURL a => FilePathLike a where
 toFilePath :: a -> FilePath

-- | Paths which are relative to the local darcs repository and normalized.
-- Note: These are understood not to have the dot in front.
newtype SubPath      = SubPath FilePath deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath FilePath deriving (Eq, Ord)

-- | This is for situations where a string (e.g. a command line argument)
-- may take the value \"-\" to mean stdin or stdout (which one depends on
-- context) instead of a normal file path.
data AbsolutePathOrStd = AP AbsolutePath | APStd deriving (Eq, Ord)
data AbsoluteOrRemotePath = AbsP AbsolutePath | RmtP String deriving (Eq, Ord)

instance FilePathOrURL AbsolutePath where
 toPath (AbsolutePath x) = x
instance FilePathOrURL SubPath where
 toPath (SubPath x) = x
instance CharLike c => FilePathOrURL [c] where
 toPath = toFilePath

instance FilePathOrURL AbsoluteOrRemotePath where
 toPath (AbsP a) = toPath a
 toPath (RmtP r) = r

instance FilePathOrURL FileName where
    toPath = fn2fp
instance FilePathLike FileName where
    toFilePath = fn2fp

instance FilePathLike AbsolutePath where
 toFilePath (AbsolutePath x) = x
instance FilePathLike SubPath where
 toFilePath (SubPath x) = x

class CharLike c where
    toChar :: c -> Char
    fromChar :: Char -> c
instance CharLike Char where
    toChar = id
    fromChar = id

instance CharLike c => FilePathLike [c] where
    toFilePath = map toChar

-- | Make the second path relative to the first, if possible
makeSubPathOf :: AbsolutePath -> AbsolutePath -> Maybe SubPath
makeSubPathOf (AbsolutePath p1) (AbsolutePath p2) =
 -- The slash prevents "foobar" from being treated as relative to "foo"
 if p1 == p2 || (p1 ++ "/") `isPrefixOf` p2
    then Just $ SubPath $ drop (length p1 + 1) p2
    else Nothing

simpleSubPath :: FilePath -> Maybe SubPath
simpleSubPath x | null x = bug "simpleSubPath called with empty path"
                | isRelative x = Just $ SubPath $ FilePath.normalise $ pathToPosix x
                | otherwise = Nothing

isSubPathOf :: SubPath -> SubPath -> Bool
isSubPathOf (SubPath p1) (SubPath p2) =
    p1 == "" || p1 == p2 || (p1 ++ "/") `isPrefixOf` p2

-- | Ensure directory exists and is not a symbolic link.
doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = do
    x <- tryJust (\x -> if isDoesNotExistError x then Just () else Nothing) $
        isDirectory <$> getSymbolicLinkStatus f
    return $ case x of
        Left () -> False
        Right y -> y

doesPathExist :: FilePath -> IO Bool
doesPathExist p = do
   dir_exists <- doesDirectoryExist p
   file_exists <- doesFileExist p
   return $ dir_exists || file_exists

-- | Interpret a possibly relative path wrt the current working directory.
ioAbsolute :: FilePath -> IO AbsolutePath
ioAbsolute dir =
    do isdir <- doesDirectoryReallyExist dir
       here <- getCurrentDirectory
       if isdir
         then bracket_ (setCurrentDirectory dir)
                       (setCurrentDirectory $ toFilePath here)
                       getCurrentDirectory
         else let super_dir = case NativeFilePath.takeDirectory dir of
                                "" ->  "."
                                d  -> d
                  file = NativeFilePath.takeFileName dir
              in do abs_dir <- if dir == super_dir
                               then return $ AbsolutePath dir
                               else ioAbsolute super_dir
                    return $ makeAbsolute abs_dir file

-- | Take an absolute path and a string representing a (possibly relative)
-- path and combine them into an absolute path. If the second argument is
-- already absolute, then the first argument gets ignored. This function also
-- takes care that the result is converted to Posix convention and
-- normalized. Also, parent directories (\"..\") at the front of the string
-- argument get canceled out against trailing directory parts of the
-- absolute path argument.
--
-- Regarding the last point, someone more familiar with how these functions
-- are used should verify that this is indeed necessary or at least useful.
makeAbsolute :: AbsolutePath -> FilePath -> AbsolutePath
makeAbsolute a dir = if not (null dir) && isAbsolute dir
                     then AbsolutePath (normSlashes dir')
                     else ma a dir'
  where
    dir' = FilePath.normalise $ pathToPosix dir
    -- Why do we care to reduce ".." here?
    -- Why not do this throughout the whole path, i.e. "x/y/../z" -> "x/z" ?
    ma here ('.':'.':'/':r) = ma (takeDirectory here) r
    ma here ".." = takeDirectory here
    ma here "." = here
    ma here "" = here
    ma here r = here /- ('/':r)

(/-) :: AbsolutePath -> String -> AbsolutePath
x /- ('/':r) = x /- r
(AbsolutePath "/") /- r = AbsolutePath ('/':simpleClean r)
(AbsolutePath x) /- r = AbsolutePath (x++'/':simpleClean r)

-- | Convert to posix, remove trailing slashes, and (under Posix)
-- reduce multiple leading slashes to one.
simpleClean :: String -> String
simpleClean = normSlashes . reverse . dropWhile (=='/') . reverse . pathToPosix

-- | The root directory as an absolute path.
rootDirectory :: AbsolutePath
rootDirectory = AbsolutePath "/"

makeAbsoluteOrStd :: AbsolutePath -> String -> AbsolutePathOrStd
makeAbsoluteOrStd _ "-" = APStd
makeAbsoluteOrStd a p = AP $ makeAbsolute a p

stdOut :: AbsolutePathOrStd
stdOut = APStd

ioAbsoluteOrStd :: String -> IO AbsolutePathOrStd
ioAbsoluteOrStd "-" = return APStd
ioAbsoluteOrStd p = AP `fmap` ioAbsolute p

-- | Execute either the first or the second argument action, depending on
-- whether the given path is an 'AbsolutePath' or stdin/stdout.
useAbsoluteOrStd :: (AbsolutePath -> a) -> a -> AbsolutePathOrStd -> a
useAbsoluteOrStd _ f APStd = f
useAbsoluteOrStd f _ (AP x) = f x

ioAbsoluteOrRemote :: String -> IO AbsoluteOrRemotePath
ioAbsoluteOrRemote p = do
  isdir <- doesDirectoryExist p
  if not isdir
     then return $ RmtP $
          case () of _ | isSshNopath p    -> p++"."
                       | "/" `isSuffixOf` p -> init p
                       | otherwise          -> p
     else AbsP `fmap` ioAbsolute p

isRemote :: AbsoluteOrRemotePath -> Bool
isRemote (RmtP _) = True
isRemote _ = False

takeDirectory :: AbsolutePath -> AbsolutePath
takeDirectory (AbsolutePath x) =
    case reverse $ drop 1 $ dropWhile (/='/') $ reverse x of
    "" -> AbsolutePath "/"
    x' -> AbsolutePath x'

instance Show AbsolutePath where
 show = show . toFilePath
instance Show SubPath where
 show = show . toFilePath
instance Show AbsolutePathOrStd where
    show (AP a) = show a
    show APStd = "standard input/output"
instance Show AbsoluteOrRemotePath where
    show (AbsP a) = show a
    show (RmtP r) = show r

-- | Normalize the path separator to Posix style (slash, not backslash).
-- This only affects Windows systems.
pathToPosix :: FilePath -> FilePath
pathToPosix = map convert where
#ifdef WIN32
  convert '\\' = '/'
#endif
  convert c = c

-- | Reduce multiple leading slashes to one. This only affects Posix systems.
normSlashes :: FilePath -> FilePath
#ifndef WIN32
-- multiple slashes in front are ignored under Posix
normSlashes ('/':p) = '/' : dropWhile (== '/') p
#endif
normSlashes p = p

getCurrentDirectory :: IO AbsolutePath
getCurrentDirectory = AbsolutePath `fmap` Workaround.getCurrentDirectory

setCurrentDirectory :: FilePathLike p => p -> IO ()
setCurrentDirectory = System.Directory.setCurrentDirectory . toFilePath

{-|
  What is a malicious path?

  A spoofed path is a malicious path.

  1. Darcs only creates explicitly relative paths (beginning with @\".\/\"@),
     so any not explicitly relative path is surely spoofed.

  2. Darcs normalizes paths so they never contain @\"\/..\/\"@, so paths with
     @\"\/..\/\"@ are surely spoofed.

  A path to a darcs repository's meta data can modify \"trusted\" patches or
  change safety defaults in that repository, so we check for paths
  containing @\"\/_darcs\/\"@ which is the entry to darcs meta data.

  To do?

  * How about get repositories?

  * Would it be worth adding a --semi-safe-paths option for allowing
    changes to certain preference files (_darcs\/prefs\/) in sub
    repositories'?
-}
isMaliciousPath :: String -> Bool
isMaliciousPath fp =
    not (isExplicitlyRelative fp) || isGenerallyMalicious fp

-- | Warning : this is less rigorous than isMaliciousPath
--   but it's to allow for subpath representations that
--   don't start with ./
isMaliciousSubPath :: String -> Bool
isMaliciousSubPath fp =
    not (isRelative fp) || isGenerallyMalicious fp

isGenerallyMalicious :: String -> Bool
isGenerallyMalicious fp =
    splitDirectories fp `contains_any` [ "..", darcsdir ]
 where
    contains_any a b = not . null $ intersect a b


isExplicitlyRelative :: String -> Bool
isExplicitlyRelative ('.':'/':_) = True  -- begins with "./"
isExplicitlyRelative _ = False

-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath]
            -> AnchoredPath
            -> t
            -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files


-- | Same as 'filterPath', but for ordinary 'FilePath's (as opposed to
-- AnchoredPath).
filterFilePaths :: [FilePath]
                -> AnchoredPath
                -> t
                -> Bool
filterFilePaths = filterPaths . map floatPath

-- | Iteratively tries find first non-existing path generated by
-- buildName, it feeds to buildName the number starting with -1.  When
-- it generates non-existing path and it isn't first, it displays the
-- message created with buildMsg. Usually used for generation of the
-- name like <path>_<number> when <path> already exist
-- (e.g. darcs.net_0).
getUniquePathName :: Bool -> (FilePath -> String) -> (Int -> FilePath) -> IO FilePath
getUniquePathName talkative buildMsg buildName = go (-1)
 where
  go :: Int -> IO FilePath
  go i = do
    exists <- doesPathExist thename
    if not exists
       then do when (i /= -1 && talkative) $ putStrLn $ buildMsg thename
               return thename
       else go $ i+1
    where thename = buildName i

-- | Transform a SubPath into an AnchoredPath.
floatSubPath :: SubPath -> AnchoredPath
floatSubPath = floatPath . fn2fp . sp2fn

