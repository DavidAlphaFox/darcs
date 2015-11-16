{-# LANGUAGE CPP #-}
module Darcs.Util.File
    (
    -- * Files and directories
      getFileStatus
    , withCurrentDirectory
    , doesDirectoryReallyExist
    , removeFileMayNotExist
    -- * OS-dependent special directories
    , xdgCacheDir
    , osxCacheDir
    , getDirectoryContents
    , getRecursiveContents
    , getRecursiveContentsFullPath
    ) where

import Prelude hiding ( catch )

import Control.Exception ( catch, bracket )
import Control.Monad ( when, unless, forM )

import System.Environment ( getEnvironment )
import System.Directory ( removeFile, getHomeDirectory,
                          getAppUserDataDirectory, doesDirectoryExist,
                          createDirectory, getDirectoryContents )
import System.IO.Error ( isDoesNotExistError )
import System.Posix.Files( getSymbolicLinkStatus, FileStatus, isDirectory
#ifndef WIN32
                         , setFileMode, ownerModes
#endif
                         )
import System.FilePath.Posix ( (</>) )

import Darcs.Util.Exception ( catchall )
import Darcs.Util.Path( FilePathLike, getCurrentDirectory, setCurrentDirectory, toFilePath )

withCurrentDirectory :: FilePathLike p
                     => p
                     -> IO a
                     -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)

getFileStatus :: FilePath
              -> IO (Maybe FileStatus)
getFileStatus f =
  Just `fmap` getSymbolicLinkStatus f `catchall` return Nothing

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f =
    catchNonExistence (isDirectory `fmap` getSymbolicLinkStatus f) False

removeFileMayNotExist :: FilePathLike p => p -> IO ()
removeFileMayNotExist f = catchNonExistence (removeFile $ toFilePath f) ()

catchNonExistence :: IO a -> a -> IO a
catchNonExistence job nonexistval =
    catch job $
    \e -> if isDoesNotExistError e then return nonexistval
                                   else ioError e

-- |osxCacheDir assumes @~/Library/Caches/@ exists.
osxCacheDir :: IO (Maybe FilePath)
osxCacheDir = do
    home <- getHomeDirectory
    return $ Just $ home </> "Library" </> "Caches"
    `catchall` return Nothing

-- |xdgCacheDir returns the $XDG_CACHE_HOME environment variable,
-- or @~/.cache@ if undefined. See the FreeDesktop specification:
-- http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
xdgCacheDir :: IO (Maybe FilePath)
xdgCacheDir = do
    env <- getEnvironment
    d <- case lookup "XDG_CACHE_HOME" env of
           Just d  -> return d
           Nothing -> getAppUserDataDirectory "cache"
    exists <- doesDirectoryExist d

    -- If directory does not exist, create it with permissions 0700
    -- as specified by the FreeDesktop standard.
    unless exists $ do createDirectory d
#ifndef WIN32
    -- see http://bugs.darcs.net/issue2334
                       setFileMode d ownerModes
#endif
    return $ Just d
    `catchall` return Nothing

-- |getRecursiveContents returns all files under topdir that aren't
-- directories.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContents path
      else return [name]
  return (concat paths)

-- |getRecursiveContentsFullPath returns all files under topdir
-- that aren't directories.
-- Unlike getRecursiveContents this function returns the full path.
getRecursiveContentsFullPath :: FilePath -> IO [FilePath]
getRecursiveContentsFullPath topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContentsFullPath path
      else return [path]
  return (concat paths)
