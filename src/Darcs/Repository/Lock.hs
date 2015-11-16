-- Copyright (C) 2003 David Roundy
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
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}

module Darcs.Repository.Lock
    ( withLock
    , withLockCanFail
    , environmentHelpLocks
    , withTemp
    , withOpenTemp
    , withStdoutTemp
    , withTempDir
    , withPermDir
    , withDelayedDir
    , withNamedTemp
    , writeToFile
    , appendToFile
    , writeBinFile
    , writeLocaleFile
    , writeDocBinFile
    , appendBinFile
    , appendDocBinFile
    , readBinFile
    , readLocaleFile
    , readDocBinFile
    , writeAtomicFilePS
    , gzWriteAtomicFilePS
    , gzWriteAtomicFilePSs
    , gzWriteDocFile
    , rmRecursive
    , removeFileMayNotExist
    , canonFilename
    , maybeRelink
    , worldReadableTemp
    , tempdirLoc
    , environmentHelpTmpdir
    , environmentHelpKeepTmpdir
    , addToErrorLoc
    ) where

import Prelude hiding ( catch )
import Data.List ( inits )
import Data.Maybe ( isJust, listToMaybe )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( withBinaryFile, openBinaryTempFile,
                   hClose, hPutStr, Handle,
                   IOMode(WriteMode, AppendMode), hFlush, stdout )
import System.IO.Error
    ( isAlreadyExistsError
    , annotateIOError
    )
import Control.Exception
    ( IOException
    , bracket
    , throwIO
    , catch
    , try
    , SomeException
    )
import System.Directory ( removeFile, removeDirectory,
                   doesFileExist, doesDirectoryExist,
                   getDirectoryContents, createDirectory,
                   getTemporaryDirectory,
                 )
import System.FilePath.Posix ( splitDirectories )
import Control.Concurrent ( threadDelay )
import Control.Monad ( unless, when, liftM )

import Darcs.Util.URL ( isRelative )
import Darcs.Util.Environment ( maybeGetEnv )
import Darcs.Util.Exception
    ( firstJustIO
    , catchall
    )
import Darcs.Util.File ( withCurrentDirectory
                       , doesDirectoryReallyExist, removeFileMayNotExist )
import Darcs.Util.Path ( AbsolutePath, FilePathLike, toFilePath,
                        getCurrentDirectory, setCurrentDirectory )

import Darcs.Util.ByteString ( gzWriteFilePSs, decodeLocale, encodeLocale )
import qualified Data.ByteString as B (null, readFile, hPut, ByteString)
import qualified Data.ByteString.Char8 as BC (unpack)

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Printer ( Doc, hPutDoc, packedString, empty, renderPSs, RenderMode(..) )
import Darcs.Util.AtExit ( atexit )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Workaround ( renameFile )
import Darcs.Repository.Compat
    ( mkStdoutTemp
    , canonFilename
    , maybeRelink
    , atomicCreate
    , sloppyAtomicCreate
    )
import System.Posix.Files ( fileMode, getFileStatus, setFileMode )
#include "impossible.h"

withLock :: String -> IO a -> IO a
withLock s job = bracket (getlock s 30) releaseLock (\_ -> job)

releaseLock :: String -> IO ()
releaseLock = removeFileMayNotExist

-- | Tries to perform some task if it can obtain the lock,
-- Otherwise, just gives up without doing the task
withLockCanFail :: String -> IO a -> IO (Either () a)
withLockCanFail s job =
  bracket (takeLock s)
          (\l -> when l $ releaseLock s)
          (\l -> if l then liftM Right job
                      else return $ Left ())

getlock :: String -> Int -> IO String
getlock l 0 = do putStrLn $ "Couldn't get lock "++l
                 exitWith $ ExitFailure 1
getlock lbad tl = do l <- canonFilename lbad
                     gotit <- takeLock l
                     if gotit then return l
                              else do putStrLn $ "Waiting for lock "++l
                                      hFlush stdout -- for Windows
                                      threadDelay 2000000
                                      getlock l (tl - 1)


takeLock :: FilePathLike p => p -> IO Bool
takeLock fp =
    do atomicCreate $ toFilePath fp
       return True
  `catch` \e -> if isAlreadyExistsError e
                then return False
                else do pwd <- getCurrentDirectory
                        throwIO $ addToErrorLoc e
                                   ("takeLock "++toFilePath fp++" in "++toFilePath pwd)

takeFile :: FilePath -> IO Bool
takeFile fp =
    do sloppyAtomicCreate fp
       return True
  `catch` \e -> if isAlreadyExistsError e
                then return False
                else do pwd <- getCurrentDirectory
                        throwIO $ addToErrorLoc e
                                   ("takeFile "++fp++" in "++toFilePath pwd)

environmentHelpLocks :: ([String],[String])
environmentHelpLocks = (["DARCS_SLOPPY_LOCKS"],[
 "If on some filesystems you get an error of the kind:",
 "",
 "    darcs: takeLock [...]: atomic_create [...]: unsupported operation",
 "",
 "you may want to try to export DARCS_SLOPPY_LOCKS=True."])

-- |'withTemp' safely creates an empty file (not open for writing) and
-- returns its name.
--
-- The temp file operations are rather similar to the locking operations, in
-- that they both should always try to clean up, so exitWith causes trouble.
withTemp :: (String -> IO a) -> IO a
withTemp = bracket get_empty_file removeFileMayNotExist
    where get_empty_file = do (f,h) <- openBinaryTempFile "." "darcs"
                              hClose h
                              return f

-- |'withOpenTemp' creates a temporary file, and opens it.
-- Both of them run their argument and then delete the file.  Also,
-- both of them (to my knowledge) are not susceptible to race conditions on
-- the temporary file (as long as you never delete the temporary file; that
-- would reintroduce a race condition).
withOpenTemp :: ((Handle, String) -> IO a) -> IO a
withOpenTemp = bracket get_empty_file cleanup
    where cleanup (h,f) = do _ <- try (hClose h) :: IO (Either SomeException ())
                             removeFileMayNotExist f
          get_empty_file = invert `fmap` openBinaryTempFile "." "darcs"
          invert (a,b) = (b,a)

withStdoutTemp :: (String -> IO a) -> IO a
withStdoutTemp = bracket (mkStdoutTemp "stdout_") removeFileMayNotExist

tempdirLoc :: IO FilePath
tempdirLoc = liftM fromJust $
    firstJustIO [ liftM (Just . head . words) (readBinFile (darcsdir++"/prefs/tmpdir")) >>= chkdir,
                  maybeGetEnv "DARCS_TMPDIR" >>= chkdir,
                  getTemporaryDirectory >>= chkdir . Just,
                  getCurrentDirectorySansDarcs,
                  return $ Just "."  -- always returns a Just
                ]
    where chkdir Nothing = return Nothing
          chkdir (Just d) = liftM (\e -> if e then Just (d++"/") else Nothing) $ doesDirectoryExist d

environmentHelpTmpdir :: ([String], [String])
environmentHelpTmpdir = (["DARCS_TMPDIR", "TMPDIR"], [
 "Darcs often creates temporary directories.  For example, the `darcs",
 "diff` command creates two for the working trees to be diffed.  By",
 "default temporary directories are created in /tmp, or if that doesn't",
 "exist, in _darcs (within the current repo).  This can be overridden by",
 "specifying some other directory in the file _darcs/prefs/tmpdir or the",
 "environment variable $DARCS_TMPDIR or $TMPDIR."])

getCurrentDirectorySansDarcs :: IO (Maybe FilePath)
getCurrentDirectorySansDarcs = do
  c <- getCurrentDirectory
  return $ listToMaybe $ drop 5 $ reverse $ takeWhile no_darcs $ inits $ toFilePath c
  where no_darcs x = darcsdir `notElem` splitDirectories x

data WithDirKind = Perm | Temp | Delayed

-- | Creates a directory based on the path parameter;
-- if a relative path is given the dir is created in the darcs temp dir.
-- If an absolute path is given this dir will be created if it doesn't exist.
-- If it is specified as a temporary dir, it is deleted after finishing the job.
withDir :: WithDirKind  -- specifies if and when directory will be deleted
        -> String       -- path parameter
        -> (AbsolutePath -> IO a) -> IO a
withDir _ "" _ = bug "withDir called with empty directory name"
withDir kind absoluteOrRelativeName job = do
  absoluteName <- if isRelative absoluteOrRelativeName
                   then fmap (++ absoluteOrRelativeName) tempdirLoc
                   else return absoluteOrRelativeName
  formerdir <- getCurrentDirectory
  bracket (createDir absoluteName 0)
          (\dir -> do setCurrentDirectory formerdir
                      k <- keepTempDir
                      unless k $ case kind of
                                   Perm -> return ()
                                   Temp -> rmRecursive (toFilePath dir)
                                   Delayed -> atexit $ rmRecursive (toFilePath dir))
          job
    where newname name 0 = name
          newname name n = name ++ "-" ++ show n
          createDir :: FilePath -> Int -> IO AbsolutePath
          createDir name n
              = do createDirectory $ newname name n
                   setCurrentDirectory $ newname name n
                   getCurrentDirectory
                `catch` (\e -> if isAlreadyExistsError e
                               then createDir name (n+1)
                               else throwIO e)
          keepTempDir = isJust `fmap` maybeGetEnv "DARCS_KEEP_TMPDIR"

environmentHelpKeepTmpdir :: ([String], [String])
environmentHelpKeepTmpdir = (["DARCS_KEEP_TMPDIR"],[
 "If the environment variable DARCS_KEEP_TMPDIR is defined, darcs will",
 "not remove the temporary directories it creates.  This is intended",
 "primarily for debugging Darcs itself, but it can also be useful, for",
 "example, to determine why your test preference (see `darcs setpref`)",
 "is failing when you run `darcs record`, but working when run manually."])

-- |'withPermDir' is like 'withTempDir', except that it doesn't
-- delete the directory afterwards.
withPermDir :: String -> (AbsolutePath -> IO a) -> IO a
withPermDir = withDir Perm

-- |'withTempDir' creates an empty directory and then removes it when it
-- is no longer needed.  withTempDir creates a temporary directory.  The
-- location of that directory is determined by the contents of
-- _darcs/prefs/tmpdir, if it exists, otherwise by @$DARCS_TMPDIR@, and if
-- that doesn't exist then whatever your operating system considers to be a
-- a temporary directory (e.g. @$TMPDIR@ under Unix, @$TEMP@ under
-- Windows).
--
-- If none of those exist it creates the temporary directory
-- in the current directory, unless the current directory is under a _darcs
-- directory, in which case the temporary directory in the parent of the highest
-- _darcs directory to avoid accidentally corrupting darcs's internals.
-- This should not fail, but if it does indeed fail, we go ahead and use the
-- current directory anyway. If @$DARCS_KEEP_TMPDIR@ variable is set
-- temporary directory is not removed, this can be useful for debugging.
withTempDir :: String -> (AbsolutePath -> IO a) -> IO a
withTempDir = withDir Temp

withDelayedDir :: String -> (AbsolutePath -> IO a) -> IO a
withDelayedDir = withDir Delayed

rmRecursive :: FilePath -> IO ()
rmRecursive d =
    do isd <- doesDirectoryReallyExist d
       if not isd
          then removeFile d
          else do conts <- actual_dir_contents
                  withCurrentDirectory d $
                    mapM_ rmRecursive conts
                  removeDirectory d
    where actual_dir_contents = -- doesn't include . or ..
              do c <- getDirectoryContents d
                 return $ filter (/=".") $ filter (/="..") c

worldReadableTemp :: String -> IO String
worldReadableTemp f = wrt 0
    where wrt :: Int -> IO String
          wrt 100 = fail $ "Failure creating temp named "++f
          wrt n = let f_new = f++"-"++show n
                  in do ok <- takeFile f_new
                        if ok then return f_new
                              else wrt (n+1)

withNamedTemp :: String -> (String -> IO a) -> IO a
withNamedTemp n = bracket get_empty_file removeFileMayNotExist
    where get_empty_file = worldReadableTemp n

readBinFile :: FilePathLike p => p -> IO String
readBinFile = fmap BC.unpack . B.readFile . toFilePath

-- | Reads a file. Differs from readBinFile in that it interprets the file in
--   the current locale instead of as ISO-8859-1.
readLocaleFile :: FilePathLike p => p -> IO String
readLocaleFile f = decodeLocale `fmap` B.readFile (toFilePath f)

readDocBinFile :: FilePathLike p => p -> IO Doc
readDocBinFile fp = do ps <- B.readFile $ toFilePath fp
                       return $ if B.null ps then empty else packedString ps

appendBinFile :: FilePathLike p => p -> String -> IO ()
appendBinFile f s = appendToFile f $ \h -> hPutStr h s

appendDocBinFile :: FilePathLike p => p -> Doc -> IO ()
appendDocBinFile f d = appendToFile f $ \h -> hPutDoc Standard h d

writeBinFile :: FilePathLike p => p -> String -> IO ()
writeBinFile f s = writeToFile f $ \h -> hPutStr h s

-- | Writes a file. Differs from writeBinFile in that it writes the string
--   encoded with the current locale instead of what GHC thinks is right.
writeLocaleFile :: FilePathLike p => p -> String -> IO ()
writeLocaleFile f s = writeToFile f $ \h -> B.hPut h (encodeLocale s)

writeDocBinFile :: FilePathLike p => p -> Doc -> IO ()
writeDocBinFile f d = writeToFile f $ \h -> hPutDoc Standard h d

writeAtomicFilePS :: FilePathLike p => p -> B.ByteString -> IO ()
writeAtomicFilePS f ps = writeToFile f $ \h -> B.hPut h ps

gzWriteAtomicFilePS :: FilePathLike p => p -> B.ByteString -> IO ()
gzWriteAtomicFilePS f ps = gzWriteAtomicFilePSs f [ps]

gzWriteAtomicFilePSs :: FilePathLike p => p -> [B.ByteString] -> IO ()
gzWriteAtomicFilePSs f pss =
    withSignalsBlocked $ withNamedTemp (toFilePath f) $ \newf -> do
    gzWriteFilePSs newf pss
    already_exists <- doesFileExist $ toFilePath f
    when already_exists $ do mode <- fileMode `fmap` getFileStatus (toFilePath f)
                             setFileMode newf mode
             `catchall` return ()
    renameFile newf (toFilePath f)

gzWriteDocFile :: FilePathLike p => p -> Doc -> IO ()
gzWriteDocFile f d = gzWriteAtomicFilePSs f $ renderPSs Standard d

writeToFile :: FilePathLike p => p -> (Handle -> IO ()) -> IO ()
writeToFile f job =
    withSignalsBlocked $ withNamedTemp (toFilePath f) $ \newf -> do
    withBinaryFile newf WriteMode job
    already_exists <- doesFileExist (toFilePath f)
    when already_exists $ do mode <- fileMode `fmap` getFileStatus (toFilePath f)
                             setFileMode newf mode
             `catchall` return ()
    renameFile newf (toFilePath f)

appendToFile :: FilePathLike p => p -> (Handle -> IO ()) -> IO ()
appendToFile f job = withSignalsBlocked $
    withBinaryFile (toFilePath f) AppendMode job


addToErrorLoc :: IOException
              -> String
              -> IOException
addToErrorLoc ioe s = annotateIOError ioe s Nothing Nothing
