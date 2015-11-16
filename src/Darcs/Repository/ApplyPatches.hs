-- Copyright (C) 2002-2005,2007 David Roundy
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

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Darcs.Repository.ApplyPatches
    ( applyPatches
    , runTolerantly
    , runSilently
    , DefaultIO, runDefault
    ) where

import Prelude hiding ( catch )
import Control.Exception ( catch, SomeException, IOException )
import Data.Char ( toLower )
import Data.List ( isSuffixOf )
import System.IO ( stderr )
import System.IO.Error ( isDoesNotExistError, isPermissionError )
import Control.Monad ( unless, mplus )
import Control.Applicative (Applicative)
import System.Directory ( createDirectory,
                          removeDirectory, removeFile,
                          renameFile, renameDirectory,
                          doesDirectoryExist, doesFileExist
                        )

import Darcs.Patch.ApplyMonad( ApplyMonad(..) )
import Darcs.Patch.ApplyPatches ( applyPatches )
import Darcs.Patch.MonadProgress ( MonadProgress(..), ProgressAction(..) )
import Darcs.Repository.Prefs( changePrefval )
import Darcs.Repository.Lock ( writeAtomicFilePS )

import Darcs.Util.Exception ( prettyException )
import Darcs.Util.Progress ( beginTedious, endTedious, tediousSize, finishedOneIO )
import Darcs.Util.Printer ( hPutDocLn, RenderMode(..) )
import Darcs.Util.Printer.Color ( showDoc )
import Darcs.Repository.External ( backupByCopying, backupByRenaming )
import Darcs.Util.Path ( FileName, fn2fp )
import qualified Data.ByteString as B (empty, null, readFile)

import Storage.Hashed.Tree( Tree )

newtype DefaultIO a = DefaultIO { runDefaultIO :: IO a }
    deriving (Functor, Applicative, Monad)

instance MonadProgress DefaultIO where
  runProgressActions _ [] = return ()
  runProgressActions what items = DefaultIO $ do
    do beginTedious what
       tediousSize what (length items)
       mapM_ go items
       endTedious what
    where go item =
            do finishedOneIO what (showDoc Encode $ paMessage item)
               runDefaultIO (paAction item) `catch` \e ->
                 do hPutDocLn Encode stderr $ paOnError item
                    ioError e

instance ApplyMonad DefaultIO Tree where
    type ApplyMonadBase DefaultIO = IO
    mDoesDirectoryExist = DefaultIO . doesDirectoryExist . fn2fp
    mChangePref a b c = DefaultIO $ changePrefval a b c
    mModifyFilePS f j = DefaultIO $ B.readFile (fn2fp f) >>= runDefaultIO . j >>= writeAtomicFilePS (fn2fp f)
    mCreateDirectory = DefaultIO . createDirectory . fn2fp
    mCreateFile f = DefaultIO $
                    do exf <- doesFileExist (fn2fp f)
                       if exf then fail $ "File '"++fn2fp f++"' already exists!"
                              else do exd <- doesDirectoryExist $ fn2fp f
                                      if exd then fail $ "File '"++fn2fp f++"' already exists!"
                                             else writeAtomicFilePS (fn2fp f) B.empty
    mRemoveFile f = DefaultIO $
                    do let fp = fn2fp f
                       x <- B.readFile fp
                       unless (B.null x) $
                            fail $ "Cannot remove non-empty file "++fp
                       removeFile fp
    mRemoveDirectory = DefaultIO . removeDirectory . fn2fp
    mRename a b = DefaultIO $
                  catch
                  (renameDirectory x y `mplus` renameFile x y)
                  -- We need to catch does not exist errors, since older
                  -- versions of darcs allowed users to rename nonexistent
                  -- files.  :(
                  (\e -> unless (isDoesNotExistError e) $ ioError e)
      where x = fn2fp a
            y = fn2fp b

class (Functor m, Monad m) => TolerantMonad m where
    warning :: IO () -> m ()
    runIO :: m a -> IO a
    runTM :: IO a -> m a

newtype TolerantIO a = TIO { runTIO :: IO a }
    deriving (Functor, Applicative, Monad)

instance TolerantMonad TolerantIO where
    warning io = TIO $ io `catch` \e -> putStrLn $ "Warning: " ++ prettyException e
    runIO (TIO io) = io
    runTM = TIO

newtype SilentIO a = SIO { runSIO :: IO a }
    deriving (Functor, Applicative, Monad)

instance TolerantMonad SilentIO where
    warning io = SIO $ io `catch` \(_ :: SomeException) -> return ()
    runIO (SIO io) = io
    runTM = SIO

newtype TolerantWrapper m a = TolerantWrapper { runTolerantWrapper :: m a }
    deriving (Functor, Applicative, Monad, TolerantMonad)

-- | Apply patches, emitting warnings if there are any IO errors
runTolerantly :: TolerantWrapper TolerantIO a -> IO a
runTolerantly = runTIO . runTolerantWrapper

-- | Apply patches, ignoring all errors
runSilently :: TolerantWrapper SilentIO a -> IO a
runSilently = runSIO . runTolerantWrapper

-- | The default mode of applying patches: fail if the directory is not
-- as we expect
runDefault :: DefaultIO a -> IO a
runDefault = runDefaultIO

instance TolerantMonad m => ApplyMonad (TolerantWrapper m) Tree where
    type ApplyMonadBase (TolerantWrapper m) = IO
    mDoesDirectoryExist d = runTM $ runDefaultIO $ mDoesDirectoryExist d
    mReadFilePS f = runTM $ runDefaultIO $ mReadFilePS f
    mChangePref a b c = warning $ runDefaultIO $ mChangePref a b c
    mModifyFilePS f j = warning $ runDefaultIO $ mModifyFilePS f (DefaultIO . runIO . j)
    mCreateFile f = warning $ backup f >> runDefaultIO (mCreateFile f)
    mCreateDirectory d = warning $ backup d >> runDefaultIO (mCreateDirectory d)
    mRemoveFile f = warning $ runDefaultIO (mRemoveFile f)
    mRemoveDirectory d = warning $ catch
                                 (runDefaultIO (mRemoveDirectory d))
                                 (\(e :: IOException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because:\n" ++ show e)
    mRename a b = warning $ catch
                          (let do_backup = if map toLower x == map toLower y
                                           then backupByCopying (fn2fp b) -- avoid making the original vanish
                                           else backupByRenaming (fn2fp b)
                           in do_backup >> runDefaultIO (mRename a b))
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y

backup :: FileName -> IO ()
backup f = backupByRenaming (fn2fp f)
