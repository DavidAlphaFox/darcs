{-# LANGUAGE CPP #-}

-- |
-- Module      : Darcs.Util.Workaround
-- Copyright   : 2008 David Roundy <droundy@darcs.net>
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Util.Workaround
    (
      renameFile
    , setExecutable
    , getCurrentDirectory
    , installHandler
    , raiseSignal
    , Handler(..)
    , Signal
    , sigINT
    , sigHUP
    , sigABRT
    , sigALRM
    , sigTERM
    , sigPIPE
    ) where

import Prelude hiding ( catch )

#ifdef WIN32

import Control.Monad ( unless )
import qualified System.Directory ( renameFile, getCurrentDirectory, removeFile )
import Control.Exception ( catch, IOException )
import qualified Control.Exception ( mask )
import qualified System.IO.Error ( isDoesNotExistError, ioError )

#else

import System.Posix.Signals(installHandler, raiseSignal, Handler(..), Signal,
                            sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE)
import System.Directory ( renameFile, getCurrentDirectory )
import System.Posix.Files (fileMode,getFileStatus, setFileMode,
                           setFileCreationMask,
                           ownerReadMode, ownerWriteMode, ownerExecuteMode,
                           groupReadMode, groupWriteMode, groupExecuteMode,
                           otherReadMode, otherWriteMode, otherExecuteMode)
import Data.Bits ( (.&.), (.|.), complement )

#endif

#ifdef WIN32
-- Dummy implementation of POSIX signals
data Handler = Default
             | Ignore
             | Catch (IO ())

type Signal = Int

installHandler :: Signal
               -> Handler
               -> Maybe ()
               -> IO ()
installHandler _ _ _ = return ()


raiseSignal :: Signal -> IO ()
raiseSignal _ = return ()


sigINT :: Signal
sigINT = 0

-- not used: sigKILL = 0

sigHUP :: Signal
sigHUP = 0

-- not used: sigQUIT = 0

sigABRT :: Signal
sigABRT = 0

sigTERM :: Signal
sigTERM = 0

sigPIPE :: Signal
sigPIPE = 0

sigALRM :: Signal
sigALRM = 0


-- | System.Directory.renameFile incorrectly fails when the new file already
-- exists.  This code works around that bug at the cost of losing atomic
-- writes.
renameFile :: FilePath
           -> FilePath
           -> IO ()
renameFile old new = Control.Exception.mask $ \_ ->
   System.Directory.renameFile old new
   `catch` \(_ :: IOException) ->
   do System.Directory.removeFile new
        `catch`
         (\e -> unless (System.IO.Error.isDoesNotExistError e) $
                    System.IO.Error.ioError e)
      System.Directory.renameFile old new


setExecutable :: FilePath
              -> Bool
              -> IO ()
setExecutable _ _ = return ()


-- | System.Directory.getCurrentDirectory returns a path with backslashes in it
-- under windows, and some of the code gets confused by that, so we override
-- getCurrentDirectory and translates '\\' to '/'
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
    d <- System.Directory.getCurrentDirectory
    return $ map rb d
  where
    rb '\\' = '/'
    rb c = c

#else


setExecutable :: FilePath
              -> Bool
              -> IO ()
setExecutable f ex = do
    st <- getFileStatus f
    umask <- setFileCreationMask 0
    _ <- setFileCreationMask umask
    let rw = fileMode st .&.
             (ownerReadMode .|. ownerWriteMode .|.
              groupReadMode .|. groupWriteMode .|.
              otherReadMode .|. otherWriteMode)
        total = if ex then rw .|.
                          ((ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)
                           .&. complement umask)
                      else rw
    setFileMode f total

#endif
