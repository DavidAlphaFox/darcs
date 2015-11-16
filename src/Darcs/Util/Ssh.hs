--
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

module Darcs.Util.Ssh
    (
      SshSettings(..)
    , defaultSsh
    , windows
    ) where


import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( catch, catchJust, SomeException )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isPrefixOf )
import System.Info ( os )
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error ( ioeGetErrorType, isDoesNotExistErrorType )
import System.Process ( readProcessWithExitCode )
import System.Environment ( getEnv )
import Prelude hiding (catch)

import Darcs.Util.Global ( whenDebugMode )

windows :: Bool
windows = "mingw" `isPrefixOf` os

data SshSettings = SshSettings
    { ssh :: String
    , scp :: String
    , sftp :: String
    } deriving (Show, Eq)


_defaultSsh :: IORef SshSettings
_defaultSsh = unsafePerformIO $ newIORef =<< detectSsh


-- | Expected properties:
--
-- * only ever runs once in the lifetime of the program
-- * environment variables override all
-- * tries Putty first on Windows
-- * falls back to plain old ssh
detectSsh :: IO SshSettings
detectSsh = do
    whenDebugMode (putStrLn "Detecting SSH settings")
    vanilla <-  if windows
                  then do
                    plinkStr <- (snd3 <$> readProcessWithExitCode "plink" [] "")
                                  `catch` \(e :: SomeException) -> return (show e)
                    whenDebugMode $ putStrLn $
                        "SSH settings (plink): " ++
                        (concat . take 1 . lines $ plinkStr)
                    if "PuTTY" `isPrefixOf` plinkStr
                      then return (SshSettings "plink" "pscp -q" "psftp")
                      else return rawVanilla
                  else return rawVanilla
    settings <- SshSettings <$> fromEnv (ssh vanilla)  "DARCS_SSH"
                            <*> fromEnv (scp vanilla)  "DARCS_SCP"
                            <*> fromEnv (sftp vanilla) "DARCS_SFTP"
    whenDebugMode (putStrLn $ "SSH settings: " ++ show settings)
    return settings
  where
    snd3 (_, x, _) = x
    rawVanilla = SshSettings "ssh" "scp -q" "sftp"
    fromEnv :: String -> String -> IO String
    fromEnv d v = catchJust notFound
                            (getEnv v)
                            (const (return d))
    notFound e =  if isDoesNotExistErrorType (ioeGetErrorType e)
                  then Just ()
                  else Nothing


defaultSsh :: SshSettings
defaultSsh = unsafePerformIO $ readIORef _defaultSsh
