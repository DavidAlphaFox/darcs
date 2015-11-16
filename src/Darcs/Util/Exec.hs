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

{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- |
-- Module      : Darcs.Util.Exec
-- Copyright   : 2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Util.Exec
    (
      exec
    , execInteractive
    , readInteractiveProcess
    , renderExecException
    , withoutNonBlock
    , Redirects
    , Redirect(..)
    , ExecException(..)
    ) where

#ifndef WIN32
import Control.Exception ( bracket )
import System.Posix.Env ( setEnv, getEnv, unsetEnv )
import System.Posix.IO ( queryFdOption, setFdOption, FdOption(..), stdInput )
#else
import Control.Exception ( catchJust, IOException )
import Data.List ( isInfixOf )
#endif

import GHC.IO.Handle ( hDuplicate )

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( newEmptyMVar, takeMVar, putMVar )
import Control.Exception
    ( evaluate, bracketOnError, Exception(..), SomeException(..) )
import Data.Typeable ( Typeable, cast )
import System.Process ( system )
import qualified System.Process as P
import System.Exit ( ExitCode (..) )
import System.IO ( IOMode(..), openBinaryFile, stdin, stdout, hGetContents, hClose )
import System.Process   ( runProcess, terminateProcess, waitForProcess )

import Darcs.Util.Global ( whenDebugMode )
import Darcs.Util.Progress ( withoutProgress )

{-
   A redirection is a three-tuple of values (in, out, err).
   The most common values are:

     AsIs    don't change it
     Null    /dev/null on Unix, NUL on Windows
     File    open a file for reading or writing

   There is also the value Stdout, which is only meaningful for
   redirection of errors, and is performed AFTER stdout is
   redirected so that output and errors mix together. StdIn and
   StdErr could be added as well if they are useful.

   NOTE: Lots of care must be taken when redirecting stdin, stdout
   and stderr to one of EACH OTHER, since the ORDER in which they
   are changed have a significant effect on the result.
-}

type Redirects = (Redirect, Redirect, Redirect)

data Redirect = AsIs
              | Null
              | File FilePath
              | Stdout
                deriving Show

{-
  ExecException is thrown by exec if any system call fails,
  for example because the executable we're trying to run
  doesn't exist.
-}
--                   ExecException cmd    args     redirecs  errorDesc
data ExecException = ExecException
                        String     -- cmd
                        [String]   -- args
                        Redirects  -- redirects
                        String     -- errorDesc
                     deriving (Typeable)

instance Exception ExecException where
    toException = SomeException
    fromException (SomeException e) = cast e

renderExecException :: ExecException -> String
renderExecException (ExecException cmd args _ msg) =
  concat [
    "The program \"", unwords (cmd:args),
    "\" failed with error: \"",msg,"\"."]

instance Show ExecException where
  show = renderExecException

_devNull :: FilePath
#ifdef WIN32
_devNull = "NUL"
#else
_devNull = "/dev/null"
#endif

{-
  We use System.Process, which does the necessary quoting
  and redirection for us behind the scenes.
-}
exec  :: String -> [String] -> Redirects -> IO ExitCode
exec cmd args (inp,out,err) = withoutProgress $ do
    h_stdin  <- redirect inp ReadMode
    h_stdout <- redirect out WriteMode
    h_stderr <- redirect err WriteMode
    withExit127 $ bracketOnError
      (do doOptionalDebug
          runProcess cmd args Nothing Nothing h_stdin h_stdout h_stderr)
      terminateProcess
      waitForProcess
  where
    doOptionalDebug = whenDebugMode . putStrLn . unwords $
        cmd : args ++ ["; #"] ++ map show [inp, out, err]
    redirect AsIs               _    = return Nothing
    redirect Null               mode = Just `fmap` openBinaryFile _devNull mode
    redirect (File "/dev/null") mode = redirect Null mode
    redirect (File f)           mode = Just `fmap` openBinaryFile f mode
    -- hDuplicate stdout rather than passing stdout itself,
    -- because runProcess closes the Handles we pass it.
    redirect Stdout             _    = Just `fmap` hDuplicate stdout

execInteractive :: String -> String -> IO ExitCode
#ifndef WIN32
{-
This should handle arbitrary commands interpreted by the shell on Unix since
that's what people expect. But we don't want to allow the shell to interpret
the argument in any way, so we set an environment variable and call
cmd "$DARCS_ARGUMENT"
-}
execInteractive cmd arg = withoutProgress $ do
    let var = "DARCS_ARGUMENT"
    stdin `seq` return ()
    withoutNonBlock $ bracket
      (do oldval <- getEnv var
          setEnv var arg True
          return oldval)
      (\oldval ->
         case oldval of
              Nothing -> unsetEnv var
              Just val -> setEnv var val True)
      (\_ -> withExit127 $ system $ cmd++" \"$"++var++"\"")

#else
-- The `system' function passes commands to execute via cmd.exe (or
-- command.com) it's return value is equivalent to the one returned by the
-- shell. For regular applications - this works correctly resulting in the
-- exit code of the program. However in case of a command/file which can't be
-- found - cmd.exe will return 1 instead of propagating the ExitFailure 9009
-- which on windows is equivalent to ExitFailure 127 from *nix machines.
--
-- Here we force return the exit code of the last cmd.exe action by appending
-- & exit !errorlevel! to the command being executed that way chaining with
-- ortryrunning works correctly.
--
-- SETLOCAL EnableDelayedExpansion makes sure that !variable! expansion is done
-- correctly on systems where that function is not enabled by default.
--
execInteractive cmd arg = withoutProgress $
  withExit127 $ system $ "SETLOCAL EnableDelayedExpansion & " ++
                          cmd ++ " " ++ arg ++
                          " & exit !errorlevel!"
#endif

withoutNonBlock :: IO a -> IO a
#ifndef WIN32
{-
Do IO without NonBlockingRead on stdInput.

This is needed when running unsuspecting external commands with interactive
mode - if read from terminal is non-blocking also write to terminal is
non-blocking.
-}
withoutNonBlock x =
    do nb <- queryFdOption stdInput NonBlockingRead
       if nb
          then bracket
                   (setFdOption stdInput NonBlockingRead False)
                   (\_ -> setFdOption stdInput NonBlockingRead True)
                   (\_ -> x)
          else x
#else
withoutNonBlock x = x
#endif

readInteractiveProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> IO (ExitCode,String)      -- ^ exitcode, stderr
readInteractiveProcess cmd args = do
    inh' <- hDuplicate stdin
    outh <- hDuplicate stdout
    (_, _, Just errh, pid) <-
        P.createProcess (P.proc cmd args){
          P.std_in  = P.UseHandle inh',
          P.std_out = P.UseHandle outh,
          P.std_err = P.CreatePipe }
    errMVar <- newEmptyMVar

    errors <- hGetContents errh
    _ <- forkIO $ do
      _ <- evaluate (length errors)
      putMVar errMVar errors

    err <- takeMVar errMVar
    hClose errh

    ex <- waitForProcess pid

    return (ex, err)
  
{-
Ensure that we exit 127 if the thing we are trying to run does not exist
(Only needed under Windows)
-}
withExit127 :: IO ExitCode -> IO ExitCode
#ifdef WIN32
withExit127 a = catchJust notFoundError a (const $ return $ ExitFailure 127)

notFoundError :: IOException -> Maybe ()
notFoundError e | "runProcess: does not exist" `isInfixOf` show e = Just ()
notFoundError _ = Nothing
#else
withExit127 = id
#endif
