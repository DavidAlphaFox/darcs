--  Copyright (C) 2002-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Repository.Test
    ( getTest
    , runPosthook
    , runPrehook
    , testTentative
    )
where

import System.Exit ( ExitCode(..) )
import System.Process ( system )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( when )

import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Prompt ( askUser )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Patch
    ( RepoPatch )

import Darcs.Repository.Internal
    ( setScriptsExecutable
    , withTentative
    )
import Darcs.Repository.Flags
    ( LeaveTestDir(..)
    , Verbosity(..)
    , SetScriptsExecutable(..)
    , RunTest (..)
    )
import Darcs.Repository.InternalTypes
    ( Repository(..) )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Repository.Lock
    ( withTempDir
    , withPermDir
    )
import Darcs.Patch.Apply ( ApplyState )
import Storage.Hashed.Tree ( Tree )


getTest :: Verbosity -> IO (IO ExitCode)
getTest verb =
 let putInfo s = when (verb /= Quiet) $ putStr s
 in do
 testline <- getPrefval "test"
 return $
   case testline of
   Nothing -> return ExitSuccess
   Just testcode -> do
     putInfo "Running test...\n"
     runTest testcode putInfo

runPosthook :: Maybe String -> Bool -> Verbosity -> AbsolutePath -> IO ExitCode
runPosthook mPostHook askPostHook verb repodir
    = do ph <- getPosthook mPostHook askPostHook
         withCurrentDirectory repodir $ runHook verb "Posthook" ph

getPosthook :: Maybe String -> Bool -> IO (Maybe String)
getPosthook mPostHookCmd askPostHook =
 case mPostHookCmd of
   Nothing -> return Nothing
   Just command ->
     if askPostHook
      then do putStr ("\nThe following command is set to execute.\n"++
                      "Execute the following command now (yes or no)?\n"++
                      command++"\n")
              yorn <- askUser ""
              case yorn of
                ('y':_) -> return $ Just command
                _ -> putStrLn "Posthook cancelled..." >> return Nothing
      else return $ Just command

runPrehook :: Maybe String -> Bool -> Verbosity -> AbsolutePath -> IO ExitCode
runPrehook mPreHookCmd askPreHook verb repodir =
    do ph <- getPrehook mPreHookCmd askPreHook
       withCurrentDirectory repodir $ runHook verb "Prehook" ph

getPrehook :: Maybe String -> Bool -> IO (Maybe String)
getPrehook mPreHookCmd askPreHook=
  case mPreHookCmd of
    Nothing -> return Nothing
    Just command ->
      if askPreHook
       then do putStr ("\nThe following command is set to execute.\n"++
                       "Execute the following command now (yes or no)?\n"++
                       command++"\n")
               yorn <- askUser ""
               case yorn of
                 ('y':_) -> return $ Just command
                 _ -> putStrLn "Prehook cancelled..." >> return Nothing
       else return $ Just command

runHook :: Verbosity -> String -> Maybe String -> IO ExitCode
runHook _ _ Nothing = return ExitSuccess
runHook verb cname (Just command) =
    do ec <- system command
       when (verb /= Quiet) $
         if ec == ExitSuccess
         then putStrLn $ cname++" ran successfully."
         else hPutStrLn stderr $ cname++" failed!"
       return ec

testTentative :: (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> RunTest
              -> LeaveTestDir
              -> SetScriptsExecutable
              -> Verbosity
              -> IO ExitCode
testTentative = testAny withTentative

runTest :: String -> (String -> IO ()) -> IO ExitCode
runTest testcode putInfo = do
    ec <- system testcode
    if ec == ExitSuccess
      then putInfo "Test ran successfully.\n"
      else putInfo "Test failed!\n"
    return ec


testAny :: RepoPatch p
        => (Repository p wR wU wT
            -> ((AbsolutePath -> IO ExitCode) -> IO ExitCode)
            -> (AbsolutePath -> IO ExitCode) -> IO ExitCode
           )
        -> Repository p wR wU wT
        -> RunTest
        -> LeaveTestDir
        -> SetScriptsExecutable
        -> Verbosity
        -> IO ExitCode
testAny withD repository@(Repo dir _ _ _) doRunTest ltd sse verb =
    debugMessage "Considering whether to test..." >>
    if doRunTest == NoRunTest then return ExitSuccess else withCurrentDirectory dir $
    do let putInfo = if verb == Quiet then const (return ()) else putStrLn
       debugMessage "About to run test if it exists."
       testline <- getPrefval "test"
       case testline of
         Nothing -> return ExitSuccess
         Just testcode ->
             withD repository (wd "testing") $ \_ ->
             do putInfo "Running test...\n"
                when (sse == YesSetScriptsExecutable) setScriptsExecutable
                runTest testcode putInfo
    where wd = if ltd == YesLeaveTestDir then withPermDir else withTempDir
