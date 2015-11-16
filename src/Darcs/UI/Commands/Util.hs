--  Copyright (C) 2002-2004 David Roundy
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

{-# LANGUAGE CPP #-}
module Darcs.UI.Commands.Util
    ( announceFiles
    , filterExistingFiles
    , testTentativeAndMaybeExit
    , getUniqueRepositoryName
    , getUniqueDPatchName
    ) where

import Control.Monad ( unless )

import System.Exit ( ExitCode(..), exitWith )

import Storage.Hashed.Monad ( virtualTreeIO, exists )
import Storage.Hashed.Tree ( Tree )
import Storage.Hashed( floatPath, readPlainTree )

import Darcs.Util.Path
    ( SubPath, toFilePath, getUniquePathName )
import Darcs.Patch ( RepoPatch )
import Darcs.Repository ( Repository, readRecorded, readUnrecorded,
                          testTentative )
import Darcs.Repository.State ( applyTreeFilter, restrictBoring )
import Darcs.Repository.Flags ( LookForAdds (..) )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Bundle ( patchFilename )
import Darcs.UI.Options.All
    ( Verbosity, SetScriptsExecutable, TestChanges (..)
    , RunTest(..), LeaveTestDir(..) )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Prompt ( PromptConfig(..), promptChar )

announceFiles :: Maybe [SubPath] -> String -> IO ()
announceFiles Nothing _ = return ()
announceFiles (Just files) message = putStrLn $ message ++ " " ++
    unwords (map show files) ++ ":\n"

testTentativeAndMaybeExit :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> Verbosity
                          -> TestChanges
                          -> SetScriptsExecutable
                          -> Bool
                          -> String
                          -> String -> Maybe String -> IO ()
testTentativeAndMaybeExit repo verb test sse interactive failMessage confirmMsg withClarification = do
    let (rt,ltd) = case test of
          NoTestChanges    -> (NoRunTest, YesLeaveTestDir)
          YesTestChanges x -> (YesRunTest, x)
    testResult <- testTentative repo rt ltd sse verb
    unless (testResult == ExitSuccess) $ do
        let doExit = maybe id (flip clarifyErrors) withClarification $
                        exitWith testResult
        unless interactive doExit
        putStrLn $ "Looks like " ++ failMessage
        let prompt = "Shall I " ++ confirmMsg ++ " anyway?"
        yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
        unless (yn == 'y') doExit

filterExistingFiles :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository p wR wU wT
                    -> LookForAdds
                    -> [SubPath]
                    -> IO [SubPath]
filterExistingFiles repo lfa files = do
      pristine <- readRecorded repo
      -- TODO this is slightly inefficient, since we should really somehow
      -- extract the unrecorded state as a side-effect of unrecordedChanges
      index <- readUnrecorded repo $ Just files
      nonboring <- restrictBoring index
      working <- applyTreeFilter nonboring `fmap` readPlainTree "."
      let paths = map toFilePath files
          check = virtualTreeIO $ mapM (exists . floatPath) paths
      (in_working, _) <- check working
      (in_pristine, _) <- check pristine
      mapM_ maybe_warn $ zip3 paths in_working in_pristine
      return [ path | (path, True) <- zip files (zipWith (||) in_working in_pristine) ]
    where maybe_warn (file, False, False) =
              putStrLn $ "WARNING: File '"++file++"' does not exist!"
          maybe_warn (file, True, False) | lfa == YesLookForAdds =
              putStrLn $ "WARNING: File '" ++ file ++ "' not in repository!"
          maybe_warn _ = return ()

getUniqueRepositoryName :: Bool -> FilePath -> IO FilePath
getUniqueRepositoryName talkative name = getUniquePathName talkative buildMsg buildName
  where
    buildName i = if i == -1 then name else name++"_"++show i
    buildMsg n = "Directory or file '"++ name ++
                 "' already exists, creating repository as '"++
                 n ++"'"

getUniqueDPatchName :: FilePath -> IO FilePath
getUniqueDPatchName name = getUniquePathName True buildMsg buildName
  where
    buildName i = if i == -1 then patchFilename name else patchFilename $ name++"_"++show i
    buildMsg n = "Directory or file '"++ name ++
                 "' already exists, creating dpatch as '"++
                 n ++"'"
