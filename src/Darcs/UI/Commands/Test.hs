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

module Darcs.UI.Commands.Test
    (
      test
    ) where

import Prelude hiding ( (^), init, catch )

import Control.Exception ( catch, IOException )
import Control.Monad( when )

import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( hFlush, stdout )

import Storage.Hashed.Tree( Tree )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , nodefaults
    , putInfo
    , amInHashedRepository )
import Darcs.UI.Flags ( DarcsFlag( SetScriptsExecutable
                                  , Linear
                                  , Backoff
                                  , Bisect
                                  , LeaveTestDir
                                  )
                      , useCache, verbosity )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Repository (
                          readRepo
                        , withRepository
                        , RepoJob(..)
                        , withRecorded
                        , setScriptsExecutablePatches
                        , setScriptsExecutable
                        )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..)
    , (:<)(..)
    , (+<+)
    , reverseRL
    , splitAtRL
    , lengthRL
    , mapRL
    , mapFL
    , mapRL_RL
    )
import Darcs.Patch.Conflict ( Conflict )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Patchy ( Patchy
                          , Invert
                          , Apply
                          , ShowPatch
                          )
import Darcs.Patch ( RepoPatch
                   , Named
                   , description
                   , apply
                   , invert
                   )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Util.Printer ( putDocLn
               , text
               )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Repository.ApplyPatches ( DefaultIO, runDefault )
import Darcs.Repository.Test ( getTest )
import Darcs.Repository.Lock
    ( withTempDir
    , withPermDir
    )


testDescription :: String
testDescription = "Run regression test."

testHelp :: String
testHelp =
 unlines
 [ "Run test on the current recorded state of the repository.  Given no"
  ,"arguments, it uses the default repository test (see `darcs setpref`)."
  ,"Given one argument, it treats it as a test command."
  ,"Given two arguments, the first is an initialization command and the"
  ,"second is the test (meaning the exit code of the first command is not"
  ,"taken into account to determine success of the test)."
  ,"If given the `--linear` or `--bisect` flags, it tries to find the most"
  ,"recent version in the repository which passes a test."
  ,""
  ,"`--linear` does linear search starting from head, and moving away"
  ,"from head. This strategy is best when the test runs very quickly"
  ,"or the patch you're seeking is near the head."
  ,""
  ,"`--bisect` does binary search.  This strategy is best when the test"
  ,"runs very slowly or the patch you're seeking is likely to be in"
  ,"the repository's distant past."
  ,""
  ,"`--backoff` starts searching from head, skipping further and further"
  ,"into the past until the test succeeds.  It then does a binary search"
  ,"on a subset of those skipped patches.  This strategy works well unless"
  ,"the patch you're seeking is in the repository's distant past."
  ,""
  ,"Under the assumption that failure is monotonous, `--linear` and"
  ,"`--bisect` produce the same result.  (Monotonous means that when moving"
  ,"away from head, the test result changes only once from \"fail\" to"
  ,"\"ok\".)  If failure is not monotonous, any one of the patches that"
  ,"break the test is found at random."
 ]

testBasicOpts :: DarcsOption a
                 (O.TestStrategy -> O.LeaveTestDir -> Maybe String -> a)
testBasicOpts = O.testStrategy ^ O.leaveTestDir ^ O.workingRepoDir

testAdvancedOpts :: DarcsOption a (O.SetScriptsExecutable -> a)
testAdvancedOpts = O.setScriptsExecutable

testOpts :: DarcsOption a
            (O.TestStrategy
             -> O.LeaveTestDir
             -> Maybe String
             -> Maybe O.StdCmdAction
             -> Bool
             -> Bool
             -> O.Verbosity
             -> Bool
             -> O.SetScriptsExecutable
             -> O.UseCache
             -> Maybe String
             -> Bool
             -> Maybe String
             -> Bool
             -> a)
testOpts = testBasicOpts `withStdOpts` testAdvancedOpts

test :: DarcsCommand [DarcsFlag]
test = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "test"
    , commandHelp = testHelp
    , commandDescription = testDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[[INITIALIZATION]", "COMMAND]"]
    , commandCommand = testCommand
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc testAdvancedOpts
    , commandBasicOptions = odesc testBasicOpts
    , commandDefaults = defaultFlags testOpts
    , commandCheckOptions = ocheck testOpts
    , commandParseOptions = onormalise testOpts
    }

-- | Functions defining a strategy for executing a test
type Strategy = forall p wX wY
               . (RepoPatch p, ApplyMonad DefaultIO (ApplyState p), ApplyState p ~ Tree)
              => [DarcsFlag]
              -> IO ExitCode  -- ^ test command
              -> ExitCode
              -> RL (Named p) wX wY
              -> IO ()

testCommand :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
testCommand _ opts args =
 withRepository (useCache opts) $ RepoJob $ \repository -> do
  patches <- readRepo repository
  (init,testCmd) <- case args of
    [] ->
      do t <- getTest (verbosity opts)
         return (return ExitSuccess, t)
    [cmd] ->
      do putStrLn $ "Using test command:\n"++cmd
         return (return ExitSuccess, system cmd)
    [init,cmd] ->
      do putStrLn $ "Using initialization command:\n"++init
         putStrLn $ "Using test command:\n"++cmd
         return (system init, system cmd)
    _ -> fail "Test expects zero to two arguments."
  let wd = if LeaveTestDir `elem` opts then withPermDir else withTempDir
  withRecorded repository (wd "testing") $ \_ -> do
    when (SetScriptsExecutable `elem` opts) setScriptsExecutable
    _ <- init
    putInfo opts $ text "Running test...\n"
    testResult <- testCmd
    let track = chooseStrategy opts
    track opts testCmd testResult (mapRL_RL hopefully . newset2RL $ patches)

chooseStrategy :: [DarcsFlag] -> Strategy
chooseStrategy opts
    | Bisect `elem` opts = trackBisect
    | Linear `elem` opts = trackLinear
    | Backoff `elem` opts = trackBackoff
    | otherwise = oneTest

-- | test only the last recorded state
oneTest :: Strategy
oneTest opts _ ExitSuccess _ = putInfo opts $ text "Test ran successfully.\n"
oneTest opts _ testResult  _ = do
    putInfo opts $ text "Test failed!\n"
    exitWith testResult

-- | linear search (with --linear)
trackLinear :: Strategy
trackLinear _ _ ExitSuccess _ = putStrLn "Success!"
trackLinear opts testCmd (ExitFailure _) (p:<:ps) = do
    let ip = invert p
    safeApply ip
    when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches ip
    putStrLn "Trying without the patch:"
    putDocLn $ description ip
    hFlush stdout
    testResult <- testCmd
    trackLinear opts testCmd testResult ps
trackLinear _ _ (ExitFailure _) NilRL = putStrLn "Noone passed the test!"

-- | exponential backoff search (with --backoff)
trackBackoff :: Strategy
trackBackoff _ _ ExitSuccess NilRL = putStrLn "Success!"
trackBackoff _ _ (ExitFailure _) NilRL = putStrLn "Noone passed the test!"
trackBackoff _ _ ExitSuccess _ = putStrLn "Test does not fail on head."
trackBackoff opts testCmd (ExitFailure _) ps =
    trackNextBackoff opts testCmd 4 ps

trackNextBackoff :: (RepoPatch p, ApplyMonad DefaultIO (ApplyState p), ApplyState p ~ Tree)
                 => [DarcsFlag]
                 -> IO ExitCode
                 -> Int -- ^ number of patches to skip
                 -> RL (Named p) wY wZ -- ^ patches not yet skipped
                 -> IO ()
trackNextBackoff _ _ _ NilRL = putStrLn "Noone passed the test!"
trackNextBackoff opts testCmd n ahead
    | n >= lengthRL ahead = initialBisect opts testCmd ahead
trackNextBackoff opts testCmd n ahead = do
    putStrLn $ "Skipping " ++ show n ++ " patches..."
    hFlush stdout
    case splitAtRL n ahead of
        ( skipped' :< ahead' ) -> do
            unapplyRL skipped'
            when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches skipped'
            testResult <- testCmd
            case testResult of
                ExitFailure _ ->
                    trackNextBackoff opts testCmd (2*n) ahead'
                ExitSuccess -> do
                    applyRL skipped'  -- offending patch is one of these
                    initialBisect opts testCmd skipped' -- bisect to find it

-- | binary search (with --bisect)
trackBisect :: Strategy
trackBisect _ _ ExitSuccess NilRL = putStrLn "Success!"
trackBisect _ _ (ExitFailure _) NilRL = putStrLn "Noone passed the test!"
trackBisect _ _ ExitSuccess _ = putStrLn "Test does not fail on head."
trackBisect opts testCmd (ExitFailure _) ps =
    initialBisect opts testCmd ps

initialBisect ::  (RepoPatch p, ApplyMonad DefaultIO (ApplyState p), ApplyState p ~ Tree)
              => [DarcsFlag]
              -> IO ExitCode
              -> RL (Named p) wX wY
              -> IO ()
initialBisect opts testCmd ps =
    trackNextBisect opts currProg testCmd BisectRight (patchTreeFromRL ps)
  where
    maxProg  = 1 + round ((logBase 2 $ fromIntegral $ lengthRL ps) :: Double)
    currProg = (1, maxProg) :: BisectState

-- | Bisect Patch Tree
data PatchTree p wX wY where
    Leaf :: p wX wY -> PatchTree p wX wY
    Fork :: PatchTree p wY wZ -> PatchTree p wX wY -> PatchTree p wX wZ

-- | Direction of Bisect trackdown
data BisectDir = BisectLeft | BisectRight deriving Show

-- | Progress of Bisect
type BisectState = (Int, Int)

-- | Create Bisect PatchTree from the RL
patchTreeFromRL :: (Patchy p) => RL p wX wY -> PatchTree p wX wY
patchTreeFromRL (l :<: NilRL) = Leaf l
patchTreeFromRL xs = case splitAtRL (lengthRL xs `div` 2) xs of
                       (l :< r) -> Fork (patchTreeFromRL l) (patchTreeFromRL r)

-- | Convert PatchTree back to RL
patchTree2RL :: (Patchy p) => PatchTree p wX wY -> RL p wX wY
patchTree2RL (Leaf p)   = p :<: NilRL
patchTree2RL (Fork l r) = patchTree2RL l +<+ patchTree2RL r

-- | Iterate the Patch Tree
trackNextBisect :: (RepoPatch p, ApplyMonad DefaultIO (ApplyState p), ApplyState p ~ Tree)
                => [DarcsFlag]
                -> BisectState
                -> IO ExitCode -- ^ test command
                -> BisectDir
                -> PatchTree (Named p) wX wY
                -> IO ()
trackNextBisect opts (dnow, dtotal) testCmd dir (Fork l r) = do
  putStr $ "Trying " ++ show dnow ++ "/" ++ show dtotal ++ " sequences...\n"
  hFlush stdout
  case dir of
    BisectRight -> jumpHalfOnRight opts l  -- move in temporary repo
    BisectLeft  -> jumpHalfOnLeft  opts r  -- within given direction
  testResult <- testCmd -- execute test on repo
  case testResult of
    ExitSuccess -> trackNextBisect opts (dnow+1, dtotal) testCmd
                                   BisectLeft l  -- continue left  (to the present)
    _           -> trackNextBisect opts (dnow+1, dtotal) testCmd
                                   BisectRight r -- continue right (to the past)
trackNextBisect _ _ _ _ (Leaf p) = do
  putStrLn "Last recent patch that fails the test (assuming monotony in the given range):"
  putDocLn (description p)

jumpHalfOnRight :: (IsHunk p, Conflict p,
                    PatchListFormat p, ShowPatch p, PatchInspect p,
                    Patchy p, ApplyMonad DefaultIO (ApplyState p))
                => [DarcsFlag] -> PatchTree p wX wY -> IO ()
jumpHalfOnRight opts l = do unapplyRL ps
                            when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches ps
  where ps = patchTree2RL l

jumpHalfOnLeft :: (IsHunk p, Conflict p,
                   PatchListFormat p, ShowPatch p, PatchInspect p,
                   Patchy p, ApplyMonad DefaultIO (ApplyState p))
               => [DarcsFlag] -> PatchTree p wX wY -> IO ()
jumpHalfOnLeft opts r = do applyRL p
                           when (SetScriptsExecutable `elem` opts) $ setScriptsExecutablePatches p

  where p = patchTree2RL r

applyRL :: (Invert p, ShowPatch p, Apply p, ApplyMonad DefaultIO (ApplyState p))
        => RL p wX wY -> IO ()
applyRL   patches = sequence_ (mapFL safeApply (reverseRL patches))

unapplyRL :: (Invert p, ShowPatch p, Apply p, ApplyMonad DefaultIO (ApplyState p))
           => RL p wX wY -> IO ()
unapplyRL patches = sequence_ (mapRL (safeApply . invert) patches)

safeApply :: (Invert p, ShowPatch p, Apply p, ApplyMonad DefaultIO (ApplyState p))
          => p wX wY -> IO ()
safeApply p = runDefault (apply p) `catch` \(msg :: IOException) -> fail $ "Bad patch:\n" ++ show msg
