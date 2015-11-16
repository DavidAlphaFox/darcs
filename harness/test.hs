{-# LANGUAGE CPP, MultiParamTypeClasses, DeriveDataTypeable, ViewPatterns, OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main ( main ) where

#ifdef DISABLE_TESTING
main :: IO ()
main = fail $   "test infrastructure not built,"
             ++ " please pass --enable-tests to configure, then rebuild"
#else

import qualified Darcs.Test.Misc
import qualified Darcs.Test.Patch
import qualified Darcs.Test.Email

import Control.Monad ( filterM )
import Control.Exception ( SomeException )
import qualified Control.Monad.Trans
import Data.Text ( Text, pack, unpack )
import Data.Char ( toLower )
import Data.List ( isPrefixOf, isSuffixOf, sort )
import Data.List.Split ( splitOn )
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromMaybe )
import GHC.IO.Encoding ( textEncodingName )
import System.Console.CmdArgs hiding ( args )
import System.Directory ( doesFileExist )
import System.Environment.FindBin ( getProgPath )
import System.FilePath( takeDirectory, takeBaseName, isAbsolute )
import System.IO( hSetBinaryMode, hSetBuffering, BufferMode( NoBuffering ), stdin, stdout, stderr, localeEncoding )
import Test.Framework.Providers.API
  ( TestResultlike(..), Testlike(..), Test, runImprovingIO, yieldImprovement, Test(..), liftIO )
import Test.Framework ( defaultMainWithArgs )
import Shelly hiding ( liftIO, run, FilePath, path )
import qualified Shelly

doUnit :: IO [Test]
doUnit = return unitTests

-- | This is the big list of tests that will be run using testrunner.
unitTests :: [Test]
unitTests =
  [ Darcs.Test.Email.testSuite
  , Darcs.Test.Misc.testSuite
  ] ++ Darcs.Test.Patch.testSuite

-- ----------------------------------------------------------------------
-- shell tests
-- ----------------------------------------------------------------------

data Format = Darcs1 | Darcs2 deriving (Show, Eq, Typeable, Data)
data DiffAlgorithm = MyersDiff | PatienceDiff deriving (Show, Eq, Typeable, Data)
data Running = Running deriving Show
data Result = Success | Skipped | Failed String

instance Show Result where
  show Success = "Success"
  show Skipped = "Skipped"
  show (Failed f) = unlines (map ("| " ++) $ lines f)

instance TestResultlike Running Result where
  testSucceeded Success = True
  testSucceeded Skipped = True
  testSucceeded _ = False

data ShellTest = ShellTest { format :: Format
                           , testfile :: FilePath
                           , testdir  :: Maybe FilePath -- ^ only if you want to set it explicitly
                           , _darcspath :: FilePath
                           , _diffalgorithm :: DiffAlgorithm
                           }
                 deriving Typeable

runtest' :: ShellTest -> Text -> Sh Result
runtest' (ShellTest fmt _ _ dp da) srcdir =
  do wd <- toTextIgnore <$> pwd
     setenv "HOME" wd
     setenv "TESTDATA" (toTextIgnore (srcdir </> "tests" </> "data"))
     setenv "TESTBIN" (toTextIgnore (srcdir </> "tests" </> "bin"))
     setenv "DARCS_TESTING_PREFS_DIR" $ toTextIgnore $ wd </> ".darcs"
     setenv "EMAIL" "tester"
     setenv "DARCS_DONT_COLOR" "1"
     setenv "DARCS_DONT_ESCAPE_ANYTHING" "1"
     p <- get_env_text "PATH"
     setenv "PATH" (pack (takeDirectory dp ++ pathVarSeparator ++ unpack p))
     setenv "DARCS" $ pack dp
     setenv "GHC_VERSION" $ pack $ show (__GLASGOW_HASKELL__ :: Int)
     mkdir ".darcs"
     writefile ".darcs/defaults" defaults
     _ <- onCommandHandles (initOutputHandles (\h -> hSetBinaryMode h True)) $
          Shelly.run "bash" [ "test" ]
     return Success
   `catch_sh` \(_::SomeException)
                 -> do code <- lastExitCode
                       case code of
                        200 -> return Skipped
                        _   -> Failed <$> unpack <$> lastStderr
  where defaults = pack $ unlines (["ALL " ++ fmtstr, "send no-edit-description", "ALL ignore-times"] ++ dcs)
        fmtstr = case fmt of
                  Darcs2 -> "darcs-2"
                  Darcs1 -> "darcs-1"
        dcs = [dc ++ " " ++ daf | dc <- ["revert","unrevert", "whatsnew",
                                         "record", "unpull", "obliterate",
                                         "amend-record", "mark-conflicts",
                                         "rebase", "pull", "repair",
                                         "rollback", "apply",
                                         "rebase pull", "rebase suspend",
                                         "rebase unsuspend", "rebase obliterate"] ]
        daf = case da of
                PatienceDiff -> "patience"
                MyersDiff -> "myers"

#ifdef WIN32
        pathVarSeparator = ";"
#else
        pathVarSeparator = ":"
#endif

-- TODO: add a 'all' option (implement using an Enum instance)?
readOptionList :: (String -> a) -> (String -> [a])
readOptionList readElem str = map readElem (splitOn "," str)

readDiffAlgorithm :: String -> DiffAlgorithm
readDiffAlgorithm (map toLower -> "myers") = MyersDiff
readDiffAlgorithm (map toLower -> "patience") = PatienceDiff
readDiffAlgorithm _ = error "Valid diff algorithms: myers, patience"

readRepoFormat :: String -> Format
readRepoFormat (map toLower -> "darcs-1") = Darcs1
readRepoFormat (map toLower -> "darcs-2") = Darcs2
readRepoFormat _ = error "Valid repo formats: darcs-1, darcs-2"

runtest :: ShellTest -> Sh Result
runtest t =
 withTmp $ \dir -> do
  cp "tests/lib" dir
  cp ("tests" </> testfile t) (dir </> "test")
  srcdir <- pwd
  silently $ sub $ cd dir >> runtest' t (toTextIgnore srcdir)
 where
  withTmp =
   case testdir t of
     Just dir -> \job -> do
       let d = (dir </> show (format t) </> takeBaseName (testfile t))
       mkdir_p d
       job d
     Nothing  -> withTmpDir

instance Testlike Running Result ShellTest where
  testTypeName _ = "Shell"
  runTest _ test = runImprovingIO $ do yieldImprovement Running
                                       liftIO (shelly $ runtest test)

shellTest :: FilePath -> Format -> Maybe FilePath -> String -> DiffAlgorithm -> Test
shellTest dp fmt tdir file da = Test (file ++ " (" ++ show fmt ++ ")" ++ " (" ++ show da ++ ")") $ ShellTest fmt file tdir dp da

hasPrefix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
hasPrefix prefix =
    let len = B.length prefix in
    \str -> if B.take len str == prefix then Just (B.drop len str) else Nothing

toString :: Shelly.FilePath -> String
toString = unpack . toTextIgnore

-- use of a pragma in a test script overrides the user's selection for that particular test,
-- based on the assumption that the test author knows best
parsePragmas :: FilePath -> FilePath -> IO (FilePath, (Maybe [DiffAlgorithm], Maybe [Format]))
parsePragmas path file = do
  contents <- B.lines <$> B.readFile (toString $ path </> file)
  let parseLine
          (_diffAlgorithms, repoFormats)
          (hasPrefix (B.pack "#pragma diff-algorithm ") -> Just (readOptionList readDiffAlgorithm . B.unpack -> newDiffAlgorithms))
         = (Just newDiffAlgorithms, repoFormats)
      parseLine
          (diffAlgorithms, _repoFormats)
          (hasPrefix (B.pack "#pragma repo-format ") -> Just (readOptionList readRepoFormat . B.unpack -> newRepoFormats))
         = (diffAlgorithms, Just newRepoFormats)
      parseLine _ (hasPrefix (B.pack "#pragma ") -> Just pragma) = error $ "Unknown pragma " ++ B.unpack pragma ++ " in " ++ (toString $ path </> file)
      parseLine x _ = x
      pragmas = foldl parseLine (Nothing, Nothing) contents
  return (file, pragmas)

findShell :: FilePath -> Maybe FilePath -> Bool -> [DiffAlgorithm] -> [Format] -> Sh [Test]
findShell dp tdir isFailing diffAlgorithmsDefault repoFormatsDefault =
  do allFiles <- map (drop (length ("tests/"::String)) . toString)  <$> ls (fromText "tests")
     let files = sort $ filter relevant $ filter (".sh" `isSuffixOf`) allFiles
     annotatedFiles <- Control.Monad.Trans.liftIO $ mapM (parsePragmas "tests") files
     return [ shellTest dp fmt tdir file da
            | (file, (diffAlgorithmsPragma, repoFormatsPragma)) <- annotatedFiles
            , fmt <- fromMaybe repoFormatsDefault repoFormatsPragma
            , da <- fromMaybe diffAlgorithmsDefault diffAlgorithmsPragma ]
  where relevant = (if isFailing then id else not) . ("failing-" `isPrefixOf`)

findNetwork :: FilePath -> Maybe FilePath -> [DiffAlgorithm] -> [Format] -> Sh [Test]
findNetwork dp tdir diffAlgorithmsDefault repoFormatsDefault =
  do files <- sort <$> filter (".sh" `isSuffixOf`) <$> map (drop (length ("tests/network/"::String)) . toString) <$> ls "tests/network"
     annotatedFiles <- Control.Monad.Trans.liftIO $ mapM (parsePragmas "tests/network") files
     return [ shellTest dp fmt tdir (toString $ "network" </> file) da
            | (file, (diffAlgorithmsPragma, repoFormatsPragma)) <- annotatedFiles
            , fmt <- fromMaybe repoFormatsDefault repoFormatsPragma
            , da <- fromMaybe diffAlgorithmsDefault diffAlgorithmsPragma ]

-- ----------------------------------------------------------------------
-- harness
-- ----------------------------------------------------------------------

data Config = Config { failing :: Bool
                     , shell :: Bool
                     , network :: Bool
                     , unit :: Bool
                     , myers :: Bool
                     , patience :: Bool
                     , darcs1 :: Bool
                     , darcs2 :: Bool
                     , darcs :: String
                     , tests :: [String]
                     , testDir :: Maybe FilePath
                     , plain :: Bool
                     , hideSuccesses :: Bool
                     , threads :: Int
                     , qcCount :: Int
                     }
            deriving (Data, Typeable, Eq)


defaultConfig :: Annotate Ann
defaultConfig
 = record Config{}
     [ failing       := False    += help "Run the failing (shell) tests [no]"
     , shell         := True     += help "Run the passing, non-network shell tests [yes]"
     , network       := False    += help "Run the network shell tests [no]"
     , unit          := True     += help "Run the unit tests [yes]"
     , myers         := False    += help "Use myers diff [no]"
     , patience      := True     += help "Use patience diff [yes]" += name "p"
     , darcs1        := False    += help "Use darcs-1 repo format [no]" += name "1"
     , darcs2        := True     += help "Use darcs-2 repo format [yes]" += name "2"
     , darcs         := ""       += help "Darcs binary path" += typ "PATH"
     , tests         := []       += help "Pattern to limit the tests to run" += typ "PATTERN" += name "t"
     , testDir       := Nothing  += help "Directory to run tests in" += typ "PATH" += name "d"
     , plain         := False    += help "Use plain-text output [no]"
     , hideSuccesses := False    += help "Hide successes [no]"
     , threads       := 1        += help "Number of threads [1]" += name "j"
     , qcCount       := 100      += help "Number of QuickCheck iterations per test [100]" += name "q"
     ]
   += summary "Darcs test harness"
   += program "darcs-test"

run :: Config -> IO ()
run conf = do
    let args = [ "-j", show $ threads conf ]
             ++ concat [ ["-t", x ] | x <- tests conf ]
             ++ [ "--plain" | True <- [plain conf] ]
             ++ [ "--hide-successes" | True <- [hideSuccesses conf] ]
                -- this multiplier is calibrated against the observed behaviour of the test harness -
                -- increase it if we see lots of "arguments exhausted" errors or similar
             ++ [ "--maximum-unsuitable-generated-tests", show (7 * qcCount conf) ]
             ++ [ "--maximum-generated-tests", show (qcCount conf) ]
    case testDir conf of
       Nothing -> return ()
       Just d  -> do e <- shelly (test_e (fromText $ pack d))
                     when e $ fail ("Directory " ++ d ++ " already exists. Cowardly exiting")
    darcsBin <-
        case darcs conf of
            "" -> do
                path <- getProgPath
                let candidates =
                      -- if darcs-test lives in foo/something, look for foo/darcs[.exe]
                      -- for example if we've done cabal install -ftest, there'll be a darcs-test and darcs in the cabal
                      -- installation folder
                      [path </> ("darcs" ++ exeSuffix)] ++
                      -- if darcs-test lives in foo/darcs-test/something, look for foo/darcs/darcs[.exe]
                      -- for example after cabal build we can run dist/build/darcs-test/darcs-test and it'll find
                      -- the darcs in dist/build/darcs/darcs
                      [takeDirectory path </> "darcs" </> ("darcs" ++ exeSuffix) | takeBaseName path == "darcs-test" ]
                availableCandidates <- filterM doesFileExist (map toString candidates)
                case availableCandidates of
                     (darcsBin:_) -> do
                         putStrLn $ "Using darcs executable in " ++ darcsBin
                         return darcsBin
                     [] -> fail ("No darcs specified or found nearby. Perhaps --darcs `pwd`/dist/build/darcs/darcs" ++ exeSuffix ++ "?")
            v -> return v
    when (shell conf || network conf || failing conf) $ do
      unless (isAbsolute $ darcsBin) $
        fail ("Argument to --darcs should be an absolute path")
      unless (exeSuffix `isSuffixOf` darcsBin) $
        putStrLn $ "Warning: --darcs flag does not end with " ++ exeSuffix ++ " - some tests may fail (case does matter)"

    putStrLn $ "Locale encoding is " ++ textEncodingName localeEncoding

    let repoFormat    = (if darcs1 conf then (Darcs1:) else id)
                      . (if darcs2 conf then (Darcs2:) else id)
                      $ []
    let diffAlgorithm = (if myers conf then (MyersDiff:) else id)
                      . (if patience conf then (PatienceDiff:) else id)
                      $ []

    ftests <- shelly $ if failing conf then findShell darcsBin (testDir conf) True diffAlgorithm repoFormat else return []
    stests <- shelly $ if shell conf then findShell darcsBin (testDir conf) False diffAlgorithm repoFormat else return []
    utests <- if unit conf then doUnit else return []
    ntests <- shelly $ if network conf then findNetwork darcsBin (testDir conf) diffAlgorithm repoFormat else return []
    defaultMainWithArgs (ftests ++ stests ++ utests ++ ntests) args
       where
          exeSuffix :: String
#ifdef WIN32
          exeSuffix = ".exe"
#else
          exeSuffix = ""
#endif

main :: IO ()
main = do hSetBinaryMode stdout True
          hSetBuffering stdout NoBuffering
          hSetBinaryMode stderr True
          hSetBinaryMode stdin True
          clp  <- cmdArgs_ defaultConfig
          run clp

#endif
