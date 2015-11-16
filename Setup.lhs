\begin{code}
{-# LANGUAGE TemplateHaskell #-}
-- copyright (c) 2008 Duncan Coutts
-- portions copyright (c) 2008 David Roundy
-- portions copyright (c) 2007-2009 Judah Jacobson

import Prelude hiding ( catch )
import qualified Prelude

import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Configure
         ( checkForeignDeps )
import Distribution.ModuleName( toFilePath )
import Distribution.PackageDescription
         ( PackageDescription(executables, testSuites), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , TestSuite(testBuildInfo)
         , FlagName(FlagName)
         , updatePackageDescription
         , cppOptions, ccOptions, ldOptions
         , library, libBuildInfo, otherModules
         , extraLibs, extraLibDirs, includeDirs )
import Distribution.Package
         ( packageVersion, packageName, PackageName(..), Package )
import Distribution.Version
         ( Version(Version, versionBranch) )
import Data.Version( showVersion )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), absoluteInstallDirs, externalPackageDeps )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.PackageIndex ( topologicalOrder )
import Distribution.Simple.Program ( gccProgram, rawSystemProgramStdoutConf )
import Distribution.Simple.Setup
    (buildVerbosity, copyDest, copyVerbosity, fromFlag,
     haddockVerbosity, installVerbosity, sDistVerbosity,
     configVerbosity, ConfigFlags, configConfigurationsFlags)
import qualified Distribution.Simple.Setup as DSS -- to get replVerbosity in Cabal > 1.18
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, exeExtension )
import Distribution.System
         ( OS(Windows), buildOS )
import Distribution.Simple.Utils
    (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout,
     rewriteFile, withTempFile, cabalVersion)
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )
import Control.Monad ( zipWithM_, when, unless, filterM )
import Control.Exception ( bracket, handle, IOException )

import Language.Haskell.TH ( mkName, newName, recUpdE, varE, appE, lamE, varP )

import System.Directory
    (copyFile, createDirectory, createDirectoryIfMissing,
     doesDirectoryExist, doesFileExist,
     getCurrentDirectory, getDirectoryContents,
     removeDirectoryRecursive, removeFile, setCurrentDirectory,
     getTemporaryDirectory
    )
import System.Exit ( ExitCode(ExitSuccess) )
import System.IO
    ( openFile, IOMode (..), stdout
    , hPutStr, hFlush, hClose
    )
import System.Process (runProcess)
import System.IO.Error ( isDoesNotExistError )
import Data.List( isPrefixOf, isSuffixOf, sort )
import System.Process( rawSystem )
import System.FilePath       ( (</>), (<.>), splitDirectories, isAbsolute )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( castPtr )
import Data.Monoid           ( mappend )
import Data.Word             ( Word8, Word32 )

import qualified Control.Exception as Exception

catchAny f h = Exception.catch f (\e -> h (e :: Exception.SomeException))

{- Template Haskell hackery for replHook while we want to support Cabal < 1.18 -}
replVerbosity =
  $(if cabalVersion >= Version [1,18,0] []
        then varE (mkName "DSS.replVerbosity")
        else [| error "This shouldn't be called" |]
   )

replHookBody replHookSel =
  \pkg lbi hooks flags args ->
    let verb = fromFlag $ replVerbosity flags
    in commonBuildHook replHookSel pkg lbi hooks verb >>= (\f -> f flags args)

addReplHook =
  $(if cabalVersion >= Version [1,18,0] []
        then
            do hooks <- newName "hooks"
               let replHook = mkName "replHook"
               app <- appE (varE (mkName "replHookBody")) (varE replHook)
               lamE [varP hooks] (recUpdE (varE hooks) [return (replHook, app)])
        else [| \hooks -> hooks |]
   )
{- End of Template Haskell hackery -}

main :: IO ()
main = defaultMainWithHooks $ addReplHook $ simpleUserHooks {

  buildHook = \ pkg lbi hooks flags ->
              let verb = fromFlag $ buildVerbosity flags
               in commonBuildHook buildHook pkg lbi hooks verb >>= ($ flags),

  haddockHook = \ pkg lbi hooks flags ->
                let verb = fromFlag $ haddockVerbosity flags
                 in commonBuildHook haddockHook pkg lbi hooks verb >>= ($ flags) ,
{-
  -- this is the actual replHook code we want
  replHook = \pkg lbi hooks flags args ->
                let verb = fromFlag $ replVerbosity flags
                 in commonBuildHook replHook pkg lbi hooks verb >>= (\f -> f flags args) ,
-}
  postBuild = \ _ _ _ lbi -> buildManpage lbi,
  postCopy = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags),
  postInst = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest,

  sDistHook = \ pkg lbi hooks flags -> do
    let pkgVer = packageVersion pkg
        verb = fromFlag $ sDistVerbosity flags
    x <- versionPatches verb pkgVer
    y <- context verb pkgVer
    rewriteFile "release/distributed-version" $ show x
    rewriteFile "release/distributed-context" $ show y
    putStrLn "about to hand over"
    let pkg' = pkg { library = sanity (library pkg) }
        sanity (Just lib) = Just $ lib { libBuildInfo = sanity' $ libBuildInfo lib }
        sanity _ = error "eh"
        sanity' bi = bi { otherModules = [ m | m <- otherModules bi, toFilePath m /= "Version" ] }

    sDistHook simpleUserHooks pkg' lbi hooks flags
             ,
  confHook =
    if buildOS == Windows
      then confHook simpleUserHooks
      else
        \genericDescript flags -> do
          lbi <- confHook simpleUserHooks genericDescript flags
          let pkgDescr = localPkgDescr lbi
          let verb = fromFlag (configVerbosity flags)
          checkForeignDeps pkgDescr lbi verb
          let Just lib = library pkgDescr
          let bi = libBuildInfo lib
          bi' <- maybeSetLibiconv flags bi lbi
          return lbi {localPkgDescr = pkgDescr {
                                  library = Just lib {
                                      libBuildInfo = bi'}}}
              ,
  postConf = \_ _ _ _ -> return () --- Usually this checked for external C
             --- dependencies, but we already have performed such
             --- check in the confHook
}

-- | For @./Setup build@ and @./Setup haddock@, do some unusual
-- things, then invoke the base behaviour ("simple hook").
commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a)
                -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  (version, state) <- determineVersion verbosity pkg

  -- Create our own context file.
  generateVersionModule verbosity pkg lbi version state

  -- Add custom -DFOO[=BAR] flags to the cpp (for .hs) and cc (for .c)
  -- invocations, doing a dance to make the base hook aware of them.
  littleEndian <- testEndianness
  let args = ("-DPACKAGE_VERSION=" ++ show' version) :
             [arg | (arg, True) <-         -- include fst iff snd.
              [("-DHAVE_HTTP", "x-have-http" `elem` customFields),
               -- We have MAPI iff building on/for Windows.
               ("-DHAVE_MAPI", buildOS == Windows),
               ("-DLITTLEENDIAN", littleEndian),
               ("-DBIGENDIAN", not littleEndian)]]
      bi = emptyBuildInfo { cppOptions = args, ccOptions = args }
      hbi = (Just bi, [(exeName exe, bi) | exe <- executables pkg])
      pkg' = updatePackageDescription hbi pkg

      -- updatePackageDescription doesn't handle test suites so we
      -- need to do this manually
      updateTestSuiteBI bi testSuite
          = testSuite { testBuildInfo = bi `mappend` testBuildInfo testSuite }
      pkg'' = pkg' { testSuites = map (updateTestSuiteBI bi) (testSuites pkg') }

      lbi' = lbi { localPkgDescr = pkg'' }
  return $ runHook simpleUserHooks pkg'' lbi' hooks

  where
    customFields = map fst . customFieldsBI . buildInfo $ darcsExe
    darcsExe = head [e | e <- executables pkg, exeName e == "darcs"]
    show' :: String -> String   -- Petr was worried that we might
    show' = show                -- allow non-String arguments.
    testEndianness :: IO Bool
    testEndianness = with (1 :: Word32) $ \p -> do o <- peek $ castPtr p
                                                   return $ o == (1 :: Word8)

-- ---------------------------------------------------------------------
-- man page
-- ---------------------------------------------------------------------

buildManpage :: LocalBuildInfo -> IO ()
buildManpage lbi = do
  let darcs = buildDir lbi </> "darcs/darcs"
      manpage = buildDir lbi </> "darcs/darcs.1"
  manpageHandle <- openFile manpage WriteMode
  runProcess darcs ["help","manpage"]
             Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo
                  -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy =
    copyFiles verbosity
              (mandir (absoluteInstallDirs pkg lbi copy) </> "man1")
              [(buildDir lbi </> "darcs", "darcs.1")]

-- ---------------------------------------------------------------------
-- version module
-- ---------------------------------------------------------------------

determineVersion :: Verbosity -> PackageDescription -> IO (String, String)
determineVersion verbosity pkg = do
  let darcsVersion  =  packageVersion pkg
  numPatches <- versionPatches verbosity darcsVersion
  return (display darcsVersion, versionStateString numPatches darcsVersion)

  where
    versionStateString :: Maybe Int -> Version -> String
    versionStateString Nothing  _ = "unknown"
    versionStateString (Just 0) v = case versionBranch v of
                         x | 97 `elem` x -> "alpha " ++ show (after 97 x)
                           | 98 `elem` x -> "beta " ++ show (after 98 x)
                           | 99 `elem` x  ->
                               "release candidate " ++ show (after 99 x)
                         _ -> "release"
    versionStateString (Just 1) _ = "+ 1 patch"
    versionStateString (Just n) _ = "+ " ++ show n ++ " patches"
    after w (x:r) | w == x = head r
                  | otherwise = after w r
    after _ [] = undefined

versionPatches :: Verbosity -> Version -> IO (Maybe Int)
versionPatches verbosity darcsVersion = do
  numPatchesDarcs <- do
      out <- rawSystemStdout verbosity "darcs"
               ["log", "-a", "--from-tag", display darcsVersion, "--count"]
      case reads (out) of
        ((n,_):_) -> return $ Just ((n :: Int) - 1)
        _         -> return Nothing
    `catchAny` \_ -> return Nothing

  numPatchesDist <- parseFile versionFile
  return $ case (numPatchesDarcs, numPatchesDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing

 where
  versionFile = "release/distributed-version"

generateVersionModule :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> String -> String -> IO ()
generateVersionModule verbosity pkg lbi version state = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  ctx <- context verbosity (packageVersion pkg)
  rewriteFile (dir </> "Version.hs") $ unlines
    ["module Version where"
    ,"builddeps, version, context :: String"
    ,"version = \"" ++ version ++ " (" ++ state ++ ")\""
    ,"builddeps = " ++ show ( formatdeps (externalPackageDeps lbi))
    ,"context = " ++ case ctx of
                       Just x -> show x
                       Nothing -> show "context not available"
    ]
  where formatdeps = unlines . map (formatone . snd)
        formatone p = case packageName p of PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

context :: Verbosity -> Version -> IO (Maybe String)
context verbosity version = do
  contextDarcs <- do
      inrepo <- doesDirectoryExist "_darcs"
      unless inrepo $ fail "Not a repository."
      out <- rawSystemStdout verbosity "darcs" ["log", "-a", "--context"]
      return $ Just out
   `catchAny` \_ -> return Nothing

  contextDist <- parseFile contextFile
  return $ case (contextDarcs, contextDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing
 where contextFile = "release/distributed-context"

parseFile :: (Read a) => String -> IO (Maybe a)
parseFile f = do
  exist <- doesFileExist f
  if exist then do
             content <- readFile f -- ^ ratify readFile: we don't care here.
             case reads content of
               ((s,_):_) -> return s
               _         -> return Nothing
             else return Nothing

-- Test whether compiling a c program that links against libiconv needs -liconv.
maybeSetLibiconv :: ConfigFlags -> BuildInfo -> LocalBuildInfo -> IO BuildInfo
maybeSetLibiconv flags bi lbi = do
    let biWithIconv = addIconv bi
    let verb = fromFlag (configVerbosity flags)
    if hasFlagSet flags (FlagName "libiconv")
        then do
            putStrLn "Using -liconv."
            return biWithIconv
        else do
    putStr "checking whether to use -liconv... "
    hFlush stdout
    worksWithout <- tryCompile iconv_prog bi lbi verb
    if worksWithout
        then do
            putStrLn "not needed."
            return bi
        else do
    worksWith <- tryCompile iconv_prog biWithIconv lbi verb
    if worksWith
        then do
            putStrLn "using -liconv."
            return biWithIconv
        else error "Unable to link against the iconv library."

hasFlagSet :: ConfigFlags -> FlagName -> Bool
hasFlagSet cflags flag = Just True == lookup flag (configConfigurationsFlags cflags)

tryCompile :: String -> BuildInfo -> LocalBuildInfo -> Verbosity -> IO Bool
tryCompile program bi lbi verb = handle processExit $ handle processException $ do
    tempDir <- getTemporaryDirectory
    withTempFile tempDir ".c" $ \fname cH ->
      withTempFile tempDir "" $ \execName oH -> do
        hPutStr cH program
        hClose cH
        hClose oH
        -- TODO take verbosity from the args.
        rawSystemProgramStdoutConf verb gccProgram (withPrograms lbi)
                        (fname : "-o" : execName : args)
        return True
  where
    processException :: IOException -> IO Bool
    processException e = return False
    processExit = return . (==ExitSuccess)
    -- Mimicing Distribution.Simple.Configure
    deps = topologicalOrder (installedPkgs lbi)
    args = concat
                  [ ccOptions bi
                  , cppOptions bi
                  , ldOptions bi
                  -- --extra-include-dirs and --extra-lib-dirs are included
                  -- in the below fields.
                  -- Also sometimes a dependency like rts points to a nonstandard
                  -- include/lib directory where iconv can be found. 
                  , map ("-I" ++) (includeDirs bi ++ concatMap Installed.includeDirs deps)
                  , map ("-L" ++) (extraLibDirs bi ++ concatMap Installed.libraryDirs deps)
                  , map ("-l" ++) (extraLibs bi)
                  ]

addIconv :: BuildInfo -> BuildInfo
addIconv bi = bi {extraLibs = "iconv" : extraLibs bi}

iconv_prog :: String
iconv_prog = unlines 
    [ "#include <iconv.h>"
    , "int main(void) {"
    , "    iconv_t t = iconv_open(\"UTF-8\", \"UTF-8\");"
    , "    return 0;"
    , "}"
    ]

\end{code}
