--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.Repository.Prefs
    ( addToPreflist
    , deleteSources
    , getPreflist
    , setPreflist
    , getGlobal
    , environmentHelpHome
    , defaultrepo
    , getDefaultRepoPath
    , addRepoSource
    , getPrefval
    , setPrefval
    , changePrefval
    , defPrefval
    , writeDefaultPrefs
    , boringRegexps
    , boringFileFilter
    , darcsdirFilter
    , FileType(..)
    , filetypeFunction
    , getCaches
    , binariesFileHelp
    , boringFileHelp
    , globalCacheDir
    , globalPrefsDirDoc
    , globalPrefsDir
    , oldGlobalCacheDir
    ) where

import Control.Exception ( catch )
import Control.Monad ( unless, when, liftM )
import Data.Char ( toUpper )
import Data.List ( nub, isPrefixOf, union, sortBy )
import Data.Maybe ( isJust, fromMaybe, mapMaybe, catMaybes, maybeToList )
import Prelude hiding ( catch )
import qualified Control.Exception as C
import qualified Data.ByteString       as B  ( empty )
import qualified Data.ByteString.Char8 as BC ( unpack )
import System.Directory ( getAppUserDataDirectory, doesDirectoryExist,
                          createDirectory, doesFileExist )
import System.Environment ( getEnvironment )
import System.FilePath.Posix ( normalise, dropTrailingPathSeparator, (</>) )
import System.IO.Error ( isDoesNotExistError )
import System.IO ( stderr )
import System.Info ( os )
import Text.Regex ( Regex, mkRegex, matchRegex )

import Darcs.Repository.Cache ( Cache(..), CacheType(..), CacheLoc(..),
                                WritableOrNot(..), compareByLocality )
import Darcs.Repository.External ( gzFetchFilePS , Cachable( Cachable ))
import Darcs.Repository.Flags( UseCache (..), DryRun (..), SetDefault (..),
                               RemoteRepos (..) )
import Darcs.Repository.Lock( readBinFile, writeBinFile )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Path ( AbsolutePath, ioAbsolute, toFilePath,
                         getCurrentDirectory )
import Darcs.Util.Printer( hPutDocLn, text, RenderMode(..) )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.File ( osxCacheDir, xdgCacheDir, removeFileMayNotExist )

windows,osx :: Bool
windows = "mingw" `isPrefixOf` os -- GHC under Windows is compiled with mingw
osx     = os == "darwin"

writeDefaultPrefs :: IO ()
writeDefaultPrefs = do
    setPreflist "boring" defaultBoring
    setPreflist "binaries" defaultBinaries
    setPreflist "motd" []

{-# NOINLINE defaultBoring #-}
defaultBoring :: [String]
defaultBoring = map ("# " ++) boringFileHelp ++
    [ ""
    , "### compiler and interpreter intermediate files"
    , "# haskell (ghc) interfaces"
    , "\\.hi$", "\\.hi-boot$", "\\.o-boot$"
    , "# object files"
    , "\\.o$","\\.o\\.cmd$"
    , "# profiling haskell"
    , "\\.p_hi$", "\\.p_o$"
    , "# haskell program coverage resp. profiling info"
    , "\\.tix$", "\\.prof$"
    , "# fortran module files"
    , "\\.mod$"
    , "# linux kernel"
    , "\\.ko\\.cmd$","\\.mod\\.c$"
    , "(^|/)\\.tmp_versions($|/)"
    , "# *.ko files aren't boring by default because they might"
    , "# be Korean translations rather than kernel modules"
    , "# \\.ko$"
    , "# python, emacs, java byte code"
    , "\\.py[co]$", "\\.elc$","\\.class$"
    , "# objects and libraries; lo and la are libtool things"
    , "\\.(obj|a|exe|so|lo|la)$"
    , "# compiled zsh configuration files"
    , "\\.zwc$"
    , "# Common LISP output files for CLISP and CMUCL"
    , "\\.(fas|fasl|sparcf|x86f)$"
    , ""
    , "### build and packaging systems"
    , "# cabal intermediates"
    , "\\.installed-pkg-config"
    , "\\.setup-config"
    , "# standard cabal build dir, might not be boring for everybody"
    , "# ^dist(/|$)"
    , "# autotools"
    , "(^|/)autom4te\\.cache($|/)", "(^|/)config\\.(log|status)$"
    , "# microsoft web expression, visual studio metadata directories"
    , "\\_vti_cnf$"
    , "\\_vti_pvt$"
    , "# gentoo tools"
    , "\\.revdep-rebuild.*"
    , "# generated dependencies"
    , "^\\.depend$"
    , ""
    , "### version control systems"
    , "# cvs"
    , "(^|/)CVS($|/)","\\.cvsignore$"
    , "# cvs, emacs locks"
    , "^\\.#"
    , "# rcs"
    , "(^|/)RCS($|/)", ",v$"
    , "# subversion"
    , "(^|/)\\.svn($|/)"
    , "# mercurial"
    , "(^|/)\\.hg($|/)"
    , "# git"
    , "(^|/)\\.git($|/)"
    , "# bzr"
    , "\\.bzr$"
    , "# sccs"
    , "(^|/)SCCS($|/)"
    , "# darcs"
    , "(^|/)"++darcsdir++"($|/)", "(^|/)\\.darcsrepo($|/)"
    , "^\\.darcs-temp-mail$"
    , "-darcs-backup[[:digit:]]+$"
    , "# gnu arch"
    , "(^|/)(\\+|,)"
    , "(^|/)vssver\\.scc$"
    , "\\.swp$","(^|/)MT($|/)"
    , "(^|/)\\{arch\\}($|/)","(^|/).arch-ids($|/)"
    , "# bitkeeper"
    , "(^|/)BitKeeper($|/)","(^|/)ChangeSet($|/)"
    , ""
    , "### miscellaneous"
    , "# backup files"
    , "~$","\\.bak$","\\.BAK$"
    , "# patch originals and rejects"
    , "\\.orig$", "\\.rej$"
    , "# X server"
    , "\\..serverauth.*"
    , "# image spam"
    , "\\#", "(^|/)Thumbs\\.db$"
    , "# vi, emacs tags"
    , "(^|/)(tags|TAGS)$"
    , "#(^|/)\\.[^/]"
    , "# core dumps"
    , "(^|/|\\.)core$"
    , "# partial broken files (KIO copy operations)"
    , "\\.part$"
    , "# waf files, see http://code.google.com/p/waf/"
    , "(^|/)\\.waf-[[:digit:].]+-[[:digit:]]+($|/)"
    , "(^|/)\\.lock-wscript$"
    , "# mac os finder"
    , "(^|/)\\.DS_Store$"
    , "# emacs saved sessions (desktops)"
    , "(^|.*/)\\.emacs\\.desktop(\\.lock)?$"
    ]

boringFileHelp :: [String]
boringFileHelp =
    [ "This file contains a list of extended regular expressions, one per"
    , "line. A file path matching any of these expressions will be filtered"
    , "out during `darcs add', or when the `--look-for-adds' flag is passed"
    , "to `darcs whatsnew' and `record'. The entries in "
        ++ globalPrefsDirDoc ++ "boring (if"
    , "it exists) supplement those in this file."
    , ""
    , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
    , "See regex(7) for a description of extended regular expressions."
    ]

darcsdirFilter :: [FilePath] -> [FilePath]
darcsdirFilter = filter (not . isDarcsdir)

isDarcsdir :: FilePath -> Bool
isDarcsdir ('.' : '/' : f) = isDarcsdir f
isDarcsdir "." = True
isDarcsdir "" = True
isDarcsdir ".." = True
isDarcsdir "../" = True
isDarcsdir fp = (darcsdir ++ "/") `isPrefixOf` fp || fp == darcsdir

-- | The path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows.
globalPrefsDir :: IO (Maybe FilePath)
globalPrefsDir = do
    env <- getEnvironment
    case lookup "DARCS_TESTING_PREFS_DIR" env of
        Just d -> return (Just d)
        Nothing -> Just `fmap` getAppUserDataDirectory "darcs"
                   `catchall` return Nothing

-- | The relative path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows. This is used for online documentation.
globalPrefsDirDoc :: String
globalPrefsDirDoc | windows   = "%APPDATA%\\darcs\\"
                  | otherwise = "~/.darcs/"

environmentHelpHome :: ([String], [String])
environmentHelpHome =
    ( ["HOME", "APPDATA"]
    , [ "Per-user preferences are set in $HOME/.darcs (on Unix) or"
      , "%APPDATA%/darcs (on Windows).  This is also the default location of"
      , "the cache."
      ]
    )

getGlobal :: String -> IO [String]
getGlobal f = do
    dir <- globalPrefsDir
    case dir of
        (Just d) -> getPreffile $ d </> f
        Nothing -> return []

globalCacheDir :: IO (Maybe FilePath)
globalCacheDir | windows   = ((</> "cache2") `fmap`) `fmap` globalPrefsDir
               | osx       = ((</> "darcs") `fmap`) `fmap` osxCacheDir
               | otherwise = ((</> "darcs") `fmap`) `fmap` xdgCacheDir

-- |oldGlobalCacheDir is the old cache path @~/.darcs/cache@
-- now ony used with read-only access.
oldGlobalCacheDir :: IO (Maybe FilePath)
oldGlobalCacheDir
    = do dir <- ((</> "cache") `fmap`) `fmap` globalPrefsDir
         case dir of
           Nothing -> return Nothing
           Just d  -> do exists <- doesDirectoryExist d
                         if exists
                          then return $ Just d
                          else return Nothing

-- |tryMakeBoringRegexp attempts to create a Regex from a given String. The
-- evaluation is forced, to ensure any malformed exceptions are thrown here,
-- and not later.
tryMakeBoringRegexp :: String -> IO (Maybe Regex)
tryMakeBoringRegexp input = regex `C.catch` handleBadRegex
  where
    regex = C.evaluate (Just $! mkRegex input)

    handleBadRegex :: C.SomeException -> IO (Maybe Regex)
    handleBadRegex _ = hPutDocLn Encode stderr warning >> return Nothing

    warning = text $ "Warning: Ignored invalid boring regex: " ++ input

-- |boringRegexps returns a list of the boring regexps, from the local and
-- global prefs/boring files. Any invalid regexps are filtered, preventing an
-- exception in (potentially) pure code, when the regexps are used.
boringRegexps :: IO [Regex]
boringRegexps = do
    borefile <- defPrefval "boringfile" (darcsdir ++ "/prefs/boring")
    localBores <- getPrefLines borefile `catchall` return []
    globalBores <- getGlobal "boring"
    liftM catMaybes $ mapM tryMakeBoringRegexp $ localBores ++ globalBores

boringFileFilter :: IO ([FilePath] -> [FilePath])
boringFileFilter = filterBoringAndDarcsdir `fmap` boringRegexps
  where
    filterBoringAndDarcsdir regexps = filter (notBoring regexps . doNormalise)
    notBoring regexps file = not $
        isDarcsdir file || any (\r -> isJust $ matchRegex r file) regexps

noncomments :: [String] -> [String]
noncomments = filter nonComment
  where
    nonComment "" = False
    nonComment ('#' : _) = False
    nonComment _ = True

getPrefLines :: FilePath -> IO [String]
getPrefLines f = removeCRsCommentsAndConflicts `fmap` readBinFile f
  where
    removeCRsCommentsAndConflicts =
        filter notconflict . noncomments . map stripCr . lines
    startswith [] _ = True
    startswith (x : xs) (y : ys) = x == y && startswith xs ys
    startswith _ _ = False
    notconflict l
        | startswith "v v v v v v v" l = False
        | startswith "*************" l = False
        | startswith "^ ^ ^ ^ ^ ^ ^" l = False
        | otherwise = True
    stripCr ""     = ""
    stripCr "\r"   = ""
    stripCr (c : cs) = c : stripCr cs

doNormalise :: FilePath -> FilePath
doNormalise = dropTrailingPathSeparator . normalise

data FileType = BinaryFile
              | TextFile
              deriving (Eq)

{-# NOINLINE defaultBinaries #-}
-- | The lines that will be inserted into @_darcs/prefs/binaries@ when
-- @darcs init@ is run.  Hence, a list of comments, blank lines and
-- regular expressions (ERE dialect).
--
-- Note that while this matches .gz and .GZ, it will not match .gZ,
-- i.e. it is not truly case insensitive.
defaultBinaries :: [String]
defaultBinaries = map ("# "++) binariesFileHelp ++
    [ "\\." ++ regexToMatchOrigOrUpper e ++ "$" | e <- extensions ]
  where
    regexToMatchOrigOrUpper e = "(" ++ e ++ "|" ++ map toUpper e ++ ")"
    extensions =
        [ "a"
        , "bmp"
        , "bz2"
        , "doc"
        , "elc"
        , "exe"
        , "gif"
        , "gz"
        , "iso"
        , "jar"
        , "jpe?g"
        , "mng"
        , "mpe?g"
        , "p[nbgp]m"
        , "pdf"
        , "png"
        , "pyc"
        , "so"
        , "tar"
        , "tgz"
        , "tiff?"
        , "z"
        , "zip"
        ]

binariesFileHelp :: [String]
binariesFileHelp =
    [ "This file contains a list of extended regular expressions, one per"
    , "line.  A file path matching any of these expressions is assumed to"
    , "contain binary data (not text). The entries in "
        ++ globalPrefsDirDoc ++ "binaries (if"
    , "it exists) supplement those in this file."
    , ""
    , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
    , "See regex(7) for a description of extended regular expressions."
    ]

filetypeFunction :: IO (FilePath -> FileType)
filetypeFunction = do
    binsfile <- defPrefval "binariesfile" (darcsdir ++ "/prefs/binaries")
    bins <- getPrefLines binsfile
            `catch`
            (\e -> if isDoesNotExistError e then return [] else ioError e)
    gbs <- getGlobal "binaries"
    let binaryRegexes = map mkRegex (bins ++ gbs)
        isBinary f = any (\r -> isJust $ matchRegex r f) binaryRegexes
        ftf f = if isBinary $ doNormalise f then BinaryFile else TextFile
    return ftf

findPrefsDirectory :: IO (Maybe String)
findPrefsDirectory = do
    inDarcsRepo <- doesDirectoryExist darcsdir
    return $ if inDarcsRepo
                 then Just $ darcsdir ++ "/prefs/"
                 else Nothing

withPrefsDirectory :: (String -> IO ()) -> IO ()
withPrefsDirectory job = findPrefsDirectory >>= maybe (return ()) job

addToPreflist :: String -> String -> IO ()
addToPreflist pref value = withPrefsDirectory $ \prefs -> do
    hasprefs <- doesDirectoryExist prefs
    unless hasprefs $ createDirectory prefs
    pl <- getPreflist pref
    writeBinFile (prefs ++ pref) . unlines $ union [value] pl

getPreflist :: String -> IO [String]
getPreflist p = findPrefsDirectory >>=
                maybe (return []) (\prefs -> getPreffile $ prefs ++ p)

getPreffile :: FilePath -> IO [String]
getPreffile f = do
    hasprefs <- doesFileExist f
    if hasprefs then getPrefLines f else return []

setPreflist :: String -> [String] -> IO ()
setPreflist p ls = withPrefsDirectory $ \prefs -> do
    haspref <- doesDirectoryExist prefs
    when haspref $
        writeBinFile (prefs ++ p) (unlines ls)

defPrefval :: String -> String -> IO String
defPrefval p d = fromMaybe d `fmap` getPrefval p

getPrefval :: String -> IO (Maybe String)
getPrefval p = do
    pl <- getPreflist "prefs"
    return $ case map snd $ filter ((== p) . fst) $ map (break (== ' ')) pl of
                 [val] -> case words val of
                    [] -> Nothing
                    _ -> Just $ tail val
                 _ -> Nothing

setPrefval :: String -> String -> IO ()
setPrefval p v = do
    pl <- getPreflist "prefs"
    setPreflist "prefs" $ updatePrefVal pl p v

updatePrefVal :: [String] -> String -> String -> [String]
updatePrefVal prefList p newVal =
    filter ((/= p) . fst . break (== ' ')) prefList ++ [p ++ " " ++ newVal]

changePrefval :: String -> String -> String -> IO ()
changePrefval p f t = do
    pl <- getPreflist "prefs"
    ov <- getPrefval p
    let newval = maybe t (\old -> if old == f then t else old) ov
    setPreflist "prefs" $ updatePrefVal pl p newval

fixRepoPath :: String -> IO FilePath
fixRepoPath p
    | isValidLocalPath p = toFilePath `fmap` ioAbsolute p
    | otherwise = return p

defaultrepo :: RemoteRepos -> AbsolutePath -> [String] -> IO [String]
defaultrepo (RemoteRepos rrepos) _ [] =
  do case rrepos of
       [] -> maybeToList `fmap` getDefaultRepoPath
       rs -> mapM fixRepoPath rs
defaultrepo _ _ r = return r

getDefaultRepoPath :: IO (Maybe String)
getDefaultRepoPath = do
    defaults <- getPreflist defaultRepoPref
    case defaults of
         [] -> return Nothing
         (d : _) -> Just `fmap` fixRepoPath d

defaultRepoPref :: String
defaultRepoPref = "defaultrepo"

-- | addRepoSource adds a new entry to _darcs/prefs/repos and sets it as default
--   in _darcs/prefs/defaultrepo, unless --no-set-default or --dry-run is passed,
--   or it is the same repository as the current one.
addRepoSource :: String -> DryRun -> RemoteRepos -> SetDefault -> IO ()
addRepoSource r isDryRun (RemoteRepos rrepos) setDefault = (do
    olddef <- getPreflist defaultRepoPref
    let shouldDoIt = null noSetDefault && greenLight
        greenLight = shouldAct && not rIsTmp && (olddef /= [r] || olddef == [])
    -- the nuance here is that we should only notify when the reason we're not
    -- setting default is the --no-set-default flag, not the various automatic
    -- show stoppers
    if shouldDoIt
       then setPreflist defaultRepoPref [r]
       else when (True `notElem` noSetDefault && greenLight) $
                putStr . unlines $ setDefaultMsg
    addToPreflist "repos" r) `catchall` return ()
  where
    shouldAct = isDryRun == NoDryRun
    rIsTmp = r `elem` rrepos
    noSetDefault = case setDefault of
                       NoSetDefault x -> [x]
                       _ -> []
    setDefaultMsg =
        [ "HINT: if you want to change the default remote repository to"
        , "      " ++ r ++ ","
        , "      quit now and issue the same command with the --set-default "
          ++ "flag."
        ]

-- | delete references to other repositories.
--   Used when cloning to a ssh destination.
--   Assume the current working dir is the repository.
deleteSources :: IO ()
deleteSources = do let prefsdir = darcsdir ++ "/prefs/"
                   removeFileMayNotExist (prefsdir ++ "sources")
                   removeFileMayNotExist (prefsdir ++ "repos")

getCaches :: UseCache -> String -> IO Cache
getCaches useCache repodir = do
    here <- parsehs `fmap` getPreffile sourcesFile
    there <- (parsehs . lines . BC.unpack)
             `fmap`
             (gzFetchFilePS (repodir </> sourcesFile) Cachable
              `catchall` return B.empty)
    oldGlobalcachedir <- oldGlobalCacheDir
    globalcachedir <- globalCacheDir
    let oldGlobalcache = if nocache then []
                        else case oldGlobalcachedir of
                              Nothing -> []
                              Just d -> [Cache Directory NotWritable d]
    let globalcache = if nocache
                          then []
                          else case globalcachedir of
                              Nothing -> []
                              Just d -> [Cache Directory Writable d]
    globalsources <- parsehs `fmap` getGlobal "sources"
    thisdir <- getCurrentDirectory
    let thisrepo = [Cache Repo Writable $ toFilePath thisdir]
        thatrepo = [Cache Repo NotWritable repodir]
        tempCache = nub $ thisrepo ++ globalcache ++ globalsources ++ here
                          ++ thatrepo ++ filterExternalSources there
                          ++ oldGlobalcache
    return $ Ca $ sortBy compareByLocality tempCache
  where
    sourcesFile = darcsdir ++ "/prefs/sources"

    parsehs = mapMaybe readln . noncomments

    readln l
        | "repo:" `isPrefixOf` l = Just (Cache Repo NotWritable (drop 5 l))
        | nocache = Nothing
        | "cache:" `isPrefixOf` l = Just (Cache Directory Writable (drop 6 l))
        | "readonly:" `isPrefixOf` l =
            Just (Cache Directory NotWritable (drop 9 l))
        | otherwise = Nothing

    nocache = useCache == NoUseCache

    filterExternalSources there =
        if isValidLocalPath repodir
            then there
            else filter (not . isValidLocalPath . cacheSource) there
