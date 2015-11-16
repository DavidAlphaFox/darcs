{-# LANGUAGE CPP #-}

module Darcs.Repository.Cache
    ( cacheHash
    , okayHash
    , takeHash
    , Cache(..)
    , CacheType(..)
    , CacheLoc(..)
    , WritableOrNot(..)
    , HashedDir(..)
    , hashedDir
    , bucketFolder
    , unionCaches
    , unionRemoteCaches
    , cleanCaches
    , cleanCachesWithHint
    , fetchFileUsingCache
    , speculateFileUsingCache
    , speculateFilesUsingCache
    , writeFileUsingCache
    , peekInCache
    , repo2cache
    , writable
    , isThisRepo
    , hashedFilePath
    , allHashedDirs
    , compareByLocality
    , reportBadSources
    ) where

import Control.Monad ( liftM, when, guard, unless, filterM, forM_, mplus )
import qualified Data.ByteString as B (length, drop, ByteString )
import qualified Data.ByteString.Char8 as BC (unpack)
import Data.List ( nub, intercalate )
import Data.Maybe ( catMaybes, listToMaybe, fromMaybe )
import System.FilePath.Posix ( (</>), joinPath, dropFileName )
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist,
                          doesDirectoryExist, getDirectoryContents,
                          getPermissions )
import qualified System.Directory as SD ( writable )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Files ( createLink, linkCount, getSymbolicLinkStatus )

import Darcs.Util.ByteString ( gzWriteFilePS, linesPS )
import Darcs.Util.Global ( darcsdir, addBadSource, isBadSource, addReachableSource,
                      isReachableSource, getBadSourcesList )
import Darcs.Repository.External ( gzFetchFilePS, fetchFilePS,
                                   speculateFileOrUrl, copyFileOrUrl,
                                   Cachable( Cachable ) )
import Darcs.Repository.Flags ( Compression(..), RemoteDarcs(..) )
import Darcs.Repository.Lock ( writeAtomicFilePS, gzWriteAtomicFilePS,
                               withTemp )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.URL ( isValidLocalPath, isHttpUrl, isSshUrl )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Crypt.SHA1 ( sha1PS )
import Darcs.Util.Crypt.SHA256 ( sha256sum )
import Darcs.Util.English ( englishNum, Noun(..), Pronoun(..) )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Progress ( progressList, debugMessage, debugFail )
import qualified Darcs.Util.Download as Download ( ConnectionError(..) )

data HashedDir = HashedPristineDir
               | HashedPatchesDir
               | HashedInventoriesDir

hashedDir :: HashedDir -> String
hashedDir HashedPristineDir = "pristine.hashed"
hashedDir HashedPatchesDir = "patches"
hashedDir HashedInventoriesDir = "inventories"

allHashedDirs :: [HashedDir]
allHashedDirs = [ HashedPristineDir
                , HashedPatchesDir
                , HashedInventoriesDir
                ]

data WritableOrNot = Writable
                   | NotWritable
                   deriving ( Eq, Show )

data CacheType = Repo
               | Directory
               deriving ( Eq, Show )

data CacheLoc = Cache
    { cacheType :: !CacheType
    , cacheWritable :: !WritableOrNot
    , cacheSource :: !String
    }

-- | Cache is an abstract type for hiding the underlying cache locations
newtype Cache = Ca [CacheLoc]

instance Eq CacheLoc where
    (Cache aTy _ aSrc) == (Cache bTy _ bSrc) = aTy == bTy && aSrc == bSrc

instance Show CacheLoc where
    show (Cache Repo Writable a) = "thisrepo:" ++ a
    show (Cache Repo NotWritable a) = "repo:" ++ a
    show (Cache Directory Writable a) = "cache:" ++ a
    show (Cache Directory NotWritable a) = "readonly:" ++ a

instance Show Cache where
    show (Ca cs) = unlines $ map show cs

unionCaches :: Cache -> Cache -> Cache
unionCaches (Ca a) (Ca b) = Ca (nub (a ++ b))

-- | unionRemoteCaches merges caches. It tries to do better than just blindly
--   copying remote cache entries:
--
--   * If remote repository is accessed through network, do not copy any cache
--     entries from it. Taking local entries does not make sense and using
--     network entries can lead to darcs hang when it tries to get to
--     unaccessible host.
--
--   * If remote repositoty is local, copy all network cache entries. For local
--     cache entries if the cache directory exists and is writable it is added
--     as writable cache, if it exists but is not writable it is added as
--     read-only cache.
--
--   This approach should save us from bogus cache entries. One case it does
--   not work very well is when you fetch from partial repository over network.
--   Hopefully this is not a common case.
unionRemoteCaches :: Cache -> Cache -> String -> IO Cache
unionRemoteCaches local (Ca remote) repourl
    | isValidLocalPath repourl =  do
        f <- filtered
        return $ local `unionCaches` Ca f
    | otherwise = return local
  where
    filtered = catMaybes `fmap`
        mapM (\x -> mbGetRemoteCacheLoc x `catchall` return Nothing) remote
    mbGetRemoteCacheLoc :: CacheLoc -> IO (Maybe CacheLoc)
    mbGetRemoteCacheLoc (Cache Repo Writable _) = return Nothing
    mbGetRemoteCacheLoc c@(Cache t _ url)
        | isValidLocalPath url = do
            ex <- doesDirectoryExist url
            if ex
                then do
                    p <- getPermissions url
                    return $ Just $ if writable c && SD.writable p
                                        then c
                                        else Cache t NotWritable url
                else return Nothing
        | otherwise = return $ Just c

-- | Compares two caches, a remote cache is greater than a local one.
-- The order of the comparison is given by: local < http < ssh
compareByLocality :: CacheLoc -> CacheLoc -> Ordering
compareByLocality (Cache _ w x) (Cache _ z y)
    | isValidLocalPath x && isRemote y  = LT
    | isRemote x && isValidLocalPath y = GT
    | isHttpUrl x && isSshUrl y = LT
    | isSshUrl x && isHttpUrl y = GT
    | isValidLocalPath x && isWritable w
        && isValidLocalPath y && isNotWritable z = LT
    | otherwise = EQ
  where
    isRemote r = isHttpUrl r || isSshUrl r
    isWritable = (==) Writable
    isNotWritable = (==) NotWritable

repo2cache :: String -> Cache
repo2cache r = Ca [Cache Repo NotWritable r]

-- | 'cacheHash' computes the cache hash (i.e. filename) of a packed string.
cacheHash :: B.ByteString -> String
cacheHash ps = if sizeStrLen > 10
                   then shaOfPs
                   else replicate (10 - sizeStrLen) '0' ++ sizeStr
                        ++ '-' : shaOfPs
  where
    sizeStr = show $ B.length ps
    sizeStrLen = length sizeStr
    shaOfPs = sha256sum ps

okayHash :: String -> Bool
okayHash s = length s `elem` [40, 64, 75]

takeHash :: B.ByteString -> Maybe (String, B.ByteString)
takeHash ps = do
    h <- listToMaybe $ linesPS ps
    let v = BC.unpack h
    guard $ okayHash v
    return (v, B.drop (B.length h) ps)

checkHash :: String -> B.ByteString -> Bool
checkHash h s
    | length h == 40 = (show $ sha1PS s) == h
    | length h == 64 = sha256sum s == h
    | length h == 75 =
        B.length s == read (take 10 h) && sha256sum s == drop 11 h
    | otherwise = False

-- |@fetchFileUsingCache cache dir hash@ receives a list of caches @cache@, the
-- directory for which that file belongs @dir@ and the @hash@ of the file to
-- fetch.  It tries to fetch the file from one of the sources, trying them in
-- order one by one.  If the file cannot be fetched from any of the sources,
-- this operation fails.
fetchFileUsingCache :: Cache -> HashedDir -> String
                    -> IO (String, B.ByteString)
fetchFileUsingCache = fetchFileUsingCachePrivate Anywhere

writable :: CacheLoc -> Bool
writable (Cache _ NotWritable _) = False
writable (Cache _ Writable _) = True

isThisRepo :: CacheLoc -> Bool
isThisRepo (Cache Repo Writable _) = True
isThisRepo _ = False

bucketFolder :: String -> String
bucketFolder f = take 2 (cleanHash f)
    where
        cleanHash fileName = case dropWhile (/= '-') fileName of
            []  -> fileName
            s   -> drop 1 s

-- | @hashedFilePath cachelocation subdir hash@ returns the physical filename
-- of hash @hash@ in the @subdir@ section of @cachelocation@.
hashedFilePath :: CacheLoc -> HashedDir -> String -> String
hashedFilePath (Cache Directory _ d) s f =
    joinPath [d, hashedDir s, bucketFolder f, f]
hashedFilePath (Cache Repo _ r) s f =
    joinPath [r, darcsdir, hashedDir s, f]

-- | @hashedFilePathReadOnly cachelocation subdir hash@ returns the physical filename
-- of hash @hash@ in the @subdir@ section of @cachelocation@.
-- If directory, assume it is non-bucketed cache (old cache location).
hashedFilePathReadOnly :: CacheLoc -> HashedDir -> String -> String
hashedFilePathReadOnly (Cache Directory _ d) s f =
    d ++ "/" ++ hashedDir s ++ "/" ++ f
hashedFilePathReadOnly (Cache Repo _ r) s f =
    r ++ "/" ++ darcsdir ++ "/" ++ hashedDir s ++ "/" ++ f

-- | @peekInCache cache subdir hash@ tells whether @cache@ and contains an
-- object with hash @hash@ in a writable position.  Florent: why do we want it
-- to be in a writable position?
peekInCache :: Cache -> HashedDir -> String -> IO Bool
peekInCache (Ca cache) subdir f = cacheHasIt cache `catchall` return False
  where
    cacheHasIt [] = return False
    cacheHasIt (c : cs)
        | not $ writable c = cacheHasIt cs
        | otherwise = do
            ex <- doesFileExist $ hashedFilePath c subdir f
            if ex then return True else cacheHasIt cs

-- | @speculateFileUsingCache cache subdirectory name@ takes note that the file
-- @name@ is likely to be useful soon: pipelined downloads will add it to the
-- (low-priority) queue, for the rest it is a noop.
speculateFileUsingCache :: Cache -> HashedDir -> String -> IO ()
speculateFileUsingCache c sd h = do
    debugMessage $ "Speculating on " ++ h
    copyFileUsingCache OnlySpeculate c sd h

-- | Note that the files are likely to be useful soon: pipelined downloads will
-- add them to the (low-priority) queue, for the rest it is a noop.
speculateFilesUsingCache :: Cache -> HashedDir -> [String] -> IO ()
speculateFilesUsingCache _ _ [] = return ()
speculateFilesUsingCache cache sd hs = do
    debugMessage $ "Thinking about speculating on " ++ unwords hs
    hs' <- filterM (fmap not . peekInCache cache sd) hs
    unless (null hs') $ do
        debugMessage $ "Speculating on " ++ unwords hs'
        copyFilesUsingCache OnlySpeculate cache sd hs'

data OrOnlySpeculate = ActuallyCopy
                     | OnlySpeculate
                     deriving ( Eq )

-- | We hace a list of locations (@cache@) ordered from "closest/fastest"
-- (typically, the destination repo) to "farthest/slowest" (typically,
-- the source repo).
-- @copyFileUsingCache@ first checks whether given file @f@ is present
-- in some writeable location, if yes, do nothing. If no, it copies it
-- to the last writeable location, which would be the global cache
-- by default, or the destination repo if `--no-cache` is passed.
-- Function does nothing if there is no writeable location at all.
-- If the copy should occur between two locations of the same filesystem,
-- a hard link is actually made.
-- TODO document @oos@: what happens when we only speculate?
copyFileUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> String -> IO ()
copyFileUsingCache oos (Ca cache) subdir f = do
    debugMessage $
        "I'm doing copyFileUsingCache on " ++ hashedDir subdir ++ "/" ++ f
    Just stickItHere <- cacheLoc cache
    createDirectoryIfMissing True
        (reverse $ dropWhile (/= '/') $ reverse stickItHere)
    debugMessage $ "Will effectively do copyFileUsingCache to: " ++ show stickItHere
    filterBadSources cache >>= sfuc stickItHere
    `catchall`
    return ()
  where
    -- return last writeable cache/repo location for file.
    -- usually returns the global cache unless `--no-cache` is passed.
    cacheLoc [] = return Nothing
    cacheLoc (c : cs)
        | not $ writable c = cacheLoc cs
        | otherwise = do
            let attemptPath = hashedFilePath c subdir f
            ex <- doesFileExist attemptPath
            if ex
                then fail $ "File already present in writable location."
                else do
                    othercache <- cacheLoc cs
                    return $ othercache `mplus` Just attemptPath
    -- do the actual copy, or hard link, or put file in download queue
    sfuc _ [] = return ()
    sfuc out (c : cs)
        | not (writable c) =
            let cacheFile = hashedFilePathReadOnly c subdir f in
            if oos == OnlySpeculate
                then speculateFileOrUrl cacheFile out
                     `catchNonSignal`
                     \e -> checkCacheReachability (show e) c
                else do debugMessage $ "Copying from " ++ show cacheFile ++ " to  " ++ show out
                        copyFileOrUrl DefaultRemoteDarcs cacheFile out Cachable
                     `catchNonSignal`
                     (\e -> do checkCacheReachability (show e) c
                               sfuc out cs) -- try another read-only location
        | otherwise = sfuc out cs

copyFilesUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> [String]
                    -> IO ()
copyFilesUsingCache oos cache subdir hs =
    forM_ hs $ copyFileUsingCache oos cache subdir

data FromWhere = LocalOnly
               | Anywhere
               deriving ( Eq )

-- | Checks if a given cache entry is reachable or not.  It receives an error
-- caught during execution and the cache entry.  If the caches is not reachable
-- it is blacklisted and not longer tried for the rest of the session. If it is
-- reachable it is whitelisted and future errors with such cache get ignore.
-- To determine reachability:
--  * For a local cache, if the given source doesn't exist anymore, it is
--    blacklisted.
--  * For remote sources if the error is timeout, it is blacklisted, if not,
--    it checks if _darcs/hashed_inventory  exist, if it does, the entry is
--    whitelisted, if it doesn't, it is blacklisted.
checkCacheReachability :: String -> CacheLoc -> IO ()
checkCacheReachability e cache
    | isValidLocalPath source = doUnreachableCheck $
        checkFileReachability (doesDirectoryExist source)
    | isHttpUrl source =
        doUnreachableCheck $ do
            let err = case dropWhile (/= '(') e of
                          (_ : xs) -> fst (break (==')') xs)
                          _ -> e
            case reads err :: [(Download.ConnectionError, String)] of
                [(_, _)] -> addBadSource source
                _ -> checkFileReachability
                    (checkHashedInventoryReachability cache)
    | isSshUrl source = doUnreachableCheck $
        checkFileReachability (checkHashedInventoryReachability cache)
    | otherwise = fail $ "unknown transport protocol for: " ++ source
  where
    source = cacheSource cache

    doUnreachableCheck unreachableAction = do
        reachable <- isReachableSource
        unless (reachable source) unreachableAction

    checkFileReachability doCheck = do
        reachable <- doCheck
        if reachable
            then addReachableSource source
            else addBadSource source

-- | Returns a list of reachables cache entries, removing blacklisted entries.
filterBadSources :: [CacheLoc] -> IO [CacheLoc]
filterBadSources cache = do
    badSource <- isBadSource
    return $ filter (not . badSource . cacheSource) cache

-- | Checks if the _darcs/hashed_inventory exist and is reachable
checkHashedInventoryReachability :: CacheLoc -> IO Bool
checkHashedInventoryReachability cache = withTemp $ \tempout -> do
    let f = cacheSource cache </> darcsdir </> "hashed_inventory"
    copyFileOrUrl DefaultRemoteDarcs f tempout Cachable
    return True
    `catchNonSignal` const (return False)

-- | Get contents of some hashed file taking advantage of the cache system.
-- We hace a list of locations (@cache@) ordered from "closest/fastest"
-- (typically, the destination repo) to "farthest/slowest" (typically,
-- the source repo).
-- First, if possible it copies the file from remote location to local.
-- Then, it reads it contents, and links the file across all writeable
-- locations including the destination repository.
fetchFileUsingCachePrivate :: FromWhere -> Cache -> HashedDir -> String
                           -> IO (String, B.ByteString)
fetchFileUsingCachePrivate fromWhere (Ca cache) subdir f = do
    when (fromWhere == Anywhere) $
        copyFileUsingCache ActuallyCopy (Ca cache) subdir f
    filterBadSources cache >>= ffuc
    `catchall` debugFail ("Couldn't fetch `" ++ f ++ "'\nin subdir "
                          ++ hashedDir subdir ++ " from sources:\n\n"
                          ++ show (Ca cache))
  where
    ffuc (c : cs)
        | not (writable c) &&
            (Anywhere == fromWhere || isValidLocalPath (hashedFilePathReadOnly c subdir f)) = do
            let cacheFile = hashedFilePathReadOnly c subdir f
            -- looks like `copyFileUsingCache` could not copy the file we wanted.
            -- this can happen if `--no-cache` is NOT passed and the global cache is not accessible
            debugMessage $ "In fetchFileUsingCachePrivate I'm directly grabbing file contents from "
                           ++ cacheFile
            x <- gzFetchFilePS cacheFile Cachable
            if not $ checkHash f x
                then do
                    x' <- fetchFilePS cacheFile Cachable
                    unless (checkHash f x') $ do
                        hPutStrLn stderr $ "Hash failure in " ++ cacheFile
                        fail $ "Hash failure in " ++ cacheFile
                    return (cacheFile, x')
                else return (cacheFile, x) -- FIXME: create links in caches
            `catchNonSignal` \e -> do
                -- something bad happened, check if cache became unaccessible and try other ones
                checkCacheReachability (show e) c
                filterBadSources cs >>= ffuc
        | writable c = let cacheFile = hashedFilePath c subdir f in do
            debugMessage $ "About to gzFetchFilePS from " ++ show cacheFile
            x1 <- gzFetchFilePS cacheFile Cachable
            debugMessage $ "gzFetchFilePS done."
            x <- if not $ checkHash f x1
                     then do
                        x2 <- fetchFilePS cacheFile Cachable
                        unless (checkHash f x2) $ do
                            hPutStrLn stderr $ "Hash failure in " ++ cacheFile
                            removeFile cacheFile
                            fail $ "Hash failure in " ++ cacheFile
                        return x2
                     else return x1
            mapM_ (tryLinking cacheFile) cs
            return (cacheFile, x)
            `catchNonSignal` \e -> do
                debugMessage "Caught exception, now attempt creating cache."
                createCache c subdir `catchall` return ()
                checkCacheReachability (show e) c
                (fname, x) <- filterBadSources cs >>= ffuc  -- fetch file from remaining locations
                debugMessage $ "Attempt creating link from: " ++ show fname ++ " to " ++ show cacheFile
                (createLink fname cacheFile >> (debugMessage "successfully created link")
                                            >> return (cacheFile, x))
                  `catchall` do
                    debugMessage $ "Attempt writing file: " ++ show cacheFile
                    -- the following block is usually when files get actually written
                    -- inside of _darcs or global cache.
                    do createDirectoryIfMissing True (dropFileName cacheFile)
                       gzWriteFilePS cacheFile x
                       debugMessage $ "successfully wrote file"
                       `catchall` return ()
                    -- above block can fail if cache is not writeable
                    return (fname, x)
        | otherwise = ffuc cs

    ffuc [] = debugFail $ "No sources from which to fetch file `" ++ f
                          ++ "'\n"++ show (Ca cache)

    tryLinking ff c@(Cache Directory Writable d) = do
        createDirectoryIfMissing False (d ++ "/" ++ hashedDir subdir)
        createLink ff (hashedFilePath c subdir f)
        `catchall`
        return ()
    tryLinking _ _ = return ()

createCache :: CacheLoc -> HashedDir -> IO ()
createCache (Cache Directory _ d) subdir =
    createDirectoryIfMissing True (d ++ "/" ++ hashedDir subdir)
createCache _ _ = return ()

-- | @write compression filename content@ writes @content@ to the file
-- @filename@ according to the policy given by @compression@.
write :: Compression -> String -> B.ByteString -> IO ()
write NoCompression = writeAtomicFilePS
write GzipCompression = gzWriteAtomicFilePS

-- | @writeFileUsingCache cache compression subdir contents@ write the string
-- @contents@ to the directory subdir, except if it is already in the cache, in
-- which case it is a noop.  Warning (?) this means that in case of a hash
-- collision, writing using writeFileUsingCache is a noop. The returned value
-- is the filename that was given to the string.
writeFileUsingCache :: Cache -> Compression -> HashedDir -> B.ByteString
                    -> IO String
writeFileUsingCache (Ca cache) compr subdir ps = do
    _ <- fetchFileUsingCachePrivate LocalOnly (Ca cache) subdir hash
    return hash
    `catchall`
    wfuc cache
    `catchall`
    debugFail ("Couldn't write `" ++ hash ++ "'\nin subdir "
               ++ hashedDir subdir ++ " to sources:\n\n"++ show (Ca cache))
  where
    hash = cacheHash ps
    wfuc (c : cs)
        | not $ writable c = wfuc cs
        | otherwise = do
            createCache c subdir
            -- FIXME: create links in caches
            write compr (hashedFilePath c subdir hash) ps
            return hash
    wfuc [] = debugFail $ "No location to write file `" ++ hashedDir subdir
                          ++ "/" ++ hash ++ "'"

cleanCaches :: Cache -> HashedDir -> IO ()
cleanCaches c d = cleanCachesWithHint' c d Nothing

cleanCachesWithHint :: Cache -> HashedDir -> [String] -> IO ()
cleanCachesWithHint c d h = cleanCachesWithHint' c d (Just h)

cleanCachesWithHint' :: Cache -> HashedDir -> Maybe [String] -> IO ()
cleanCachesWithHint' (Ca cs) subdir hint = mapM_ cleanCache cs
  where
    cleanCache (Cache Directory Writable d) =
        withCurrentDirectory (d ++ "/" ++ hashedDir subdir) (do
            fs' <- getDirectoryContents "."
            let fs = filter okayHash $ fromMaybe fs' hint
                cleanMsg = "Cleaning cache " ++ d ++ "/" ++ hashedDir subdir
            mapM_ clean $ progressList cleanMsg fs)
        `catchall`
        return ()
    cleanCache _ = return ()
    clean f = do
        lc <- linkCount `liftM` getSymbolicLinkStatus f
        when (lc < 2) $ removeFile f
        `catchall`
        return ()

-- | Prints an error message with a list of bad caches.
reportBadSources :: IO ()
reportBadSources = do
    sources <- getBadSourcesList
    let size = length sources
    unless (null sources) $ hPutStrLn stderr $
        concat [ "\nHINT: I could not reach the following "
               , englishNum size (Noun "repository") ":"
               , "\n"
               , intercalate "\n" (map ("        " ++) sources)
               , "\n      If you're not using "
               , englishNum size It ", you should probably delete"
               , "\n      the corresponding "
               , englishNum size (Noun "entry") " from _darcs/prefs/sources."
               ]
