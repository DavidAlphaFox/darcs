--  Copyright (C) 2003-2005 David Roundy
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

{-# LANGUAGE CPP, OverloadedStrings #-}

module Darcs.UI.Commands.Optimize ( optimize, doOptimizeHTTP ) where

import Prelude hiding ( (^) )
import Control.Applicative ( (<$>) )
import Control.Exception ( finally )
import Control.Monad ( when, unless, forM_ )
import Data.Maybe ( isJust, fromJust )
import Data.List ( sort )
import Data.Set ( difference )
import System.Directory
    ( getDirectoryContents
    , doesDirectoryExist
    , doesFileExist
    , renameFile
    , getModificationTime
    , createDirectoryIfMissing
    , removeFile
    , getHomeDirectory
    )
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL


import Darcs.Patch.PatchInfoAnd ( extractHash )
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, amInRepository, putInfo
                         , normalCommand, withStdOpts )
import Darcs.Repository.Prefs ( getPreflist, getCaches, globalCacheDir, oldGlobalCacheDir )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , readRepo
    , reorderInventory
    , cleanRepository
    , replacePristine
    )
import Darcs.Repository.HashedRepo ( inventoriesDir, patchesDir, pristineDir,
                                    hashedInventory, filterDirContents,
                                    readHashedPristineRoot, listInventoriesRepoDir,
                                    listPatchesLocalBucketed, set, unset, inv2pris )
import Darcs.Repository.HashedIO ( getHashedFiles )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.InternalTypes ( Repository(..) )
import Darcs.Repository.Util ( getRecursiveDarcsRepos )
import Darcs.Patch.Witnesses.Ordered
     ( mapFL
     , bunchFL
     , lengthRL
     )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set
    ( newset2RL
    , newset2FL
    , progressPatchSet
    )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Printer ( text )
import Darcs.Repository.Lock
    ( maybeRelink
    , gzWriteAtomicFilePS
    , writeAtomicFilePS
    , rmRecursive
    , removeFileMayNotExist
    )
import Darcs.Util.File ( withCurrentDirectory, getRecursiveContents )
import Darcs.UI.External ( catchall )
import Darcs.Util.Progress
    ( beginTedious
    , endTedious
    , tediousSize
    , debugMessage
    )
import Darcs.Util.Global ( darcsdir )

import System.FilePath.Posix
    ( takeExtension
    , (</>)
    , (<.>)
    , takeFileName
    , joinPath
    )
import Text.Printf ( printf )
import System.Posix.Files ( getFileStatus, isDirectory )
import Darcs.UI.Flags
    (  DarcsFlag(Compress)
    , compression, verbosity, useCache, umask )
import Darcs.UI.Options ( DarcsOption, (^), oid, odesc, ocheck, onormalise
                        , defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags
    ( UpdateWorking (..), DryRun ( NoDryRun ), UseCache (..), UMask (..)
    , WithWorkingDir(WithWorkingDir) )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Repository.Cache ( hashedDir, bucketFolder,
                                HashedDir(HashedPristineDir) )
import Darcs.Repository.Format
    ( identifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , formatHas
    , RepoProperty ( HashedInventory )
    )
import Darcs.Repository.PatchIndex
import qualified Darcs.Repository.HashedRepo as HashedRepo
import Darcs.Repository.State ( readRecorded )

import Storage.Hashed.Tree
    ( Tree
    , TreeItem(..)
    , list
    , expand
    , emptyTree
    )
import Darcs.Util.Path( anchorPath, toFilePath, AbsolutePath )
import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Darcs
    ( writeDarcsHashed
    , decodeDarcsSize
    )
import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath )
import Codec.Compression.GZip ( compress )


optimizeDescription :: String
optimizeDescription = "Optimize the repository."

optimizeHelp :: String
optimizeHelp =
 "The `darcs optimize` command modifies the current repository in an\n" ++
 "attempt to reduce its resource requirements."

optimize :: DarcsCommand [DarcsFlag]
optimize = SuperCommand {
      commandProgramName = "darcs"
    , commandName = "optimize"
    , commandHelp = optimizeHelp
    , commandDescription = optimizeDescription
    , commandPrereq = amInRepository
    , commandSubCommands = [  normalCommand optimizeClean,
                              normalCommand optimizeHttp,
                              normalCommand optimizeReorder,
                              normalCommand optimizeEnablePatchIndex,
                              normalCommand optimizeDisablePatchIndex,
                              normalCommand optimizeCompress,
                              normalCommand optimizeUncompress,
                              normalCommand optimizeRelink,
                              normalCommand optimizePristine,
                              normalCommand optimizeUpgrade,
                              normalCommand optimizeGlobalCache
                           ]
    }

commonBasicOpts :: DarcsOption a (Maybe String -> UMask -> a)
commonBasicOpts = O.workingRepoDir ^ O.umask
commonAdvancedOpts :: DarcsOption a a
commonAdvancedOpts = oid
commonOpts :: DarcsOption a
              (Maybe String
               -> UMask
               -> Maybe O.StdCmdAction
               -> Bool
               -> Bool
               -> O.Verbosity
               -> Bool
               -> UseCache
               -> Maybe String
               -> Bool
               -> Maybe String
               -> Bool
               -> a)
commonOpts = commonBasicOpts `withStdOpts` commonAdvancedOpts

common :: DarcsCommand [DarcsFlag]
common = DarcsCommand
    { commandProgramName = "darcs"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandPrereq =  amInRepository
    , commandArgdefaults = nodefaults
    , commandName = undefined
    , commandHelp = undefined
    , commandDescription = undefined
    , commandCommand =  undefined
    , commandGetArgPossibilities = undefined
    , commandAdvancedOptions = odesc commonAdvancedOpts
    , commandBasicOptions = odesc commonBasicOpts
    , commandDefaults = defaultFlags commonOpts
    , commandCheckOptions = ocheck commonOpts
    , commandParseOptions = onormalise commonOpts
    }


optimizeClean :: DarcsCommand [DarcsFlag]
optimizeClean = common
    { commandName = "clean"
    , commandHelp = "This command deletes obsolete files within the repository."
    , commandDescription = "garbage collect pristine, inventories and patches"
    , commandCommand = optimizeCleanCmd
    }

optimizeCleanCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCleanCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      putInfo opts "Done cleaning repository!"

optimizeUpgrade :: DarcsCommand [DarcsFlag]
optimizeUpgrade = common
    { commandName = "upgrade"
    , commandHelp = "Convert old-fashioned repositories to the current default hashed format."
    , commandDescription = "upgrade repository to latest compatible format"
    , commandCommand = optimizeUpgradeCmd
    }

optimizeHttp :: DarcsCommand [DarcsFlag]
optimizeHttp = common
    { commandName = "http"
    , commandHelp = optimizeHelpHttp
    , commandDescription = "optimize repository for getting over network"
    , commandCommand = optimizeHttpCmd
    }

optimizeHttpCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeHttpCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      doOptimizeHTTP repository
      putInfo opts "Done creating packs!"

optimizePristine :: DarcsCommand [DarcsFlag]
optimizePristine = common
    { commandName = "pristine"
    , commandHelp = "This command updates the format of `_darcs/pristine.hashed/`, which was different\n"
                 ++ "before darcs 2.3.1."
    , commandDescription = "optimize hashed pristine layout"
    , commandCommand = optimizePristineCmd
    }

optimizePristineCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizePristineCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      doOptimizePristine opts repository
      putInfo opts "Done optimizing pristine!"

optimizeCompress :: DarcsCommand [DarcsFlag]
optimizeCompress = common
    { commandName = "compress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "compress patches and inventories"
    , commandCommand = optimizeCompressCmd
    }

optimizeUncompress :: DarcsCommand [DarcsFlag]
optimizeUncompress = common
    { commandName = "uncompress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "uncompress patches and inventories"
    , commandCommand = optimizeUncompressCmd
    }

optimizeCompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCompressCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      optimizeCompression [Compress]
      putInfo opts "Done optimizing by compression!"

optimizeUncompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUncompressCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      optimizeCompression []
      putInfo opts "Done optimizing by uncompression!"

optimizeCompression :: [DarcsFlag] -> IO ()
optimizeCompression opts = do
    putInfo opts "Optimizing (un)compression of patches..."
    do_compress (darcsdir++"/patches")
    putInfo opts "Optimizing (un)compression of inventories..."
    do_compress (darcsdir++"/inventories")
    where do_compress f =
              do isd <- doesDirectoryExist f
                 if isd then withCurrentDirectory f $
                             do fs <- filter notdot `fmap` getDirectoryContents "."
                                mapM_ do_compress fs
                        else if Compress `elem` opts
                             then gzReadFilePS f >>= gzWriteAtomicFilePS f
                             else gzReadFilePS f >>= writeAtomicFilePS f
          notdot ('.':_) = False
          notdot _ = True

optimizeEnablePatchIndex :: DarcsCommand [DarcsFlag]
optimizeEnablePatchIndex = common
    { commandName = "enable-patch-index"
    , commandHelp = "Build the patch index, an internal data structure that accelerates\n"
                 ++ "commands that need to know what patches touch a given file. Such as\n"
                 ++ "annotate and log."
    , commandDescription = "Enable patch index"
    , commandCommand = optimizeEnablePatchIndexCmd
    }

optimizeDisablePatchIndex :: DarcsCommand [DarcsFlag]
optimizeDisablePatchIndex = common
    { commandName = "disable-patch-index"
    , commandHelp = "Delete and stop maintaining the patch index from the repository."
    , commandDescription = "Disable patch index"
    , commandCommand = optimizeDisablePatchIndexCmd
    }

optimizeEnablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeEnablePatchIndexCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      createOrUpdatePatchIndexDisk repository
      putInfo opts "Done enabling patch index!"

optimizeDisablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeDisablePatchIndexCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \(Repo repodir _ _ _) -> do
      deletePatchIndex repodir
      putInfo opts "Done disabling patch index!"

optimizeReorder :: DarcsCommand [DarcsFlag]
optimizeReorder = common
    { commandName = "reorder"
    , commandHelp = "This command moves recent patches (those not included in\n" ++
                    "the latest tag) to the \"front\", reducing the amount that a typical\n" ++
                    "remote command needs to download.  It should also reduce the CPU time\n" ++
                    "needed for some operations."
    , commandDescription = "reorder the patches in the repository"
    , commandCommand = optimizeReorderCmd
    }

optimizeReorderCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeReorderCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      reorderInventory repository (compression opts) YesUpdateWorking (verbosity opts)
      putInfo opts "Done reordering!"

optimizeRelinkBasicOpts :: DarcsOption a
                           (Maybe String -> UMask -> [AbsolutePath] -> a)
optimizeRelinkBasicOpts = commonBasicOpts ^ O.siblings
optimizeRelinkOpts :: DarcsOption a
                      (Maybe String
                       -> UMask
                       -> [AbsolutePath]
                       -> Maybe O.StdCmdAction
                       -> Bool
                       -> Bool
                       -> O.Verbosity
                       -> Bool
                       -> UseCache
                       -> Maybe String
                       -> Bool
                       -> Maybe String
                       -> Bool
                       -> a)
optimizeRelinkOpts = optimizeRelinkBasicOpts `withStdOpts` commonAdvancedOpts

optimizeRelink :: DarcsCommand [DarcsFlag]
optimizeRelink = common
    { commandName = "relink"
    , commandHelp = optimizeHelpRelink 
    , commandDescription = "relink random internal data to a sibling"
    , commandCommand = optimizeRelinkCmd
    , commandAdvancedOptions = odesc commonAdvancedOpts
    , commandBasicOptions = odesc optimizeRelinkBasicOpts
    , commandDefaults = defaultFlags optimizeRelinkOpts
    , commandCheckOptions = ocheck optimizeRelinkOpts
    , commandParseOptions = onormalise optimizeRelinkOpts
    }

optimizeRelinkCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeRelinkCmd _ opts _ = do
    withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      doRelink opts
      putInfo opts "Done relinking!"

optimizeHelpHttp :: String
optimizeHelpHttp = unlines
  [ "Using this option creates 'repository packs' that could dramatically"
  , "speed up performance when a user does a `darcs clone` of the repository"
  , "over HTTP. To make use of packs, the clients must have a darcs of at"
  , "least version 2.10."
  ]

optimizeHelpCompression :: String
optimizeHelpCompression =
 "By default patches are compressed with zlib (RFC 1951) to reduce\n" ++
 "storage (and download) size.  In exceptional circumstances, it may be\n" ++
 "preferable to avoid compression.  In this case the `--dont-compress`\n" ++
 "option can be used (e.g. with `darcs record`) to avoid compression.\n" ++
 "\n" ++
 "The `darcs optimize uncompress` and `darcs optimize compress`\n" ++
 "commands can be used to ensure existing patches in the current\n" ++
 "repository are respectively uncompressed or compressed."

optimizeHelpRelink :: String
optimizeHelpRelink =
 "The `darcs optimize relink` command hard-links patches that the\n" ++
 "current repository has in common with its peers.  Peers are those\n" ++
 "repositories listed in `_darcs/prefs/sources`, or defined with the\n" ++
 "`--sibling` option (which can be used multiple times).\n" ++
 "\n" ++
 "Darcs uses hard-links automatically, so this command is rarely needed.\n" ++
 "It is most useful if you used `cp -r` instead of `darcs clone` to copy a\n" ++
 "repository, or if you pulled the same patch from a remote repository\n" ++
 "into multiple local repositories."

doOptimizePristine :: (RepoPatch p, ApplyState p ~ Tree) => [DarcsFlag] -> Repository p wR wU wT -> IO ()
doOptimizePristine opts repo = do
  hashed <- doesFileExist $ darcsdir </> "hashed_inventory"
  when hashed $ do
    inv <- BS.readFile (darcsdir </> "hashed_inventory")
    let linesInv = BS.split '\n' inv
    case linesInv of
      [] -> return ()
      (pris_line:_) ->
          let size = decodeDarcsSize $ BS.drop 9 pris_line
           in when (isJust size) $ do putInfo opts "Optimizing hashed pristine..."
                                      readRecorded repo >>= replacePristine repo
                                      cleanRepository repo

doRelink :: [DarcsFlag] -> IO ()
doRelink opts =
    do let some_siblings = parseFlags O.siblings opts
       defrepolist <- getPreflist "defaultrepo"
       let siblings = map toFilePath some_siblings ++ defrepolist
       if null siblings
          then putInfo opts "No siblings -- no relinking done."
          else do debugMessage "Relinking patches..."
                  patch_tree <- expand =<< readPlainTree (darcsdir </> "patches")
                  let patches = [ anchorPath "" p | (p, File _) <- list patch_tree ]
                  maybeRelinkFiles siblings patches $ darcsdir </> "patches"
                  debugMessage "Done relinking."

maybeRelinkFiles :: [String] -> [String] -> String -> IO ()
maybeRelinkFiles src dst dir =
    mapM_ (maybeRelinkFile src . ((dir ++ "/") ++)) dst

maybeRelinkFile :: [String] -> String -> IO ()
maybeRelinkFile [] _ = return ()
maybeRelinkFile (h:t) f =
    do done <- maybeRelink (h ++ "/" ++ f) f
       unless done $
           maybeRelinkFile t f

optimizeUpgradeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUpgradeCmd _ opts _ = do
  debugMessage "Upgrading to hashed..."
  rf <- identifyRepoFormat "."
  debugMessage "Found our format"
  if formatHas HashedInventory rf
     then putInfo opts "No action taken because this repository already is hashed."
     else do putInfo opts "Checking repository in case of corruption..."
             withRepoLock NoDryRun YesUseCache YesUpdateWorking NoUMask $ RepoJob $ \repository ->
               actuallyUpgradeFormat repository

actuallyUpgradeFormat :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
actuallyUpgradeFormat repository = do
  -- convert patches/inventory
  patches <- readRepo repository
  let k = "Hashing patch"
  beginTedious k
  tediousSize k (lengthRL $ newset2RL patches)
  let patches' = progressPatchSet k patches
  cache <- getCaches YesUseCache "."
  let compr = compression [] -- default compression
  HashedRepo.writeTentativeInventory cache compr patches'
  endTedious k
  -- convert pristine by applying patches
  -- the faster alternative would be to copy pristine, but the apply method is more reliable
  let patchesToApply = progressFL "Applying patch" $ newset2FL patches'
  createDirectoryIfMissing False $ darcsdir </> hashedDir HashedPristineDir
  -- We ignore the returned root hash, we don't use it.
  _ <- writeDarcsHashed emptyTree $ darcsdir </> "pristine.hashed"
  sequence_ $ mapFL HashedRepo.applyToTentativePristine $ bunchFL 100 patchesToApply
  -- now make it official
  HashedRepo.finalizeTentativeChanges repository compr
  writeRepoFormat (createRepoFormat True WithWorkingDir) (darcsdir </> "format")
  -- clean out old-fashioned junk
  debugMessage "Cleaning out old-fashioned repository files..."
  removeFile   $ darcsdir </> "inventory"
  removeFile   $ darcsdir </> "tentative_inventory"
  rmRecursive (darcsdir </> "pristine") `catchall` rmRecursive (darcsdir </> "current")
  rmGzsIn (darcsdir </> "patches")
  rmGzsIn (darcsdir </> "inventories")
  let checkpointDir = darcsdir </> "checkpoints"
  hasCheckPoints <- doesDirectoryExist checkpointDir
  when hasCheckPoints $ rmRecursive checkpointDir
 where
  rmGzsIn dir =
    withCurrentDirectory dir $ do
      gzs <- filter ((== ".gz") . takeExtension) `fmap` getDirectoryContents "."
      mapM_ removeFile gzs

doOptimizeHTTP :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wT -> IO ()
doOptimizeHTTP repo = flip finally (mapM_ removeFileIfExists
  [ darcsdir </> "meta-filelist-inventories"
  , darcsdir </> "meta-filelist-pristine"
  , basicTar <.> "part"
  , patchesTar <.> "part"
  ]) $ do
  rf <- identifyRepoFormat "."
  unless (formatHas HashedInventory rf) $ fail oldRepoFailMsg
  createDirectoryIfMissing False packsDir
  -- pristine hash
  Just hash <- readHashedPristineRoot repo
  writeFile ( packsDir </> "pristine" ) hash
  -- pack patchesTar
  ps <- mapFL hashedPatchFileName . newset2FL <$> readRepo repo
  is <- map ((darcsdir </> "inventories") </>) <$> HashedRepo.listInventories
  writeFile (darcsdir </> "meta-filelist-inventories") . unlines $
    map takeFileName is
  BL.writeFile (patchesTar <.> "part") . compress . write =<<
    mapM fileEntry' ((darcsdir </> "meta-filelist-inventories") : ps ++
    reverse is)
  renameFile (patchesTar <.> "part") patchesTar
  -- pack basicTar
  pr <- sortByMTime =<< dirContents "pristine.hashed"
  writeFile (darcsdir </> "meta-filelist-pristine") . unlines $
    map takeFileName pr
  BL.writeFile (basicTar <.> "part") . compress . write =<< mapM fileEntry' (
    [ darcsdir </> "meta-filelist-pristine"
    , darcsdir </> "hashed_inventory"
    ] ++ reverse pr)
  renameFile (basicTar <.> "part") basicTar
 where
  packsDir = darcsdir </> "packs"
  basicTar = packsDir </> "basic.tar.gz"
  patchesTar = packsDir </> "patches.tar.gz"
  fileEntry' x = unsafeInterleaveIO $ do
    content <- BL.fromChunks . return <$> gzReadFilePS x
    tp <- either fail return $ toTarPath False x
    return $ fileEntry tp content
  dirContents d = map ((darcsdir </> d) </>) <$>
    (filterDirContents d $ const True)
  hashedPatchFileName x = case extractHash x of
    Left _ -> fail "unexpected unhashed patch"
    Right h -> darcsdir </> "patches" </> h
  sortByMTime xs = map snd . sort <$> mapM (\x -> (\t -> (t, x)) <$>
    getModificationTime x) xs
  removeFileIfExists x = do
    ex <- doesFileExist x
    when ex $ removeFile x

optimizeBucketed :: [DarcsFlag] -> IO ()
optimizeBucketed opts = do
  putInfo opts "Migrating global cache to bucketed format."
  gOldCacheDir <- oldGlobalCacheDir
  gCacheDir <- globalCacheDir

  case gCacheDir of
    Nothing -> fail "New global cache doesn't exist."
    Just gCacheDir' -> do
      let gCachePristineDir = joinPath [gCacheDir', pristineDir]
          gCacheInventoriesDir = joinPath [gCacheDir', inventoriesDir]
          gCachePatchesDir = joinPath [gCacheDir', patchesDir]
      debugMessage "Making bucketed cache from new cache."
      toBucketed gCachePristineDir gCachePristineDir
      toBucketed gCacheInventoriesDir gCacheInventoriesDir
      toBucketed gCachePatchesDir gCachePatchesDir
      case gOldCacheDir of
        Nothing -> debugMessage "Old global cache doesn't exist."
        Just gOldCacheDir' -> do
          debugMessage "Making bucketed cache from old cache."
          toBucketed (joinPath [gOldCacheDir', pristineDir]) gCachePristineDir
          toBucketed (joinPath [gOldCacheDir', inventoriesDir]) gCacheInventoriesDir
          toBucketed (joinPath [gOldCacheDir', patchesDir]) gCachePatchesDir
      putInfo opts "Done making bucketed cache!"
  where
    toBucketed :: FilePath -> FilePath -> IO ()
    toBucketed src dest = do
      srcExist <- doesDirectoryExist src
      if srcExist
        then  do
                debugMessage $ "Making " ++ src ++ " bucketed in " ++ dest
                forM_ subDirSet $ \subDir ->
                  createDirectoryIfMissing True (dest </> subDir)
                fileNames <- getDirectoryContents src
                forM_ fileNames $ \file -> do
                  fileStatus <- getFileStatus (src </> file)
                  if not $ isDirectory fileStatus
                    then renameFile' src dest file
                    else return ()
        else do
          debugMessage $ show src ++ " didn't exist, doing nothing."
          return ()

    renameFile' :: FilePath -> FilePath -> FilePath -> IO ()
    renameFile' s d f = renameFile (s </> f) (joinPath [d, bucketFolder f, f])

    subDirSet :: [String]
    subDirSet = map toStrHex [0..255]

    toStrHex :: Int -> String
    toStrHex = printf "%02x"


optimizeGlobalCache :: DarcsCommand [DarcsFlag]
optimizeGlobalCache = common
    { commandName = "cache"
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = [ "<DIRECTORY> ..." ]
    , commandHelp = optimizeHelpGlobalCache
    , commandDescription = "garbage collect global cache"
    , commandCommand = optimizeGlobalCacheCmd
    , commandPrereq = \_ -> return $ Right ()
    }

optimizeHelpGlobalCache :: String
optimizeHelpGlobalCache = unlines
  [ "This command deletes obsolete files within the global cache."
  , "It takes one or more directories as arguments, and recursively"
  , "searches all repositories within these directories. Then it deletes"
  , "all files in the global cache not belonging to these repositories."
  , "When no directory is given, it searches repositories in the user's"
  , "home directory."
  , ""
  , "It also automatically migrates the global cache to the (default)"
  , "bucketed format."
  ]

optimizeGlobalCacheCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeGlobalCacheCmd _ opts args = do
  optimizeBucketed opts
  home <- getHomeDirectory
  let args' = if null args then [home] else args
  cleanGlobalCache args' opts
  putInfo opts "Done cleaning global cache!"

cleanGlobalCache :: [String] -> [DarcsFlag] -> IO ()
cleanGlobalCache dirs opts = do
  putInfo opts "\nLooking for repositories in the following directories:"
  putInfo opts $ text $ unlines dirs
  gCacheDir' <- globalCacheDir
  repoPaths'  <- mapM getRecursiveDarcsRepos dirs

  putInfo opts "Finished listing repositories."

  let repoPaths         = unset . set $ concat repoPaths'
      gCache            = fromJust gCacheDir'
      gCacheInvDir      = gCache </> inventoriesDir
      gCachePatchesDir  = gCache </> patchesDir
      gCachePristineDir = gCache </> pristineDir

  createDirectoryIfMissing True gCacheInvDir
  createDirectoryIfMissing True gCachePatchesDir
  createDirectoryIfMissing True gCachePristineDir

  remove listInventoriesRepoDir gCacheInvDir repoPaths
  remove ((listPatchesLocalBucketed gCache) . (</> darcsdir)) gCachePatchesDir repoPaths
  remove getPristine gCachePristineDir repoPaths

  where
  remove fGetFiles cacheSubDir repoPaths = do
    s1 <- mapM fGetFiles repoPaths
    s2 <- getRecursiveContents cacheSubDir
    remove' cacheSubDir s2 (concat s1)

  remove' :: String -> [String] -> [String] -> IO ()
  remove' dir s1 s2 = do
    mapM_ (removeFileMayNotExist . (\hashedFile ->
      dir </> bucketFolder hashedFile </> hashedFile))
      (unset $ (set s1) `difference` (set s2))

  getPristine :: String -> IO [String]
  getPristine darcsDir = do
    i <- gzReadFilePS (darcsDir </> darcsdir </> hashedInventory)
    priss <- getHashedFiles (darcsDir </> darcsdir </> pristineDir) [inv2pris i]
    return priss
