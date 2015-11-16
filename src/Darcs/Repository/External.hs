{-# LANGUAGE CPP #-}
module Darcs.Repository.External
    ( cloneTree
    , cloneFile
    , fetchFilePS
    , fetchFileLazyPS
    , gzFetchFilePS
    , speculateFileOrUrl
    , copyFileOrUrl
    , Cachable(..)
    , backupByRenaming
    , backupByCopying
    , environmentHelpProtocols
    ) where

import Prelude hiding ( catch )
import Control.Exception ( catch, IOException )

import System.Posix.Files
    ( getSymbolicLinkStatus
    , isRegularFile
    , isDirectory
    , createLink
    )
import System.Directory
    ( createDirectory
    , getDirectoryContents
    , doesDirectoryExist
    , doesFileExist
    , renameFile
    , renameDirectory
    , copyFile
    )

import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
import System.FilePath.Posix ( (</>), normalise )
import System.IO.Error ( isDoesNotExistError )
import Control.Monad
    ( unless
    , when
    , zipWithM_
    )

import Data.Char ( toUpper )

import Darcs.Util.Exec ( exec, Redirect(..) )
import Darcs.Util.Download
    ( copyUrl
    , copyUrlFirst
    , waitUrl
    , Cachable(..)
    )

import Darcs.Util.URL
    ( isValidLocalPath
    , isHttpUrl
    , isSshUrl
    , splitSshUrl
    )
import Darcs.Util.Text ( breakCommand )
import Darcs.Util.Exception ( catchall )
import Darcs.Repository.Flags ( RemoteDarcs(..) )
import Darcs.Repository.Lock ( withTemp )
import Darcs.Repository.Ssh ( copySSH )

import Darcs.Util.ByteString ( gzReadFilePS )
import qualified Data.ByteString as B (ByteString, readFile )
import qualified Data.ByteString.Lazy as BL

#ifdef HAVE_HTTP
import Network.Browser
    ( browse
    , request
    , setErrHandler
    , setOutHandler
    , setAllowRedirects
    )
import Network.HTTP
    ( RequestMethod(GET)
    , rspCode
    , rspBody
    , rspReason
    , mkRequest
    )
import Network.URI
    ( parseURI
    , uriScheme
    )
#endif

copyFileOrUrl :: RemoteDarcs -> FilePath -> FilePath -> Cachable -> IO ()
copyFileOrUrl _    fou out _     | isValidLocalPath fou = copyLocal fou out
copyFileOrUrl _    fou out cache | isHttpUrl  fou = copyRemote fou out cache
copyFileOrUrl rd   fou out _     | isSshUrl  fou = copySSH rd (splitSshUrl fou) out
copyFileOrUrl _    fou _   _     = fail $ "unknown transport protocol: " ++ fou

copyLocal  :: String -> FilePath -> IO ()
copyLocal fou out = createLink fou out `catchall` cloneFile fou out

copyRemote :: String -> FilePath -> Cachable -> IO ()
copyRemote u v cache =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Nothing -> copyRemoteNormal u v cache
         Just get ->
           do let (cmd,args) = breakCommand get
              r <- exec cmd (args++[u]) (Null, File v, AsIs)
              when (r /= ExitSuccess) $
                  fail $ "(" ++ get ++ ") failed to fetch: " ++ u

cloneTree :: FilePath -> FilePath -> IO ()
cloneTree = cloneTreeExcept []

cloneTreeExcept :: [FilePath] -> FilePath -> FilePath -> IO ()
cloneTreeExcept except source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` (".":"..":except)) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else fail ("cloneTreeExcept: Bad source " ++ source)
   `catch` \(_ :: IOException) -> fail ("cloneTreeExcept: Bad source " ++ source)

cloneSubTree :: FilePath -> FilePath -> IO ()
cloneSubTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        createDirectory dest
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` [".", ".."]) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else if isRegularFile fs then
        cloneFile source dest
     else fail ("cloneSubTree: Bad source "++ source)
    `catch` (\e -> unless (isDoesNotExistError e) $ ioError e)

cloneFile :: FilePath -> FilePath -> IO ()
cloneFile = copyFile

backupByRenaming :: FilePath -> IO ()
backupByRenaming = backupBy rename
 where rename x y = do
         isD <- doesDirectoryExist x
         if isD then renameDirectory x y else renameFile x y

backupByCopying :: FilePath -> IO ()
backupByCopying = backupBy copy
 where
  copy x y = do
    isD <- doesDirectoryExist x
    if isD then do createDirectory y
                   cloneTree (normalise x) (normalise y)
           else copyFile x y

backupBy :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
backupBy backup f =
           do hasBF <- doesFileExist f
              hasBD <- doesDirectoryExist f
              when (hasBF || hasBD) $ helper 0
  where
  helper :: Int -> IO ()
  helper i = do existsF <- doesFileExist next
                existsD <- doesDirectoryExist next
                if existsF || existsD
                   then helper (i + 1)
                   else do putStrLn $ "Backing up " ++ f ++ "(" ++ suffix ++ ")"
                           backup f next
             where next = f ++ suffix
                   suffix = ".~" ++ show i ++ "~"

copyAndReadFile :: (FilePath -> IO a) -> String -> Cachable -> IO a
copyAndReadFile readfn fou _ | isValidLocalPath fou = readfn fou
copyAndReadFile readfn fou cache = withTemp $ \t -> do
  copyFileOrUrl DefaultRemoteDarcs fou t cache
  readfn t

-- | @fetchFile fileOrUrl cache@ returns the content of its argument (either a
-- file or an URL). If it has to download an url, then it will use a cache as
-- required by its second argument.
--
-- We always use default remote darcs, since it is not fatal if the remote
-- darcs does not exist or is too old -- anything that supports transfer-mode
-- should do, and if not, we will fall back to SFTP or SCP.
fetchFilePS :: String -> Cachable -> IO B.ByteString
fetchFilePS = copyAndReadFile (B.readFile)

-- | @fetchFileLazyPS fileOrUrl cache@ lazily reads the content of its argument
-- (either a file or an URL). Warning: this function may constitute a fd leak;
-- make sure to force consumption of file contents to avoid that. See
-- "fetchFilePS" for details.
fetchFileLazyPS :: String -> Cachable -> IO BL.ByteString
#ifdef HAVE_HTTP
fetchFileLazyPS x c = case parseURI x of
  Just x' | uriScheme x' == "http:" -> do
    rsp <- fmap snd . browse $ do
      setErrHandler . const $ return ()
      setOutHandler . const $ return ()
      setAllowRedirects True
      request $ mkRequest GET x'
    if rspCode rsp /= (2, 0, 0)
      then fail $ "fetchFileLazyPS: " ++ rspReason rsp
      else return $ rspBody rsp
  _ -> copyAndReadFile BL.readFile x c
#else
fetchFileLazyPS = copyAndReadFile BL.readFile
#endif

gzFetchFilePS :: String -> Cachable -> IO B.ByteString
gzFetchFilePS = copyAndReadFile gzReadFilePS

maybeURLCmd :: String -> String -> IO (Maybe String)
maybeURLCmd what url =
  do let prot = map toUpper $ takeWhile (/= ':') url
     fmap Just (getEnv ("DARCS_" ++ what ++ "_" ++ prot))
             `catch` \(_ :: IOException) -> return Nothing

copyRemoteNormal :: String -> FilePath -> Cachable -> IO ()
copyRemoteNormal u v cache = copyUrlFirst u v cache >> waitUrl u

speculateFileOrUrl :: String -> FilePath -> IO ()
speculateFileOrUrl fou out | isHttpUrl fou = speculateRemote fou out
                           | otherwise = return ()

speculateRemote :: String -> FilePath -> IO () -- speculations are always Cachable
#if defined(HAVE_CURL) || defined(HAVE_HTTP)
speculateRemote u v =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Just _ -> return () -- can't pipeline these
         Nothing -> copyUrl u v Cachable
#else
speculateRemote u _ = const () `fmap` maybeURLCmd "GET" u
#endif

environmentHelpProtocols :: ([String], [String])
environmentHelpProtocols = (["DARCS_GET_FOO", "DARCS_APPLY_FOO"],[
    "When trying to access a repository with a URL beginning foo://,",
    "darcs will invoke the program specified by the DARCS_GET_FOO",
    "environment variable (if defined) to download each file, and the",
    "command specified by the DARCS_APPLY_FOO environment variable (if",
    "defined) when pushing to a foo:// URL.",
    "",
    "This method overrides all other ways of getting `foo://xxx` URLs.",
    "",
    "Note that each command should be constructed so that it sends the downloaded",
    "content to STDOUT, and the next argument to it should be the URL.",
    "Here are some examples that should work for DARCS_GET_HTTP:",
    "",
    "    fetch -q -o -",
    "    curl -s -f",
    "    lynx -source",
    "    wget -q -O -",
    "",
    "Apart from such toy examples, it is likely that you will need to",
    "manipulate the argument before passing it to the actual fetcher",
    "program.  For example, consider the problem of getting read access to",
    "a repository on a CIFS (SMB) share without mount privileges:",
    "",
    "    export DARCS_GET_SMB='smbclient -c get'",
    "    darcs get smb://fs/twb/Desktop/hello-world",
    "",
    "The above command will not work for several reasons.  Firstly, Darcs",
    "will pass it an argument beginning with `smb:`, which smbclient does",
    "not understand.  Secondly, the host and share `//fs/twb` must be",
    "presented as a separate argument to the path `Desktop/hello-world`.",
    "Thirdly, smbclient requires that `get` and the path be a single",
    "argument (including a space), rather than two separate arguments.",
    "Finally, smbclient's `get` command writes the file to disk, while",
    "Darcs expects it to be printed to standard output.",
    "",
    "In principle, we could get around such problems by making the variable",
    "contain a shell script, unfortunately, Darcs splits the command on",
    "whitespace and does not understand quotation or escaping.  Therefore,",
    "we instead need to put commands in separate, executable scripts.",
    "",
    "Continuing our smbclient example, we create an executable script",
    "`~/.darcs/libexec/get_smb` with the following contents:",
    "",
    "    #!/bin/bash -e",
    "    IFS=/ read host share file <<<'${1#smb://}'",
    "    smbclient //$host/$share -c 'get $file -'",
    "",
    "And at last we can say",
    "",
    "    export DARCS_GET_SMB=~/.darcs/libexec/get_smb",
    "    darcs get smb://fs/twb/Desktop/hello-world",
    "",
    "The APPLY command will be called with a darcs patchfile piped into",
    "its standard input."
    ])


