{-# OPTIONS_GHC -fno-warn-deprecations #-} -- using isEmptyChan
{-# LANGUAGE CPP #-}

-- |
-- Module      : Darcs.Util.Download
-- Copyright   : 2008 Dmitry Kurochkin <dmitry.kurochkin@gmail.com>
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Util.Download
    ( copyUrl
    , copyUrlFirst
    , setDebugHTTP
    , disableHTTPPipelining
    , maxPipelineLength
    , waitUrl
    , Cachable(Cachable, Uncachable, MaxAge)
    , environmentHelpProxy
    , environmentHelpProxyPassword
    , ConnectionError(..)
    ) where

import Control.Arrow ( (&&&) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( isEmptyChan, newChan, readChan, writeChan,
                                 Chan )
import Control.Concurrent.MVar ( isEmptyMVar, modifyMVar_, modifyMVar, newEmptyMVar,
                                 newMVar, putMVar, readMVar, withMVar, MVar )
import Control.Monad ( unless, when )
import Control.Monad.State ( evalStateT, get, modify, put, StateT )
import Control.Monad.Trans ( liftIO )
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Tuple ( swap )
import System.Directory ( copyFile )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomRIO )

import Darcs.Util.AtExit ( atexit )
import Darcs.Util.File ( removeFileMayNotExist )
import Numeric ( showHex )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Download.Request
import Darcs.Util.Workaround ( renameFile )

#ifdef HAVE_CURL
import qualified Darcs.Util.Download.Curl as Curl
#elif defined(HAVE_HTTP)
import qualified Darcs.Util.Download.HTTP as HTTP
#else
import Darcs.Util.Progress ( debugFail )
import qualified HTTP ( requestUrl, waitNextUrl )
#endif

#include "impossible.h"

{-# NOINLINE maxPipelineLengthRef #-}
maxPipelineLengthRef :: IORef Int
maxPipelineLengthRef = unsafePerformIO $ do
    enabled <- pipeliningEnabled
#ifdef HAVE_CURL
    unless enabled $ debugMessage $
        "Warning: pipelining is disabled, because libcurl version darcs was "
        ++ "compiled with is too old (< 7.19.1)"
#endif
    newIORef $ if enabled then 100 else 1

maxPipelineLength :: IO Int
maxPipelineLength = readIORef maxPipelineLengthRef

{-# NOINLINE urlNotifications #-}
urlNotifications :: MVar (Map String (MVar (Maybe String)))
urlNotifications = unsafePerformIO $ newMVar Map.empty

{-# NOINLINE urlChan #-}
urlChan :: Chan UrlRequest
urlChan = unsafePerformIO $ do
    ch <- newChan
    _ <- forkIO (urlThread ch)
    return ch

type UrlM a = StateT UrlState IO a

urlThread :: Chan UrlRequest -> IO ()
urlThread ch = do
    junk <- flip showHex "" `fmap` randomRIO rrange
    evalStateT urlThread' (UrlState Map.empty emptyQ 0 junk)
  where
    rrange = (0, 2 ^ (128 :: Integer) :: Integer)

    urlThread' :: UrlM ()
    urlThread' = do
        empty <- liftIO $ isEmptyChan ch
        (l, w) <- (pipeLength &&& waitToStart) `fmap` get
        -- If we've got UrlRequests waiting on the chan, or there's nothing
        -- waiting to start and nothing already downloading, we just block
        -- waiting for more UrlRequests.
        reqs <- if not empty || (nullQ w && l == 0)
                    then liftIO readAllRequests
                    else return []
        mapM_ addReq reqs
        checkWaitToStart
        waitNextUrl
        urlThread'

    readAllRequests :: IO [UrlRequest]
    readAllRequests = do
        r <- readChan ch
        debugMessage $ "URL.urlThread (" ++ url r ++ "\n"++
                       "-> " ++ file r ++ ")"
        empty <- isEmptyChan ch
        reqs <- if not empty
                then readAllRequests
                else return []
        return (r : reqs)

    -- | addReq adds a UrlRequest to the current downloads, being careful to
    -- update the lists of target filenames if the url is already being
    -- downloaded.
    addReq :: UrlRequest -> UrlM ()
    addReq (UrlRequest u f c p) = do
        d <- liftIO (alreadyDownloaded u)
        if d
            then dbg "Ignoring UrlRequest of URL that is already downloaded."
            else do
            (ip, wts) <- (inProgress &&& waitToStart) `fmap` get
            case Map.lookup u ip of
                Nothing -> modify $ \st ->
                    st { inProgress = Map.insert u (f, [], c) ip
                       , waitToStart = addUsingPriority p u wts }
                Just (f', fs', c') -> do
                    let new_c = minCachable c c'
                    when (c /= c') $ do
                        let new_p = Map.insert u (f', fs', new_c) ip
                        modify (\s -> s { inProgress = new_p })
                        dbg $ "Changing " ++ u ++ " request cachability from "
                              ++ show c ++ " to " ++ show new_c
                    when (u `elemQ` wts && p == High) $ do
                        modify $ \s ->
                            s { waitToStart = pushQ u (deleteQ u wts) }
                        dbg $ "Moving " ++ u ++ " to head of download queue."
                    if f `notElem` (f' : fs')
                        then do
                            let new_ip = Map.insert u (f', f : fs', new_c) ip
                            modify (\s -> s { inProgress = new_ip })
                            dbg "Adding new file to existing UrlRequest."
                        else dbg $ "Ignoring UrlRequest of file that's "
                                   ++ "already queued."

    alreadyDownloaded :: String -> IO Bool
    alreadyDownloaded u = do
        n <- withMVar urlNotifications $ return . Map.lookup u
        maybe (return True) (\v -> not `fmap` isEmptyMVar v) n

-- |'checkWaitToStart' will inspect the current waiting-to-start queue, if the
-- pipe isn't full,
checkWaitToStart :: UrlM ()
checkWaitToStart = do
    st <- get
    let l = pipeLength st
    mpl <- liftIO maxPipelineLength
    when (l < mpl) $
        case readQ (waitToStart st) of
            Nothing -> return ()
            Just (u, rest) -> do
                case Map.lookup u (inProgress st) of
                    Nothing -> bug $ "bug in URL.checkWaitToStart " ++ u
                    Just (f, _, c) -> do
                        dbg $ "URL.requestUrl (" ++ u ++ "\n"
                              ++ "-> " ++ f ++ ")"
                        let f_new = createDownloadFileName f st
                        err <- liftIO $ requestUrl u f_new c
                        if null err
                            then do
                                -- waitNextUrl might never return this url as
                                -- complete/failed, so being careful, we should
                                -- try and delete the corresponding file atexit
                                liftIO $ atexit (removeFileMayNotExist f_new)
                                -- We've started off another download, so the
                                -- pipline length should increase.
                                put $ st { waitToStart = rest
                                         , pipeLength = l + 1 }
                            else do
                                dbg $ "Failed to start download URL " ++ u
                                      ++ ": " ++ err
                                liftIO $ do
                                    removeFileMayNotExist f_new
                                    downloadComplete u err
                                put $ st { waitToStart = rest }
                checkWaitToStart

copyUrlFirst :: String -> FilePath -> Cachable -> IO ()
copyUrlFirst = copyUrlWithPriority High

copyUrl :: String -> FilePath -> Cachable -> IO ()
copyUrl = copyUrlWithPriority Low

copyUrlWithPriority :: Priority -> String -> String -> Cachable -> IO ()
copyUrlWithPriority p u f c = do
    debugMessage $ "URL.copyUrlWithPriority (" ++ u ++ "\n"
                   ++ "-> " ++ f ++ ")"
    v <- newEmptyMVar
    old_mv <- modifyMVar urlNotifications (return . swap . Map.insertLookupWithKey (\_k _n old -> old) u v)
    case old_mv of
        Nothing -> writeChan urlChan $ UrlRequest u f c p -- ok, new URL
        Just _  -> debugMessage $ "URL.copyUrlWithPriority already in progress, skip (" ++ u ++ "\n" ++ "-> " ++ f ++ ")"

createDownloadFileName :: FilePath -> UrlState -> FilePath
createDownloadFileName f st = f ++ "-new_" ++ randomJunk st

waitNextUrl :: UrlM ()
waitNextUrl = do
    st <- get
    let l = pipeLength st
    when (l > 0) $ do
        dbg "URL.waitNextUrl start"
        (u, e, ce) <- liftIO waitNextUrl'
        let p = inProgress st
        liftIO $ case Map.lookup u p of
            Nothing ->
                -- A url finished downloading, but we don't have a record of it
                bug $ "bug in URL.waitNextUrl: " ++ u
            Just (f, fs, _) -> if null e
                then do -- Succesful download
                    renameFile (createDownloadFileName f st) f
                    mapM_ (safeCopyFile st f) fs
                    downloadComplete u e
                    debugMessage $
                        "URL.waitNextUrl succeeded: " ++ u ++ " " ++ f
                else do -- An error while downloading
                    removeFileMayNotExist (createDownloadFileName f st)
                    downloadComplete u (maybe e show ce)
                    debugMessage $
                        "URL.waitNextUrl failed: " ++ u ++ " " ++ f ++ " " ++ e
        unless (null u) . put $ st { inProgress = Map.delete u p
                                   , pipeLength = l - 1 }
  where
    safeCopyFile st f t = do
        let new_t = createDownloadFileName t st
        copyFile f new_t
        renameFile new_t t

downloadComplete :: String -> String -> IO ()
downloadComplete u e = do
    r <- withMVar urlNotifications (return . Map.lookup u)
    case r of
        Just notifyVar ->
            putMVar notifyVar $ if null e then Nothing else Just e
        Nothing -> debugMessage $ "downloadComplete URL '" ++ u
                                  ++ "' downloaded several times"

waitUrl :: String -> IO ()
waitUrl u = do
    debugMessage $ "URL.waitUrl " ++ u
    r <- withMVar urlNotifications (return . Map.lookup u)
    case r of
        Nothing  -> return () -- file was already downloaded
        Just var -> do
            mbErr <- readMVar var
            modifyMVar_ urlNotifications (return . Map.delete u)
            flip (maybe (return ())) mbErr $ \e -> do
                debugMessage $ "Failed to download URL " ++ u ++ ": " ++ e
                fail e

dbg :: String -> StateT a IO ()
dbg = liftIO . debugMessage

minCachable :: Cachable -> Cachable -> Cachable
minCachable Uncachable _          = Uncachable
minCachable _          Uncachable = Uncachable
minCachable (MaxAge a) (MaxAge b) = MaxAge $ min a b
minCachable (MaxAge a) _          = MaxAge a
minCachable _          (MaxAge b) = MaxAge b
minCachable _          _          = Cachable

disableHTTPPipelining :: IO ()
disableHTTPPipelining = writeIORef maxPipelineLengthRef 1

setDebugHTTP :: IO ()
requestUrl :: String -> FilePath -> Cachable -> IO String
waitNextUrl' :: IO (String, String, Maybe ConnectionError)
pipeliningEnabled :: IO Bool

#ifdef HAVE_CURL

setDebugHTTP = Curl.setDebugHTTP
requestUrl = Curl.requestUrl
waitNextUrl' = Curl.waitNextUrl
pipeliningEnabled = Curl.pipeliningEnabled

#elif defined(HAVE_HTTP)

setDebugHTTP = return ()
requestUrl = HTTP.requestUrl
waitNextUrl' = HTTP.waitNextUrl
pipeliningEnabled = return False

#else

setDebugHTTP = debugMessage "URL.setDebugHttp works only with libcurl"
requestUrl _ _ _ = debugFail "URL.requestUrl: there is no libcurl!"
waitNextUrl' = debugFail "URL.waitNextUrl': there is no libcurl!"
pipeliningEnabled = return False

#endif

-- Usage of these environment variables happens in C code, so the
-- closest to "literate" user documentation is here, where the
-- offending function 'curl_request_url' is imported.
environmentHelpProxy :: ([String], [String])
environmentHelpProxy =
    ( [ "HTTP_PROXY", "HTTPS_PROXY", "FTP_PROXY", "ALL_PROXY", "NO_PROXY"]
    , [ "If Darcs was built with libcurl, the environment variables"
      , "HTTP_PROXY, HTTPS_PROXY and FTP_PROXY can be set to the URL of a"
      , "proxy in the form"
      , ""
      , "    [protocol://]<host>[:port]"
      , ""
      , "In which case libcurl will use the proxy for the associated protocol"
      , "(HTTP, HTTPS and FTP). The environment variable ALL_PROXY can be used"
      , "to set a single proxy for all libcurl requests."
      , ""
      , "If the environment variable NO_PROXY is a comma-separated list of"
      , "host names, access to those hosts will bypass proxies defined by the"
      , "above variables. For example, it is quite common to avoid proxying"
      , "requests to machines on the local network with"
      , ""
      , "    NO_PROXY=localhost,*.localdomain"
      , ""
      , "For compatibility with lynx et al, lowercase equivalents of these"
      , "environment variables (e.g. $http_proxy) are also understood and are"
      , "used in preference to the uppercase versions."
      , ""
      , "If Darcs was not built with libcurl, all these environment variables"
      , "are silently ignored, and there is no way to use a web proxy."
      ]
    )

environmentHelpProxyPassword :: ([String], [String])
environmentHelpProxyPassword =
    ( [ "DARCS_PROXYUSERPWD" ]
    , [ "If Darcs was built with libcurl, and you are using a web proxy that"
      , "requires authentication, you can set the $DARCS_PROXYUSERPWD"
      , "environment variable to the username and password expected by the"
      , "proxy, separated by a colon.  This environment variable is silently"
      , "ignored if Darcs was not built with libcurl."
      ]
    )
