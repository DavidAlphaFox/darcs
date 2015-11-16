{-# LANGUAGE CPP #-}

module Darcs.Util.Download.HTTP
    ( fetchUrl, postUrl, requestUrl, waitNextUrl
    ) where

import Prelude hiding ( catch )

import Darcs.Util.Global ( debugFail )

import Darcs.Util.Download.Request ( ConnectionError(..) )

#ifdef HAVE_HTTP
import Control.Exception ( catch, IOException )
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Network.HTTP
import Network.Browser ( browse, request, setCheckForProxy, setErrHandler, setOutHandler )
import Network.URI
import System.IO.Error ( ioeGetErrorString )
import System.IO.Unsafe ( unsafePerformIO )
import Darcs.Util.Global ( debugMessage )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Version ( version )
#endif

fetchUrl :: String -> IO String
postUrl
    :: String     -- ^ url
    -> String     -- ^ body
    -> String     -- ^ mime type
    -> IO ()  -- ^ result

requestUrl :: String -> FilePath -> a -> IO String
waitNextUrl :: IO (String, String, Maybe ConnectionError)

#ifdef HAVE_HTTP

headers :: [Header]
headers =  [Header HdrUserAgent $ "darcs-HTTP/" ++ version]

fetchUrl url = case parseURI url of
    Nothing -> fail $ "Invalid URI: " ++ url
    Just uri -> do debugMessage $ "Fetching over HTTP:  "++url
                   resp <- catch (browse $ do
                     setCheckForProxy True
                     setOutHandler debugMessage
                     setErrHandler debugMessage
                     request Request { rqURI = uri,
                                       rqMethod = GET,
                                       rqHeaders = headers,
                                       rqBody = "" })
                     (\(err :: IOException) -> debugFail $ show err)
                   case resp of
                     (_, res@Response { rspCode = (2,0,0) }) -> return (rspBody res)
                     (_, Response { rspCode = (x,y,z) }) ->
                         debugFail $ "HTTP " ++ show x ++ show y ++ show z ++ " error getting " ++ show uri

postUrl url body mime = case parseURI url of
    Nothing -> fail $ "Invalid URI: " ++ url
    Just uri -> do debugMessage $ "Posting to HTTP:  "++url
                   resp <- catch (browse $ do
                     setCheckForProxy True
                     setOutHandler debugMessage
                     setErrHandler debugMessage
                     request Request { rqURI = uri,
                                       rqMethod = POST,
                                       rqHeaders = headers ++ [Header HdrContentType mime,
                                                               Header HdrAccept "text/plain",
                                                               Header HdrContentLength
                                                                        (show $ length body) ],
                                       rqBody = body })
                     (\(err :: IOException) -> debugFail $ show err)
                   case resp of
                     (_, res@Response { rspCode = (2,y,z) }) -> do
                        putStrLn $ "Success 2" ++ show y ++ show z
                        putStrLn (rspBody res)
                        return ()
                     (_, res@Response { rspCode = (x,y,z) }) -> do
                        putStrLn $ rspBody res
                        debugFail $ "HTTP " ++ show x ++ show y ++ show z ++ " error posting to " ++ show uri

requestedUrl :: IORef (String, FilePath)
requestedUrl = unsafePerformIO $ newIORef ("", "")

requestUrl u f _ = do
  (u', _) <- readIORef requestedUrl
  if null u'
     then do writeIORef requestedUrl (u, f)
             return ""
     else return "URL already requested"

waitNextUrl = do
  (u, f) <- readIORef requestedUrl
  if null u
     then return ("", "No URL requested", Nothing)
     else do writeIORef requestedUrl ("", "")
             e <- (fetchUrl u >>= \s -> B.writeFile f (BC.pack s) >> return "") `catch` h
             let ce = case e of
                       "timeout" -> Just OperationTimeout
                       _         -> Nothing
             return (u, e, ce)
    where h = return . ioeGetErrorString

#else

fetchUrl _ = debugFail "Network.HTTP does not exist"
postUrl _ _ _ = debugFail "Cannot use http POST because darcs was not compiled with Network.HTTP."

requestUrl _ _ _ = debugFail "Network.HTTP does not exist"
waitNextUrl = debugFail "Network.HTTP does not exist"

#endif
