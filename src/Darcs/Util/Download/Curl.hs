{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- needed for GHC 7.0/7.2
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.Util.Download.Curl where

#ifdef HAVE_CURL

import Control.Exception ( bracket )
import Control.Monad ( when )
import Foreign.C.Types ( CLong(..), CInt(..) )

import Darcs.Util.Progress ( debugMessage )

import Darcs.Util.Download.Request

import Foreign.C.String ( withCString, peekCString, CString )
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

setDebugHTTP :: IO ()
setDebugHTTP = curl_enable_debug

requestUrl :: String -> FilePath -> Cachable -> IO String
requestUrl u f cache =
  withCString u $ \ustr ->
  withCString f $ \fstr ->
  bracket malloc free $ \ errorPointer -> do
    e <- curl_request_url ustr fstr (cachableToInt cache) errorPointer >>= peekCString
    errorNum <- peek errorPointer
    when (errorNum == 90 ) $ debugMessage "The environment variable DARCS_CONNECTION_TIMEOUT is not a number"
    return e

waitNextUrl :: IO (String, String, Maybe ConnectionError)
waitNextUrl =
  bracket malloc free $ \ errorPointer ->
  bracket malloc free $ \ httpErrorPointer -> do
    e <- curl_wait_next_url errorPointer httpErrorPointer >>= peekCString
    ce <- do
           errorNum <- peek errorPointer
           if null e then return Nothing
             else return $
              case errorNum of
                6  -> Just CouldNotResolveHost
                7  -> Just CouldNotConnectToServer
                28 -> Just OperationTimeout
                _  -> Nothing
    u <- curl_last_url >>= peekCString
    httpErrorCode <- peek httpErrorPointer
    let detailedErrorMessage = if httpErrorCode > 0
                               then e ++ " " ++ show httpErrorCode
                               else e
    return (u, detailedErrorMessage, ce)

pipeliningEnabled :: IO Bool
pipeliningEnabled = do
  r <- curl_pipelining_enabled
  return $ r /= 0

cachableToInt :: Cachable -> CInt
cachableToInt Cachable = -1
cachableToInt Uncachable = 0
cachableToInt (MaxAge n) = n

foreign import ccall "hscurl.h curl_request_url"
  curl_request_url :: CString -> CString -> CInt -> Ptr CInt -> IO CString

foreign import ccall "hscurl.h curl_wait_next_url"
  curl_wait_next_url :: Ptr CInt -> Ptr CLong-> IO CString

foreign import ccall "hscurl.h curl_last_url"
  curl_last_url :: IO CString

foreign import ccall "hscurl.h curl_enable_debug"
  curl_enable_debug :: IO ()

foreign import ccall "hscurl.h curl_pipelining_enabled"
  curl_pipelining_enabled :: IO CInt

#endif
