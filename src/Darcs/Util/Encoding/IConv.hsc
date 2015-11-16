-- Copyright 2007-2009, Judah Jacobson.
-- All Rights Reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- - Redistribution of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.

-- - Redistribution in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE ForeignFunctionInterface #-}
module Darcs.Util.Encoding.IConv
    ( encode, decode
    ) where

import Foreign.C
    ( CString, CSize(..), CInt(..)
    , peekCAString, withCAString
    , Errno(..), getErrno, throwErrno, eINVAL, e2BIG
    )
import Foreign
    ( Ptr, castPtr, nullPtr, plusPtr
    , peek, maybePeek
    , with, maybeWith
    , ForeignPtr, withForeignPtr, newForeignPtr
    , FunPtr
    , Int32, Word8
    )
import Control.Exception ( bracket )
import Data.ByteString ( ByteString, useAsCStringLen, append )
import Data.ByteString.Internal ( createAndTrim' )
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe ( fromMaybe )

#include <locale.h>
#include <langinfo.h>
#include "h_iconv.h"

getLocaleCodeset :: IO String
getLocaleCodeset = bracket (setLocale (Just "")) setLocale (const getCodeset)

encode :: String -> IO ByteString
encode str = getLocaleCodeset >>= \codeset -> openEncoder codeset >>= ($ str)

decode :: ByteString -> IO String
decode str = getLocaleCodeset >>= \codeset -> openDecoder codeset >>= ($ str)

openEncoder :: String -> IO (String -> IO ByteString)
openEncoder codeset = do
    encodeT <- iconvOpen codeset "UTF-8"
    return $ simpleIConv dropUTF8Char encodeT . UTF8.fromString

openDecoder :: String -> IO (ByteString -> IO String)
openDecoder codeset = do
    decodeT <- iconvOpen "UTF-8" codeset
    return $ fmap UTF8.toString . simpleIConv (B.drop 1) decodeT

dropUTF8Char :: ByteString -> ByteString
dropUTF8Char = fromMaybe B.empty . fmap snd . UTF8.uncons

replacement :: Word8
replacement = toEnum (fromEnum '?')

-- handle errors by dropping unuseable chars.
simpleIConv :: (ByteString -> ByteString) -> IConvT -> ByteString -> IO ByteString
simpleIConv dropper t bs = do
    (cs,result) <- iconv t bs
    case result of
        Invalid rest    -> continueOnError cs rest
        Incomplete rest -> continueOnError cs rest
        _               -> return cs
  where
    continueOnError cs rest = fmap ((cs `append`) . (replacement `B.cons`))
                                $ simpleIConv dropper t (dropper rest)

---------------------
-- Setting the locale

foreign import ccall "setlocale" c_setlocale :: CInt -> CString -> IO CString

setLocale :: Maybe String -> IO (Maybe String)
setLocale oldLocale = (maybeWith withCAString) oldLocale $ \loc_p -> do
    c_setlocale (#const LC_CTYPE) loc_p >>= maybePeek peekCAString

-----------------
-- Getting the encoding

type NLItem = #type nl_item

foreign import ccall nl_langinfo :: NLItem -> IO CString

getCodeset :: IO String
getCodeset = do
    str <- nl_langinfo (#const CODESET) >>= peekCAString
    -- check for codesets which may be returned by Solaris, but not understood
    -- by GNU iconv.
    if str `elem` ["","646"]
        then return "ISO-8859-1"
        else return str

----------------
-- Iconv

-- TODO: This may not work on platforms where iconv_t is not a pointer.
type IConvT = ForeignPtr ()
type IConvTPtr = Ptr ()

foreign import ccall "darcs_iconv_open" iconv_open
    :: CString -> CString -> IO IConvTPtr

iconvOpen :: String -> String -> IO IConvT
iconvOpen destName srcName = withCAString destName $ \dest ->
                            withCAString srcName $ \src -> do
                                res <- iconv_open dest src
                                if res == nullPtr `plusPtr` (-1)
                                    then throwErrno $ "iconvOpen "
                                            ++ show (srcName,destName)
                                    -- list the two it couldn't convert between?
                                    else newForeignPtr iconv_close res

-- really this returns a CInt, but it's easiest to just ignore that, I think.
foreign import ccall "& darcs_iconv_close" iconv_close :: FunPtr (IConvTPtr -> IO ())

foreign import ccall "darcs_iconv" c_iconv :: IConvTPtr -> Ptr CString -> Ptr CSize
                            -> Ptr CString -> Ptr CSize -> IO CSize

data Result = Successful
            | Invalid ByteString
            | Incomplete ByteString
    deriving Show

iconv :: IConvT -> ByteString -> IO (ByteString,Result)
iconv cd inStr = useAsCStringLen inStr $ \(inPtr, inBuffLen) ->
        with inPtr $ \inBuff ->
        with (toEnum inBuffLen) $ \inBytesLeft -> do
                out <- loop inBuffLen (castPtr inBuff) inBytesLeft
                return out
    where
        -- TODO: maybe a better algorithm for increasing the buffer size?
        -- and also maybe a different starting buffer size?
        biggerBuffer = (+1)
        loop outSize inBuff inBytesLeft = do
            (bs, errno) <- partialIconv cd outSize inBuff inBytesLeft
            inLeft <- fmap fromEnum $ peek inBytesLeft
            let rest = B.drop (B.length inStr - inLeft) inStr
            case errno of
                Nothing -> return (bs,Successful)
                Just err 
                    | err == e2BIG  -> do -- output buffer too small
                            (bs',result) <- loop (biggerBuffer outSize) inBuff inBytesLeft
                            -- TODO: is this efficient enough?
                            return (bs `append` bs', result)
                    | err == eINVAL -> return (bs,Incomplete rest)
                    | otherwise     -> return (bs, Invalid rest)

partialIconv :: IConvT -> Int -> Ptr CString -> Ptr CSize -> IO (ByteString, Maybe Errno)
partialIconv cd outSize inBuff inBytesLeft =
    withForeignPtr cd $ \cd_p ->
    createAndTrim' outSize $ \outPtr ->
    with outPtr $ \outBuff ->
    with (toEnum outSize) $ \outBytesLeft -> do
        -- ignore the return value; checking the errno is more reliable.
        _ <- c_iconv cd_p inBuff inBytesLeft (castPtr outBuff) outBytesLeft
        outLeft <- fmap fromEnum $ peek outBytesLeft
        inLeft <- peek inBytesLeft
        errno <- if inLeft > 0
                    then fmap Just getErrno
                    else return Nothing
        return (0,outSize - outLeft,errno)

