{-# LANGUAGE DeriveDataTypeable #-}
module Storage.Hashed.Hash( Hash(..), encodeBase64u, decodeBase64u
                          , encodeBase16, decodeBase16, sha256, rawHash
                          , match ) where

import qualified Crypto.Hash.SHA256 as SHA256 ( hash )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8

import qualified Codec.Binary.Base64Url as B64U
import qualified Codec.Binary.Base16 as B16

import Data.Maybe( isJust, fromJust )
import Data.Char( toLower, toUpper )

import Data.Data( Data )
import Data.Typeable( Typeable )

data Hash = SHA256 !BS.ByteString
          | SHA1 !BS.ByteString
          | NoHash
            deriving (Show, Eq, Ord, Read, Typeable, Data)

base16 :: BS.ByteString -> BS.ByteString
debase16 :: BS.ByteString -> Maybe BS.ByteString
base64u :: BS.ByteString -> BS.ByteString
debase64u :: BS.ByteString -> Maybe BS.ByteString

base16 = BS8.map toLower . B16.b16_enc
base64u = B64U.encode
debase64u bs = case B64U.decode bs of
                 Right s -> Just s
                 Left _ -> Nothing
debase16 bs = case B16.b16_dec $ BS8.map toUpper bs of
                Right (s, _) -> Just s
                Left _ -> Nothing

encodeBase64u :: Hash -> BS.ByteString
encodeBase64u (SHA256 bs) = base64u bs
encodeBase64u (SHA1 bs) = base64u bs
encodeBase64u NoHash = BS.empty

-- | Produce a base16 (ascii-hex) encoded string from a hash. This can be
-- turned back into a Hash (see "decodeBase16". This is a loss-less process.
encodeBase16 :: Hash -> BS.ByteString
encodeBase16 (SHA256 bs) = base16 bs
encodeBase16 (SHA1 bs) = base16 bs
encodeBase16 NoHash = BS.empty

-- | Take a base64/url-encoded string and decode it as a "Hash". If the string
-- is malformed, yields NoHash.
decodeBase64u :: BS.ByteString -> Hash
decodeBase64u bs
    | BS.length bs == 44 && isJust (debase64u bs) = SHA256 (fromJust $ debase64u bs)
    | BS.length bs == 28 && isJust (debase64u bs) = SHA1 (fromJust $ debase64u bs)
    | otherwise = NoHash

-- | Take a base16-encoded string and decode it as a "Hash". If the string is
-- malformed, yields NoHash.
decodeBase16 :: BS.ByteString -> Hash
decodeBase16 bs | BS.length bs == 64 && isJust (debase16 bs) = SHA256 (fromJust $ debase16 bs)
                | BS.length bs == 40 && isJust (debase16 bs) = SHA1 (fromJust $ debase16 bs)
                | otherwise = NoHash

-- | Compute a sha256 of a (lazy) ByteString. However, although this works
-- correctly for any bytestring, it is only efficient if the bytestring only
-- has a sigle chunk.
sha256 :: BL.ByteString -> Hash
sha256 bits = SHA256 $ SHA256.hash $ BS.concat $ BL.toChunks bits

rawHash :: Hash -> BS.ByteString
rawHash NoHash = error "Cannot obtain raw hash from NoHash."
rawHash (SHA1 s) = s
rawHash (SHA256 s) = s

match :: Hash -> Hash -> Bool
NoHash `match` _ = False
_ `match` NoHash = False
x `match` y = x == y
