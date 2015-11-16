module Darcs.Util.Crypt.SHA256 ( sha256sum ) where

import Crypto.Hash.SHA256 ( hash )
import Data.ByteString ( ByteString )
import Data.ByteString.Base16 ( encode )
import Data.ByteString.Char8 ( unpack )

sha256sum :: ByteString -> String
sha256sum = unpack . encode . hash
