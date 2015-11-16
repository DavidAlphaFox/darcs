module Darcs.Util.Show
    ( appPrec, BSWrapper(..)
    ) where

import qualified Data.ByteString as B

appPrec :: Int
appPrec = 10

newtype BSWrapper = BSWrapper B.ByteString

instance Show BSWrapper where
    showsPrec d (BSWrapper bs) =
        showParen (d > appPrec) $
            showString "Data.ByteString.Char8.pack " .
            showsPrec (appPrec + 1) bs
