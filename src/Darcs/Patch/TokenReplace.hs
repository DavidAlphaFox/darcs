module Darcs.Patch.TokenReplace
    (
      tryTokInternal
    , forceTokReplace
    , breakOutToken
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( isNothing )

import Darcs.Util.ByteString ( substrPS, linesPS, unlinesPS )
import Darcs.Patch.RegChars ( regChars )

-- | breakOutToken takes a String of token chars and an input ByteString, and
-- returns the ByteString triple of (beforeToken, token, afterToken).
breakOutToken :: String -> BC.ByteString
              -> (BC.ByteString, BC.ByteString, BC.ByteString)
breakOutToken tokChars input =
    let isTokChar = regChars tokChars
        (before, tokAndRest) = BC.break isTokChar input
        (tok, remaining) = BC.break (not . isTokChar) tokAndRest in
    (before, tok, remaining)

-- | tryTokInternal takes a String of token chars, an oldToken ByteString, a
-- newToken ByteString and returns the list of token-delimited ByteStrings,
-- with any tokens matching oldToken being replaced by newToken. If newToken is
-- already in the input, we return Nothing.
tryTokInternal :: String -> B.ByteString -> B.ByteString -> B.ByteString
               -> Maybe [B.ByteString]
tryTokInternal _ oldToken newToken input
  | isNothing (substrPS oldToken input) &&
    isNothing (substrPS newToken input) = Just [ input ]
tryTokInternal tokChars oldToken newToken input =
    let (before, tok, remaining) = breakOutToken tokChars input in
    case tryTokInternal tokChars oldToken newToken remaining of
        Nothing -> Nothing
        Just rest | tok == oldToken -> Just $ before : newToken : rest
                  | tok == newToken -> Nothing
                  | otherwise -> Just $ before : tok : rest

-- | forceTokReplace replaces all occurrences of the old token with the new
-- token, throughout the input ByteString.
forceTokReplace :: String -> B.ByteString -> B.ByteString -> B.ByteString
                -> B.ByteString
forceTokReplace tokChars oldToken newToken = forceReplaceLines
  where
    forceReplaceLines = unlinesPS . map forceReplace . linesPS
    breakOutAllTokens input | B.null input = []
    breakOutAllTokens input =
        let (before, tok, remaining) = breakOutToken tokChars input in
        before : tok : breakOutAllTokens remaining

    forceReplace = B.concat . map replaceMatchingToken . breakOutAllTokens

    replaceMatchingToken input | input == oldToken = newToken
                               | otherwise = input
