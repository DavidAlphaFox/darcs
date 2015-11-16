-- Copyright (C) 2005 Benedikt Schmidt
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

-- |
-- Module      : Darcs.Util.CommandLine
-- Copyright   : 2005 Benedikt Schmidt
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- |A parser for commandlines, returns an arg list and expands
-- format strings given in a translation table. Additionally
-- the commandline can end with "%<" specifying that the command
-- expects input on stdin.
--
-- Some tests for the parser.
--
-- > formatTable = [('s',"<insert subject here>"),
-- >                ('a',"<insert author here>")]
-- >
-- > testParser :: (Show a, Eq a) => Parser a -> String -> a -> a
-- > testParser p s ok = case parse p "" s of
-- >                     Left e -> error $ "Parser failed with: " ++ (show e)
-- >                     Right res -> if res == ok
-- >                                  then res
-- >                                  else error $ "Parser failed: got "
-- >                                         ++ (show res) ++ ", expected "
-- >                                         ++ (show ok)
-- >
-- > testCases = [("a b",(["a","b"], False)),
-- >              ("a b %<",(["a","b"], True)),
-- >              ("a b %< ",(["a","b"], True)),
-- >              ("\"arg0 contains spaces \\\"quotes\\\"\" b",
-- >               (["arg0 contains spaces \"quotes\"","b"],False)),
-- >              ("a %s %<",(["a","<insert subject here>"], True))]
-- >
-- > runTests = map (uncurry $ testParser (commandline formatTable)) testCases

module Darcs.Util.CommandLine
    ( parseCmd
    , addUrlencoded
    ) where

import Control.Arrow ( (***) )
import Data.Char ( ord, intToDigit, toUpper )
import Data.List ( find )
import Text.ParserCombinators.Parsec

-- | assoc list mapping characters to strings
-- eg (c,s) means that %c is replaced by s
type FTable = [(Char,String)]

commandline :: FTable -> Parser ([String], Bool)
commandline ftable = consumeAll $ do
    l <- sepEndBy1 (arg ftable) (try separator)
    redir <- formatRedir
    spaces
    return (l,redir)

arg :: FTable -> Parser String
arg ftable = quotedArg ftable <|> unquotedArg ftable

unquotedArg :: FTable -> Parser String
unquotedArg ftable = try (format ftable) <|> many1 (noneOf " \t\"%")

quotedArg :: FTable -> Parser String
quotedArg ftable = between quoteChar quoteChar $ quoteContent ftable
  where
    quoteChar = char '"'

quoteContent :: FTable -> Parser String
quoteContent ftable = do s1 <- escape
                               <|> try (format ftable)
                               <|> many1 (noneOf "\"\\%")
                         s2 <- quoteContent ftable
                         return $ s1 ++ s2
                     <|> return ""

formatRedir :: Parser Bool
formatRedir = (string "%<" >> return True)
              <|> return False

format :: FTable -> Parser String
format ftable = do _ <- char '%'
                   c <- oneOf (map fst ftable)
                   return $ expandFormat ftable c

escape :: Parser String
escape = do _ <- char '\\'
            c <- anyChar
            return [c]

consumeAll :: Parser a -> Parser a
consumeAll p = do r <- p
                  eof
                  return r

separator :: Parser ()
separator = skipMany1 space

expandFormat :: FTable -> Char -> String
expandFormat ftable c = case find ((==c) . fst) ftable of
                            Just (_,s) -> s
                            Nothing -> error "impossible"

-- | parse a commandline returning a list of strings
-- (intended to be used as argv) and a bool value which
-- specifies if the command expects input on stdin
-- format specifiers with a mapping in ftable are accepted
-- and replaced by the given strings. E.g. if the ftable is
-- [('s',"Some subject")], then "%s" is replaced by "Some subject"
parseCmd :: FTable -> String -> Either ParseError ([String],Bool)
parseCmd ftable = parse (commandline ftable) ""

urlEncode :: String -> String
urlEncode = concatMap escapeC
    where escapeC x = if allowed x then [x] else '%' : intToHex (ord x)
          intToHex i = map intToDigit [i `div` 16, i `mod` 16]
          allowed x = x `elem` allowedChars
          allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!'()*-.~"

-- | for every mapping (c,s), add a mapping with uppercase c
-- and the urlencoded string s
addUrlencoded :: FTable -> FTable
addUrlencoded ftable = ftable ++ map (toUpper *** urlEncode) ftable
