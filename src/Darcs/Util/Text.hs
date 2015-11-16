{-# LANGUAGE OverloadedStrings #-}

module Darcs.Util.Text
    (
    -- * Text construction.
      sentence
    -- * Text formatting.
    , formatText
    , formatParas
    , formatPara
    , chompTrailingNewline
    -- * Text processing
    , breakCommand
    ) where

import Control.Arrow ( first )
import Data.List ( intercalate )

import Darcs.Util.Printer ( Doc, (<>) )

sentence :: Doc -> Doc
sentence = (<> ".")

-- |Take a list of paragraphs and format them to the given line length, with
-- a blank line between paragraphs.
formatText :: Int -> [String] -> String
formatText linelen = unlines . formatParas linelen

formatParas :: Int -> [String] -> [String]
formatParas linelen = intercalate [""] .
                      map (map unwords . formatPara linelen . words)

-- |Take a list of words and split it up so that each chunk fits into the specified width
-- when spaces are included. Any words longer than the specified width end up in a chunk
-- of their own.
formatPara :: Int -> [[a]] -> [[[a]]]
formatPara w = para'
  where para' [] = []
        para' xs = uncurry (:) $ para'' w xs
        para'' r (x:xs) | w == r || length x < r = first (x:) $ para'' (r - length x - 1) xs
        para'' _ xs = ([], para' xs)

breakCommand :: String -> (String, [String])
breakCommand s = case words s of
                   (arg0:args) -> (arg0,args)
                   [] -> (s,[])

chompTrailingNewline :: String -> String
chompTrailingNewline "" = ""
chompTrailingNewline s = if last s == '\n' then init s else s
