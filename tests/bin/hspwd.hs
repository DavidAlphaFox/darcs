module Main where

import System.Directory ( getCurrentDirectory )

main = getCurrentDirectory >>= putStr
