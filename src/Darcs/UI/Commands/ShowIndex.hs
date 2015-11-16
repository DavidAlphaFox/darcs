-- Copyright (C) 2009 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE CPP #-}
module Darcs.UI.Commands.ShowIndex
    ( showIndex
    , showPristineCmd -- for alias
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( (>=>) )
import Darcs.UI.Flags ( DarcsFlag(NullFlag), useCache )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Prelude hiding ( (^) )
import Darcs.UI.Options ( DarcsOption, (^), oid, odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository ( withRepository, RepoJob(..), readIndex )
import Darcs.Repository.State ( readRecorded )

import Storage.Hashed( floatPath )
import Storage.Hashed.Hash( encodeBase16, Hash( NoHash ) )
import Storage.Hashed.Tree( list, expand, itemHash, Tree, TreeItem( SubTree ) )
import Storage.Hashed.Index( updateIndex, listFileIDs )
import Darcs.Util.Path( anchorPath, AbsolutePath )

import System.Posix.Types ( FileID )

import qualified Data.ByteString.Char8 as BS
import Data.Maybe ( fromJust )
import qualified Data.Map as M ( Map, lookup, fromList )

showIndexBasicOpts :: DarcsOption a
                      (Bool -> Bool -> Bool -> Maybe String -> a)
showIndexBasicOpts = O.files ^ O.directories ^ O.nullFlag ^ O.workingRepoDir

showIndexOpts :: DarcsOption a
                 (Bool
                  -> Bool
                  -> Bool
                  -> Maybe String
                  -> Maybe O.StdCmdAction
                  -> Bool
                  -> Bool
                  -> O.Verbosity
                  -> Bool
                  -> O.UseCache
                  -> Maybe String
                  -> Bool
                  -> Maybe String
                  -> Bool
                  -> a)
showIndexOpts = showIndexBasicOpts `withStdOpts` oid

showIndex :: DarcsCommand [DarcsFlag]
showIndex = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "index",
  commandDescription = "Dump contents of working tree index.",
  commandHelp =
      "The `darcs show index` command lists all version-controlled files and " ++
      "directories along with their hashes as stored in `_darcs/index`. " ++
      "For files, the fields correspond to file size, sha256 of the current " ++
      "file content and the filename.",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showIndexCmd,
  commandPrereq = amInRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = odesc showIndexBasicOpts,
  commandDefaults = defaultFlags showIndexOpts,
  commandCheckOptions = ocheck showIndexOpts,
  commandParseOptions = onormalise showIndexOpts }

dump :: [DarcsFlag] -> Maybe (M.Map FilePath FileID) -> Tree IO -> IO ()
dump opts fileids tree = do
  let line | NullFlag `elem` opts = \t -> putStr t >> putChar '\0'
           | otherwise = putStrLn
      output (p, i) = do
        let hash = case itemHash i of
                     NoHash -> "(no hash available)"
                     h -> BS.unpack $ encodeBase16 h
            path = anchorPath "" p
            isdir = case i of
                      SubTree _ -> "/"
                      _ -> ""
            fileid = case fileids of
                       Nothing -> ""
                       Just fileids' -> " " ++ (show $ fromJust $ M.lookup path fileids')
        line $ hash ++ fileid ++ " " ++ path ++ isdir
  x <- expand tree
  mapM_ output $ (floatPath ".", SubTree x) : list x

showIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showIndexCmd _ opts _ = withRepository (useCache opts) $ RepoJob $ \repo ->
  do index <- readIndex repo
     index_tree <- updateIndex index
     fileids <- (M.fromList . map (\((a,_),b) -> (anchorPath "" a,b))) <$> listFileIDs index
     dump opts (Just fileids) index_tree

showPristineCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPristineCmd _ opts _ = withRepository (useCache opts) $ RepoJob $
  readRecorded >=> dump opts Nothing

