module Storage.Hashed
    ( -- * Obtaining Trees.
    --
    -- | Please note that Trees obtained this way will contain Stub
    -- items. These need to be executed (they are IO actions) in order to be
    -- accessed. Use 'expand' to do this. However, many operations are
    -- perfectly fine to be used on a stubbed Tree (and it is often more
    -- efficient to do everything that can be done before expanding a Tree).
    readPlainTree, readDarcsHashed

    -- * Blob access.
    , readBlob

    -- * Writing trees.
    , writePlainTree, writeDarcsHashed

    -- * Unsafe functions for the curious explorer.
    --
    -- | These are more useful for playing within ghci than for real, serious
    -- programs. They generally trade safety for conciseness. Please use
    -- responsibly. Don't kill innocent kittens.
    , floatPath, printPath ) where

import Storage.Hashed.AnchoredPath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Storage.Hashed.Tree ( Tree, TreeItem(..), listImmediate, find, readBlob )

-- For re-exports.
import Storage.Hashed.Darcs( readDarcsHashed, writeDarcsHashed )
import Storage.Hashed.Plain( readPlainTree, writePlainTree )

------------------------
-- For explorers
--

-- | Take a relative FilePath within a Tree and print the contents of the
-- object there. Useful for exploration, less so for serious programming.
printPath :: Tree IO -> FilePath -> IO ()
printPath t p = print' $ find t (floatPath p)
    where print' Nothing = putStrLn $ "ERROR: No object at " ++ p
          print' (Just (File b)) = do
            putStrLn $ "== Contents of file " ++ p ++ ":"
            BL.unpack `fmap` readBlob b >>= putStr
          print' (Just (SubTree t')) = do
            putStrLn $ "== Listing Tree " ++ p ++ " (immediates only):"
            putStr $ unlines $ map BS.unpack $ listNames t'
          print' (Just (Stub _ _)) =
            putStrLn $ "== (not listing stub at " ++ p ++ ")"
          listNames t' = [ n | (Name n, _) <- listImmediate t' ]

