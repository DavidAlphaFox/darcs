-- | This module implements relative paths within a Tree. All paths are
-- anchored at a certain root (this is usually the Tree root). They are
-- represented by a list of Names (these are just strict bytestrings).
module Storage.Hashed.AnchoredPath
    ( Name(..), AnchoredPath(..), anchoredRoot, appendPath, anchorPath
    , isPrefix, parent, parents, catPaths, flatten, makeName, appendToName
    -- * Unsafe functions.
    , floatBS, floatPath, replacePrefixPath ) where

import qualified Data.ByteString.Char8 as BS
import Data.List( isPrefixOf, inits )
import System.FilePath( (</>), splitDirectories, normalise, dropTrailingPathSeparator )

-------------------------------
-- AnchoredPath utilities
--

newtype Name = Name BS.ByteString  deriving (Eq, Show, Ord)

-- | This is a type of "sane" file paths. These are always canonic in the sense
-- that there are no stray slashes, no ".." components and similar. They are
-- usually used to refer to a location within a Tree, but a relative filesystem
-- path works just as well. These are either constructed from individual name
-- components (using "appendPath", "catPaths" and "makeName"), or converted
-- from a FilePath ("floatPath" -- but take care when doing that) or .
newtype AnchoredPath = AnchoredPath [Name] deriving (Eq, Show, Ord)

-- | Check whether a path is a prefix of another path.
isPrefix :: AnchoredPath -> AnchoredPath -> Bool
(AnchoredPath a) `isPrefix` (AnchoredPath b) = a `isPrefixOf` b

-- | Append an element to the end of a path.
appendPath :: AnchoredPath -> Name -> AnchoredPath
appendPath (AnchoredPath p) n =
    case n of
      (Name s) | s == BS.empty -> AnchoredPath p
               | s == BS.pack "." -> AnchoredPath p
               | otherwise -> AnchoredPath $ p ++ [n]

-- | Catenate two paths together. Not very safe, but sometimes useful
-- (e.g. when you are representing paths relative to a different point than a
-- Tree root).
catPaths :: AnchoredPath -> AnchoredPath -> AnchoredPath
catPaths (AnchoredPath p) (AnchoredPath n) = AnchoredPath $ p ++ n

-- | Get parent (path) of a given path. foo/bar/baz -> foo/bar
parent :: AnchoredPath -> AnchoredPath
parent (AnchoredPath x) = AnchoredPath (init x)

-- | List all parents of a given path. foo/bar/baz -> [foo, foo/bar]
parents :: AnchoredPath -> [AnchoredPath]
parents (AnchoredPath x) = map AnchoredPath . init . inits $ x

-- | Take a "root" directory and an anchored path and produce a full
-- 'FilePath'. Moreover, you can use @anchorPath \"\"@ to get a relative
-- 'FilePath'.
anchorPath :: FilePath -> AnchoredPath -> FilePath
anchorPath dir p = dir </> BS.unpack (flatten p)
{-# INLINE anchorPath #-}

-- | Unsafe. Only ever use on bytestrings that came from flatten on a
-- pre-existing AnchoredPath.
floatBS :: BS.ByteString -> AnchoredPath
floatBS = AnchoredPath . map Name . takeWhile (not . BS.null) . BS.split '/'

flatten :: AnchoredPath -> BS.ByteString
flatten (AnchoredPath []) = BS.singleton '.'
flatten (AnchoredPath p) = BS.intercalate (BS.singleton '/')
                                           [ n | (Name n) <- p ]

makeName :: String -> Name
makeName ".." = error ".. is not a valid AnchoredPath component name"
makeName n | '/' `elem` n = error "/ may not occur in a valid AnchoredPath component name"
           | otherwise = Name $ BS.pack n

-- | Take a relative FilePath and turn it into an AnchoredPath. The operation
-- is (relatively) unsafe. Basically, by using floatPath, you are testifying
-- that the argument is a path relative to some common root -- i.e. the root of
-- the associated "Tree" object. Also, there are certain invariants about
-- AnchoredPath that this function tries hard to preserve, but probably cannot
-- guarantee (i.e. this is a best-effort thing). You should sanitize any
-- FilePaths before you declare them "good" by converting into AnchoredPath
-- (using this function).
floatPath :: FilePath -> AnchoredPath
floatPath = make . splitDirectories . normalise . dropTrailingPathSeparator
  where make ["."] = AnchoredPath []
        make x = AnchoredPath $ map (Name . BS.pack) x


anchoredRoot :: AnchoredPath
anchoredRoot = AnchoredPath []

-- | Take a prefix path, the changed prefix path, and a path to change.
-- Assumes the prefix path is a valid prefix. If prefix is wrong return
-- AnchoredPath [].
replacePrefixPath :: AnchoredPath -> AnchoredPath -> AnchoredPath -> AnchoredPath
replacePrefixPath (AnchoredPath []) b c = catPaths b c
replacePrefixPath (AnchoredPath (r:p)) b (AnchoredPath (r':p'))
    | r == r' = replacePrefixPath (AnchoredPath p) b (AnchoredPath p')
    | otherwise = AnchoredPath []
replacePrefixPath _ _ _ = AnchoredPath []

-- | Append a ByteString to the last Name of an AnchoredPath.
appendToName :: AnchoredPath -> String -> AnchoredPath
appendToName (AnchoredPath p) s = AnchoredPath (init p++[Name finalname])
    where suffix = BS.pack s
          finalname | suffix `elem` (BS.tails lastname) = lastname
                    | otherwise = BS.append lastname suffix
          lastname = case last p of
                        Name name -> name
