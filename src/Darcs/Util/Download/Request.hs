module Darcs.Util.Download.Request
    ( UrlRequest(..)
    , Cachable(..)
    , UrlState(..)
    , Q(..)
    , readQ
    , insertQ
    , pushQ
    , addUsingPriority
    , deleteQ
    , elemQ
    , emptyQ
    , nullQ
    , Priority(..)
    , ConnectionError(..)
    ) where

import Data.List ( delete )
import Data.Map ( Map )
import Foreign.C.Types ( CInt )

data Priority = High
              | Low
              deriving Eq

data Cachable = Cachable
              | Uncachable
              | MaxAge !CInt
              deriving (Show, Eq)

-- | A UrlRequest object contains a url to get, the file into which the
-- contents at the given url should be written, the cachability of this request
-- and the request's priority.
data UrlRequest = UrlRequest
    { url :: String
    , file :: FilePath
    , cachable :: Cachable
    , priority :: Priority
    }

type InProgressStatus = ( FilePath -- FilePath to write url contents into
                        , [FilePath] -- Extra paths to copy complete file into
                        , Cachable -- Cachable status
                        )

-- | A UrlState object contains a map of url -> InProgressStatus, a Q of urls
-- waiting to be started, the current pipe length and the unique junk to
-- create unique filenames.
data UrlState = UrlState
    { inProgress :: Map String InProgressStatus
    , waitToStart :: Q String
    , pipeLength :: Int
    , randomJunk :: String
    }

-- |Q represents a prioritised queue, with two-tier priority. The left list
-- contains higher priority items than the right list.
data Q a = Q [a] [a]

-- |'readQ' will try and take an element from the Q, preferring elements from
-- the high priority list.
readQ :: Q a -> Maybe (a, Q a)
readQ (Q (x : xs) ys) = return (x, Q xs ys)
readQ (Q [] ys) = do
    x : xs <- return $ reverse ys
    return (x, Q xs [])

-- | Return a function for adding an element based on the priority.
addUsingPriority :: Priority -> a -> Q a -> Q a
addUsingPriority High = pushQ
addUsingPriority Low = insertQ

-- |'insertQ' inserts a low priority item into a Q.
insertQ :: a -> Q a -> Q a
insertQ y (Q xs ys) = Q xs (y:ys)

-- |'pushQ' inserts a high priority item into a Q.
pushQ :: a -> Q a -> Q a
pushQ x (Q xs ys) = Q (x:xs) ys

-- |'deleteQ' removes any instances of a given element from the Q.
deleteQ :: Eq a => a -> Q a -> Q a
deleteQ x (Q xs ys) = Q (delete x xs) (delete x ys)

-- |'deleteQ' checks for membership in a Q.
elemQ :: Eq a => a -> Q a -> Bool
elemQ x (Q xs ys) = x `elem` xs || x `elem` ys

-- |'emptyQ' is an empty Q.
emptyQ :: Q a
emptyQ = Q [] []

-- |'nullQ' checks if the Q contains no items.
nullQ :: Q a -> Bool
nullQ (Q [] []) = True
nullQ _         = False

-- | Data type to represent a connection error.
-- The following are the codes from libcurl
-- which map to each of the constructors:
-- * 6  -> CouldNotResolveHost : The remote host was not resolved.
-- * 7  -> CouldNotConnectToServer : Failed to connect() to host or proxy.
-- * 28 -> OperationTimeout: the specified time-out period was reached.
data ConnectionError = CouldNotResolveHost
                     | CouldNotConnectToServer
                     | OperationTimeout
                     deriving (Eq, Read, Show)
