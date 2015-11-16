{-# Language MultiParamTypeClasses, DeriveDataTypeable #-}
module Darcs.Util.Exception
    ( firstJustIO
    , catchall
    , clarifyErrors
    , prettyException
    , prettyError
    ) where


import Prelude hiding ( catch )

import Control.Exception ( SomeException, Exception(fromException), catch )
import Data.Maybe ( isJust )

import System.IO.Error ( isUserError, ioeGetErrorString
                       , isDoesNotExistError, ioeGetFileName )

import Darcs.Util.SignalHandler ( catchNonSignal )

catchall :: IO a
         -> IO a
         -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)

-- | The firstJustM returns the first Just entry in a list of monadic
-- operations. This is close to `listToMaybe `fmap` sequence`, but the sequence
-- operator evaluates all monadic members of the list before passing it along
-- (i.e. sequence is strict). The firstJustM is lazy in that list member monads
-- are only evaluated up to the point where the first Just entry is obtained.
firstJustM :: Monad m
           => [m (Maybe a)]
           -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (e:es) = e >>= (\v -> if isJust v then return v else firstJustM es)


-- | The firstJustIO is a slight modification to firstJustM: the entries in the
-- list must be IO monad operations and the firstJustIO will silently turn any
-- monad call that throws an exception into Nothing, basically causing it to be
-- ignored.
firstJustIO :: [IO (Maybe a)]
            -> IO (Maybe a)
firstJustIO = firstJustM . map (`catchall` return Nothing)


clarifyErrors :: IO a
              -> String
              -> IO a
clarifyErrors a e = a `catch` (\x -> fail $ unlines [prettyException x,e])

prettyException :: SomeException
                -> String
prettyException e | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
prettyException e | Just ioe <- fromException e, isDoesNotExistError ioe =
  case ioeGetFileName ioe of
    Just f  -> f ++ " does not exist"
    Nothing -> show e
prettyException e = show e


prettyError :: IOError -> String
prettyError e | isUserError e = ioeGetErrorString e
              | otherwise = show e
