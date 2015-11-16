module Darcs.Util.Environment
    (
      maybeGetEnv
    ) where


import Prelude hiding ( catch )

import System.Environment ( getEnv )

import Darcs.Util.Exception ( catchall )

maybeGetEnv :: String
            -> IO (Maybe String)
maybeGetEnv s = fmap Just (getEnv s) `catchall` return Nothing -- err can only be isDoesNotExist
