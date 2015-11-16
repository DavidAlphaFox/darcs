module Darcs.Util.Diff
    ( getChanges
    , DiffAlgorithm(..)
    ) where

import qualified Darcs.Util.Diff.Myers as M ( getChanges )
import qualified Darcs.Util.Diff.Patience as P ( getChanges )
import qualified Data.ByteString as B ( ByteString )


data DiffAlgorithm = PatienceDiff | MyersDiff
    deriving ( Eq, Show )

getChanges :: DiffAlgorithm -> [B.ByteString] -> [B.ByteString]
              -> [(Int,[B.ByteString],[B.ByteString])]
getChanges dac = case dac of
                   PatienceDiff -> P.getChanges
                   MyersDiff -> M.getChanges