module Darcs.Patch.Inspect
       ( PatchInspect(..)
       )
       where

import Darcs.Patch.Witnesses.Ordered ( FL, RL, reverseRL, mapFL )

import qualified Data.ByteString.Char8 as BC
import Data.List ( nub )


class PatchInspect p where
    listTouchedFiles :: p wX wY -> [FilePath]
    hunkMatches :: (BC.ByteString -> Bool) -> p wX wY -> Bool

instance PatchInspect p => PatchInspect (FL p) where
    listTouchedFiles xs = nub $ concat $ mapFL listTouchedFiles xs
    hunkMatches f = or . mapFL (hunkMatches f)

instance PatchInspect p => PatchInspect (RL p) where
    listTouchedFiles = listTouchedFiles . reverseRL
    hunkMatches f = hunkMatches f . reverseRL

