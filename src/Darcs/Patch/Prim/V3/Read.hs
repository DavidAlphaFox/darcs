{-# LANGUAGE CPP, ViewPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V3.Read () where
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.ReadMonads
import Darcs.Patch.Prim.Class( PrimRead(..) )
import Darcs.Patch.Prim.V3.Core( Prim(..), Hunk(..) )
import Darcs.Patch.Prim.V3.ObjectMap
import Darcs.Patch.Witnesses.Sealed( seal )

import Control.Applicative ( (<$>) )
import Control.Monad ( liftM, liftM2 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char ( chr )

#include "impossible.h"

instance PrimRead Prim where
  readPrim _ = do skipSpace
                  choice $ map (liftM seal) [
                    identity,
                    hunk "hunk" TextHunk,
                    hunk "binhunk" BinaryHunk,
                    manifest "manifest" Manifest,
                    manifest "demanifest" Demanifest ]

    where manifest kind ctor = liftM2 ctor (patch kind) location
          identity = lexString "identity" >> return Identity
          patch x = string x >> uuid
          uuid = UUID <$> myLex'
          filename = encoded
          encoded = decodeWhite <$> myLex'
          hunktext = skipSpace >> choice [ string "." >> encoded, string "!" >> return B.empty ]
          location = liftM2 (,) uuid filename
          hunk kind ctor = do uid <- patch kind
                              offset <- int
                              old <- hunktext
                              new <- hunktext
                              return $ ctor uid (Hunk offset old new)

instance ReadPatch Prim where
 readPatch' = readPrim undefined

-- XXX a bytestring version of decodeWhite from Darcs.FileName
decodeWhite :: B.ByteString -> B.ByteString
decodeWhite (BC.uncons -> Just ('\\', cs)) =
    case BC.break (=='\\') cs of
    (theord, BC.uncons -> Just ('\\', rest)) ->
        chr (read $ BC.unpack theord) `BC.cons` decodeWhite rest
    _ -> error "malformed filename"
decodeWhite (BC.uncons -> Just (c, cs)) = c `BC.cons` decodeWhite cs
decodeWhite (BC.uncons -> Nothing) = BC.empty
decodeWhite _ = impossible

