{-# LANGUAGE CPP #-}
module Darcs.Test.Patch.Properties.Check ( Check(..), checkAPatch ) where

import Control.Monad ( liftM )

import Darcs.Test.Patch.Check ( PatchCheck,
                                checkMove, removeDir, createDir,
                                isValid, insertLine, fileEmpty, fileExists,
                                deleteLine, modifyFile, createFile, removeFile,
                                doCheck, FileContents(..)
                              )
import Darcs.Patch.RegChars ( regChars )
import Darcs.Util.ByteString ( linesPS )
import qualified Data.ByteString as B ( ByteString, null, concat )
import qualified Data.ByteString.Char8 as BC ( break, pack )
import Darcs.Util.Path ( fn2fp )
import qualified Data.Map as M ( mapMaybe )

import Darcs.Patch ( invert,
                     effect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.V1 ()
import qualified Darcs.Patch.V1.Core as V1 ( Patch(..) )
import Darcs.Patch.V1.Core ( isMerger )
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( Prim(..), DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.Witnesses.Ordered

#include "impossible.h"

class Check p where
   checkPatch :: p wX wY -> PatchCheck Bool

instance Check p => Check (FL p) where
   checkPatch NilFL = isValid
   checkPatch (p :>: ps) = checkPatch p >> checkPatch ps

checkAPatch :: (Invert p, Check p) => p wX wY -> Bool
checkAPatch p = doCheck $ do _ <- checkPatch p
                             checkPatch $ invert p

instance Check (V1.Patch Prim) where
   checkPatch p | isMerger p = do
     checkPatch $ effect p
   checkPatch (V1.Merger _ _ _ _) = impossible
   checkPatch (V1.Regrem _ _ _ _) = impossible
   checkPatch (V1.PP p) = checkPatch p

instance Check Prim where

   checkPatch (FP f RmFile) = removeFile $ fn2fp f
   checkPatch (FP f AddFile) =  createFile $ fn2fp f
   checkPatch (FP f (Hunk line old new)) = do
       _ <- fileExists $ fn2fp f
       mapM_ (deleteLine (fn2fp f) line) old
       mapM_ (insertLine (fn2fp f) line) (reverse new)
       isValid
   checkPatch (FP f (TokReplace t old new)) =
       modifyFile (fn2fp f) (tryTokPossibly t old new)
   -- note that the above isn't really a sure check, as it leaves PSomethings
   -- and PNothings which may have contained new...
   checkPatch (FP f (Binary o n)) = do
       _ <- fileExists $ fn2fp f
       mapM_ (deleteLine (fn2fp f) 1) (linesPS o)
       _ <- fileEmpty $ fn2fp f
       mapM_ (insertLine (fn2fp f) 1) (reverse $ linesPS n)
       isValid

   checkPatch (DP d AddDir) = createDir $ fn2fp d
   checkPatch (DP d RmDir) = removeDir $ fn2fp d

   checkPatch (Move f f') = checkMove (fn2fp f) (fn2fp f')
   checkPatch (ChangePref _ _ _) = return True

tryTokPossibly :: String -> String -> String
                -> (Maybe FileContents) -> (Maybe FileContents)
tryTokPossibly t o n = liftM $ \contents ->
        let lines' = M.mapMaybe (liftM B.concat
                                  . tryTokInternal t (BC.pack o) (BC.pack n))
                                (fcLines contents)
        in contents { fcLines = lines' }

tryTokInternal :: String -> B.ByteString -> B.ByteString
                 -> B.ByteString -> Maybe [B.ByteString]
tryTokInternal _ _ _ s | B.null s = Just []
tryTokInternal t o n s =
    case BC.break (regChars t) s of
    (before,s') ->
        case BC.break (not . regChars t) s' of
        (tok,after) ->
            case tryTokInternal t o n after of
            Nothing -> Nothing
            Just rest ->
                if tok == o
                then Just $ before : n : rest
                else if tok == n
                     then Nothing
                     else Just $ before : tok : rest
