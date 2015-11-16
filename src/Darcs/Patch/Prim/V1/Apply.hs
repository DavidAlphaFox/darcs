{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
module Darcs.Patch.Prim.V1.Apply () where

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )

import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..),
      DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.Prim.V1.Show ( showHunk )

import Darcs.Util.Path ( fn2fp )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.TokenReplace ( tryTokInternal )

import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Storage.Hashed.Tree( Tree )

import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, spanFL, (:>)(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePStart )

import Darcs.Util.ByteString ( unlinesPS, breakAfterNthNewline, breakBeforeNthNewline, )
import Darcs.Util.Printer( renderString, RenderMode(..) )

import qualified Data.ByteString as B ( ByteString, empty, null, concat )
import qualified Data.ByteString.Char8 as BC (pack, singleton, unpack)
import Data.List ( intersperse )

#include "impossible.h"

type FileContents = B.ByteString

instance Apply Prim where
    type ApplyState Prim = Tree
    apply (FP f RmFile) = mRemoveFile f
    apply (FP f AddFile) = mCreateFile f
    apply p@(FP _ (Hunk{})) = applyPrimFL (p :>: NilFL)
    apply (FP f (TokReplace t o n)) = mModifyFilePSs f doreplace
        where doreplace ls =
                  case mapM (tryTokInternal t (BC.pack o) (BC.pack n)) ls of
                  Nothing -> fail $ "replace patch to " ++ fn2fp f
                             ++ " couldn't apply."
                  Just ls' -> return $ map B.concat ls'
    apply (FP f (Binary o n)) = mModifyFilePS f doapply
        where doapply oldf = if o == oldf
                             then return n
                             else fail $ "binary patch to " ++ fn2fp f
                                  ++ " couldn't apply."
    apply (DP d AddDir) = mCreateDirectory d
    apply (DP d RmDir) = mRemoveDirectory d
    apply (Move f f') = mRename f f'
    apply (ChangePref p f t) = mChangePref p f t

instance RepairToFL Prim where
    applyAndTryToFixFL (FP f RmFile) =
        do x <- mReadFilePS f
           mRemoveFile f
           return $ if B.null x
                        then Nothing
                        else Just ("WARNING: Fixing removal of non-empty file "++fn2fp f,
                                   -- No need to coerce because the content
                                   -- removal patch has freely decided contexts
                                   FP f (Binary x B.empty) :>: FP f RmFile :>: NilFL )
    applyAndTryToFixFL (FP f AddFile) =
        do exists <- mDoesFileExist f
           if exists
             then return $
                     Just ("WARNING: Dropping add of existing file "++fn2fp f,
                           -- the old context was wrong, so we have to coerce
                           unsafeCoercePStart NilFL
                          )
             else do mCreateFile f
                     return Nothing
    applyAndTryToFixFL (DP f AddDir) =
        do exists <- mDoesDirectoryExist f
           if exists
             then return $
                     Just ("WARNING: Dropping add of existing directory "++fn2fp f,
                           -- the old context was wrong, so we have to coerce
                           unsafeCoercePStart NilFL
                          )
             else do mCreateDirectory f
                     return Nothing
    applyAndTryToFixFL p = do apply p; return Nothing

instance PrimApply Prim where
    applyPrimFL NilFL = return ()
    applyPrimFL (FP f h@(Hunk{}):>:the_ps)
     = case spanFL f_hunk the_ps of
           (xs :> ps') ->
               do let foo = h :>: mapFL_FL (\(FP _ h') -> h') xs
                  mModifyFilePS f $ hunkmod foo
                  applyPrimFL ps'
        where f_hunk (FP f' (Hunk{})) | f == f' = True
              f_hunk _ = False
              hunkmod :: ApplyMonad m Tree => FL FilePatchType wX wY
                      -> B.ByteString -> m B.ByteString
              hunkmod NilFL ps = return ps
              hunkmod (Hunk line old new:>:hs) ps
               = case applyHunkLines [(line,old,new)] ps of
                     Just ps' -> hunkmod hs ps'
                     Nothing -> fail $ "### Error applying:\n" ++
                                       renderString Encode
                                             (showHunk NewFormat f line old new) ++
                                       "\n### to file " ++ fn2fp f ++ ":\n" ++ BC.unpack ps
              hunkmod _ _ = impossible
    applyPrimFL (p:>:ps) = do apply p
                              applyPrimFL ps

applyHunks :: [(Int, [B.ByteString], [B.ByteString])]
           -> B.ByteString -> Maybe [B.ByteString]
applyHunks [] ps = Just [ps]
applyHunks ((l, [], n):hs) ps
    = case breakBeforeNthNewline (l - 2) ps of
      (prfix, after_prefix) -> do rest <- applyHunks hs after_prefix
                                  return $ intersperse nl (prfix:n) ++ rest
                                       where nl = BC.singleton '\n'
applyHunks ((l, o, n):hs) ps
    = case breakBeforeNthNewline (l - 2) ps of
      (prfix, after_prefix) ->
          case breakBeforeNthNewline (length o) after_prefix of
          (oo, _) | oo /= unlinesPS (B.empty:o) -> fail "applyHunks error"
          (_, suffix) ->
              do rest <- applyHunks hs suffix
                 return $ intersperse nl (prfix:n) ++ rest
    where nl = BC.singleton '\n'

applyHunkLines :: [(Int, [B.ByteString], [B.ByteString])]
               -> FileContents -> Maybe FileContents
applyHunkLines [] c = Just c
applyHunkLines [(1, [], n)] ps | B.null ps = Just $ unlinesPS (n++[B.empty])
applyHunkLines hs@((l, o, n):hs') ps =
 do pss <- case l of
           1 -> case breakAfterNthNewline (length o) ps of
                Nothing -> if ps == unlinesPS o
                           then return $ intersperse nl n
                           else fail "applyHunkLines: Unexpected hunks"
                Just (shouldbeo, suffix)
                    | shouldbeo /= unlinesPS (o++[B.empty]) ->
                        fail "applyHunkLines: Bad patch!"
                    | null n ->
                        do x <- applyHunkLines hs' suffix
                           return [x]
                    | otherwise ->
                        do rest <- applyHunks hs' suffix
                           return $ intersperse nl n ++ nl:rest
           _ | l < 0 -> bug "Prim.applyHunkLines: After -ve lines?"
             | otherwise -> applyHunks hs ps
    let result = B.concat pss
    return result
    where nl = BC.singleton '\n'
