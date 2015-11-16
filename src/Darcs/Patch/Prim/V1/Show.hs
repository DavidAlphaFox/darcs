{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.Prim.V1.Show
    ( showHunk )
    where

import Prelude hiding ( pi )

import Darcs.Util.ByteString ( fromPS2Hex )
import qualified Data.ByteString as B (ByteString, length, take, drop)
import qualified Data.ByteString.Char8 as BC (head)

import Storage.Hashed.Tree( Tree )

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..), showFileHunk )
import Darcs.Util.Path ( FileName )
import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), formatFileName )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Viewing ( showContextHunk )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( PrimShow(..) )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Patch.Witnesses.Show ( Show1(..), Show2(..), ShowDict(..) )
import Darcs.Util.Printer ( Doc, vcat,
                 text, userchunk, invisibleText, invisiblePS, blueText,
                 ($$), (<+>), (<>),
               )
import Darcs.Util.Show ( appPrec, BSWrapper(..) )


-- TODO this instance shouldn't really be necessary, as Prims aren't used generically
instance PatchListFormat Prim

instance ShowPatchBasic Prim where
    showPatch = showPrim OldFormat

instance (ApplyState Prim ~ Tree) => ShowPatch Prim where
    showContextPatch (isHunk -> Just fh) = showContextHunk fh
    showContextPatch p = return $ showPatch p
    summary = plainSummaryPrim
    summaryFL = plainSummaryPrims
    thing _ = "change"

instance Show (Prim wX wY) where
    showsPrec d (Move fn1 fn2) = showParen (d > appPrec) $ showString "Move " .
                                 showsPrec (appPrec + 1) fn1 . showString " " .
                                 showsPrec (appPrec + 1) fn2
    showsPrec d (DP fn dp) = showParen (d > appPrec) $ showString "DP " .
                             showsPrec (appPrec + 1) fn . showString " " .
                             showsPrec (appPrec + 1) dp
    showsPrec d (FP fn fp) = showParen (d > appPrec) $ showString "FP " .
                             showsPrec (appPrec + 1) fn . showString " " .
                             showsPrec (appPrec + 1) fp
    showsPrec d (ChangePref p f t) = showParen (d > appPrec) $ showString "ChangePref " .
                                     showsPrec (appPrec + 1) p . showString " " .
                                     showsPrec (appPrec + 1) f . showString " " .
                                     showsPrec (appPrec + 1) t

instance Show2 Prim where
   showDict2 = ShowDictClass

instance Show1 (Prim wX) where
   showDict1 = ShowDictClass

instance Show (FilePatchType wX wY) where
    showsPrec _ RmFile = showString "RmFile"
    showsPrec _ AddFile = showString "AddFile"
    showsPrec d (Hunk line old new) | all ((==1) . B.length) old && all ((==1) . B.length) new
        = showParen (d > appPrec) $ showString "Hunk " .
                                      showsPrec (appPrec + 1) line . showString " " .
                                      showsPrecC old . showString " " .
                                      showsPrecC new
       where showsPrecC [] = showString "[]"
             showsPrecC ss = showParen True $ showString "packStringLetters " . showsPrec (appPrec + 1) (map BC.head ss)
    showsPrec d (Hunk line old new) = showParen (d > appPrec) $ showString "Hunk " .
                                      showsPrec (appPrec + 1) line . showString " " .
                                      showsPrec (appPrec + 1) (map BSWrapper old) . showString " " .
                                      showsPrec (appPrec + 1) (map BSWrapper new)
    showsPrec d (TokReplace t old new) = showParen (d > appPrec) $ showString "TokReplace " .
                                         showsPrec (appPrec + 1) t . showString " " .
                                         showsPrec (appPrec + 1) old . showString " " .
                                         showsPrec (appPrec + 1) new
    -- this case may not work usefully
    showsPrec d (Binary old new) = showParen (d > appPrec) $ showString "Binary " .
                                   showsPrec (appPrec + 1) (BSWrapper old) . showString " " .
                                   showsPrec (appPrec + 1) (BSWrapper new)

instance Show (DirPatchType wX wY) where
    showsPrec _ RmDir = showString "RmDir"
    showsPrec _ AddDir = showString "AddDir"

instance PrimShow Prim where
  showPrim x (FP f AddFile) = showAddFile x f
  showPrim x (FP f RmFile)  = showRmFile x f
  showPrim x (FP f (Hunk line old new))  = showHunk x f line old new
  showPrim x (FP f (TokReplace t old new))  = showTok x f t old new
  showPrim x (FP f (Binary old new))  = showBinary x f old new
  showPrim x (DP d AddDir) = showAddDir x d
  showPrim x (DP d RmDir)  = showRmDir x d
  showPrim x (Move f f') = showMove x f f'
  showPrim _ (ChangePref p f t) = showChangePref p f t

showAddFile :: FileNameFormat -> FileName -> Doc
showAddFile x f = blueText "addfile" <+> formatFileName x f

showRmFile :: FileNameFormat -> FileName -> Doc
showRmFile x f = blueText "rmfile" <+> formatFileName x f

showMove :: FileNameFormat -> FileName -> FileName -> Doc
showMove x d d' = blueText "move" <+> formatFileName x d <+> formatFileName x d'

showChangePref :: String -> String -> String -> Doc
showChangePref p f t = blueText "changepref" <+> text p
                    $$ userchunk f
                    $$ userchunk t

showAddDir :: FileNameFormat -> FileName -> Doc
showAddDir x d = blueText "adddir" <+> formatFileName x d

showRmDir :: FileNameFormat -> FileName -> Doc
showRmDir x d = blueText "rmdir" <+> formatFileName x d

showHunk :: FileNameFormat -> FileName -> Int -> [B.ByteString] -> [B.ByteString] -> Doc
showHunk x f line old new = showFileHunk x (FileHunk f line old new)

showTok :: FileNameFormat -> FileName -> String -> String -> String -> Doc
showTok x f t o n = blueText "replace" <+> formatFileName x f
                                     <+> text "[" <> userchunk t <> text "]"
                                     <+> userchunk o
                                     <+> userchunk n

showBinary :: FileNameFormat -> FileName -> B.ByteString -> B.ByteString -> Doc
showBinary x f o n =
    blueText "binary" <+> formatFileName x f
 $$ invisibleText "oldhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex o)
 $$ invisibleText "newhex"
 $$ vcat (map makeprintable $ breakEvery 78 $ fromPS2Hex n)
     where makeprintable ps = invisibleText "*" <> invisiblePS ps

breakEvery :: Int -> B.ByteString -> [B.ByteString]
breakEvery n ps | B.length ps < n = [ps]
                 | otherwise = B.take n ps : breakEvery n (B.drop n ps)
