{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE CPP, OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses #-}

-- Copyright (C) 2010 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |
-- Module      : Darcs.Patch.Annotate
-- Copyright   : 2010 Petr Rockai
-- License     : MIT
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Annotate
    (
      annotate
    , annotateDirectory
    , format
    , machineFormat
    , AnnotateResult
    ) where

import Prelude hiding ( pi )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Function ( on )
import Data.List( nub, groupBy )
import Data.Maybe( isJust, mapMaybe )

import Control.Monad.State ( modify, when, gets, State, execState )
import Control.Applicative( (<$>) )

import Darcs.Patch.ApplyMonad( ApplyMonad(..) )
import Darcs.Patch.Apply ( Apply, apply, ApplyState )
import Darcs.Patch.Info ( PatchInfo(..), showPatchInfoUI, piAuthor, makePatchname )
import Darcs.Patch.PatchInfoAnd( info, PatchInfoAnd )
import Darcs.Patch.Witnesses.Ordered

import Storage.Hashed.Tree( Tree )
import Darcs.Util.Path ( FileName, movedirfilename, fn2ps, ps2fn )
import Darcs.Util.Printer( renderString, RenderMode(..) )
import Darcs.Util.ByteString ( linesPS, unlinesPS )
import Darcs.Util.Diff ( getChanges )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )

#include "impossible.h"


data FileOrDirectory = File
                     | Directory
                       deriving (Show, Eq)

data Annotated = Annotated
    { annotated     :: V.Vector (Maybe PatchInfo, B.ByteString)
    , current       :: [(Int, B.ByteString)]
    , path          :: Maybe FileName
    , what          :: FileOrDirectory
    , currentInfo   :: PatchInfo
    , diffAlgorithm :: D.DiffAlgorithm
    } deriving Show

type AnnotateResult = V.Vector (Maybe PatchInfo, B.ByteString)

type AnnotatedM = State Annotated

-- XXX: No explicit method nor default method for 'editFile', 'editDirectory'
instance ApplyMonad AnnotatedM Tree where
  type ApplyMonadBase AnnotatedM = AnnotatedM

  nestedApply _ _ = undefinedFun "nestedApply"
  liftApply _ _   = undefinedFun "liftApply"
  getApplyState   = undefinedFun "getApplyState"
  putApplyState _ = undefinedFun "putApplyState"
  mReadFilePS     = undefinedFun "mReadFilePS"

  mDoesFileExist _      = return True
  mDoesDirectoryExist _ = return True
  mCreateDirectory _    = return ()
  mCreateFile _         = return ()

  mRemoveFile f = do
      p <- gets path
      when (p == Just f) $ modify (\x -> x { path = Nothing })
      updateDirectory f

  mRemoveDirectory = mRemoveFile

  mRename a b = do
      p <- gets path
      w <- gets what
      when (isJust p) $
          modify $ \st -> st { path = Just $ movedirfilename a b (fromJust p) }
      when (w == Directory) $ do
          let fix (i, x) = (i, fn2ps $ movedirfilename a b (ps2fn x))
          modify $ \st -> st { current = map fix $ current st }

  mModifyFilePS f job = do
      p <- gets path
      when (p == Just f) $ updateFile (fmap linesPS . job . unlinesPS)

  mModifyFilePSs f job = do
      p <- gets path
      when (p == Just f) $ updateFile job

undefinedFun :: Monad m
             => String
             -> m a
undefinedFun name = fail $ name ++ " undefined for Annotated"


updateFile :: ([B.ByteString]
           -> AnnotatedM [B.ByteString])
           -> AnnotatedM ()
updateFile job = (==File) <$> gets what >>= flip when go
  where
    go = do
        before <- map snd `fmap` gets current
        after <- job before
        da <- gets diffAlgorithm
        reannotate $ getChanges da before after

    reannotate [] = return ()
    reannotate ((off, remove, add):rest) = do
        i <- gets currentInfo
        c <- gets current
        a <- gets annotated
        modify $ \s -> s { current = take off c ++ [ (-1, x) | x <- add ] ++
                                     drop (off + length remove) c
                         , annotated = merge i a $ take (length remove) $ drop off c
                         }
        reannotate rest

    merge i a l = a V.// [ (line, (Just i, B.empty))
                         | (line, _) <- l, line >= 0 && line < V.length a]


updateDirectory :: FileName -> AnnotatedM ()
updateDirectory p = (==Directory) <$> gets what >>= flip when go
  where
    go = do let line = fn2ps p
            files <- gets current
            case filter ((==line) . snd) files of
              [match@(ident, _)] -> reannotate ident match line
              _ -> return ()
    reannotate ident match line =
      modify $ \x -> x { annotated = annotated x V.// [ (ident, update line $ currentInfo x) ]
                       , current = filter (/= match) $ current x }
    update line inf = (Just inf, BC.concat [ " -- created as: ", line ])


complete :: Annotated -> Bool
complete x = V.all (isJust . fst) $ annotated x


annotate' :: (Apply p, ApplyState p ~ Tree)
          => FL (PatchInfoAnd p) wX wY
          -> Annotated
          -> Annotated
annotate' NilFL ann = ann
annotate' (p :>: ps) ann
    | complete ann = ann
    | otherwise = annotate' ps $ execState (apply p) (ann { currentInfo = info p })


annotate :: (Apply p, ApplyState p ~ Tree)
         => D.DiffAlgorithm
         -> FL (PatchInfoAnd p) wX wY
         -> FileName
         -> B.ByteString
         -> AnnotateResult
annotate da patches inipath inicontent = annotated $ annotate' patches initial
  where
    initial = Annotated { path = Just inipath
                        , currentInfo = error "There is no currentInfo."
                        , current = zip [0..] (linesPS inicontent)
                        , what = File
                        , annotated = V.replicate (length $ breakLines inicontent)
                                                      (Nothing, B.empty)
                        , diffAlgorithm = da
                        }


annotateDirectory :: (Apply p, ApplyState p ~ Tree)
                  => D.DiffAlgorithm
                  -> FL (PatchInfoAnd p) wX wY
                  -> FileName
                  -> [FileName]
                  -> AnnotateResult
annotateDirectory da patches inipath inicontent = annotated $ annotate' patches initial
  where
    initial = Annotated { path = Just inipath
                        , currentInfo = error "There is no currentInfo."
                        , current = zip [0..] (map fn2ps inicontent)
                        , what = Directory
                        , annotated = V.replicate (length inicontent) (Nothing, B.empty)
                        , diffAlgorithm = da
                        }


machineFormat :: B.ByteString -> AnnotateResult -> String
machineFormat d a = unlines [ case i of
                                 Just inf -> show $ makePatchname inf
                                 Nothing -> -- make unknowns uniform, for easier parsing
                                   take 40 ( repeat '0' ) -- fake hash of the right size
                              ++ " | " ++ BC.unpack line ++ " " ++ BC.unpack add
                            | ((i, add), line) <- zip (V.toList a) (breakLines d) ]


format :: B.ByteString
       -> AnnotateResult
       -> String
format d a = pi_list ++ "\n" ++ numbered
  where
    numberedLines = zip [(1 :: Int)..] . lines $ file

    prependNum (lnum, annLine) =
        let maxDigits = length . show . length $ numberedLines
            lnumStr = show lnum
            paddingNum = maxDigits - length lnumStr
        in replicate paddingNum ' ' ++ lnumStr ++ ": " ++ annLine

    numbered = unlines . map prependNum $ numberedLines

    pi_list = unlines [ show n ++ ": " ++ renderString Encode (showPatchInfoUI i)
                      | (n :: Int, i) <- zip [1..] pis ]

    file = concat [ annotation (fst $ head chunk) ++ " | " ++ line (head chunk) ++
                    "\n" ++ unlines [ indent 25 (" | " ++ line l) | l <- tail chunk ]
                  | chunk <- file_ann ]

    pis = nub $ mapMaybe fst $ V.toList a

    pi_map = M.fromList (zip pis [1 :: Int ..])

    file_ann = groupBy ((==) `on` fst) $ zip (V.toList a) (breakLines d)

    line ((_, add), l) = BC.unpack $ BC.concat [l, " ", add]

    annotation (Just i, _) | Just n <- M.lookup i pi_map =
        pad 20 (piMail i) ++ " " ++ pad 4 ('#' : show n)
    annotation _ = pad 25 "unknown"

    pad n str = replicate (n - length str) ' ' ++ take n str

    indent n str = replicate n ' ' ++ str

    piMail pi
        | '<' `elem` piAuthor pi = takeWhile (/= '>') . drop 1 . dropWhile (/= '<') $ piAuthor pi
        | otherwise = piAuthor pi


breakLines :: BC.ByteString
           -> [BC.ByteString]
breakLines s = case BC.split '\n' s of
    [] -> []
    split | BC.null (last split) -> init split
          | otherwise -> split

