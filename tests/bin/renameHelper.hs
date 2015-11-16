-- Helper to test renaming
--
-- Copyright (C) 2014 Owen Stephens
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
module Main where

import Data.List ( sort, groupBy )
import Data.Function ( on )
import System.FilePath ( (</>) )
import Control.Monad ( forM, void, when, unless, forM_ )
import System.Directory ( createDirectory, removeFile, removeDirectory
                        , doesDirectoryExist, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.Process ( system )

{-
N = nonexistent (unrecorded, not in working)
U = unadded     (unrecorded, in working)
S = shadow      (recorded, not in working)
K = known       (recorded, in working)

O = OK
F = Fail
           tgt
      N   U   S   K
   N  F1  F1  F1  F1
s  U  F2  F2  F2  F2
r  S  F3  O4  F5  O6
c  K  O7  F8  O9  OF10

Reasons:
 1    no such source path
 2    shouldn't move paths we don't manage
 3    target not in WD/Repo
 4    post-hoc move
 5    target exists in Repo
 6    target exists in WD/Repo, but could be successful post-hoc if target
      is made to appear deleted before the move
 7    simple rename
 8    target in WD
 9    rename into file that has been deleted.
 10   If target is dir then OK: move into dir, else Fail: target exists in WD
-}

data PathExistence = N
                   | U
                   | K
                   | S
                   deriving (Eq, Ord, Show)

data PathType = File
              | Dir
              deriving (Eq, Ord, Show )

-- OK takes a function to modify src/tgt names, used to check for tgt existence
data ExpectedResult = OK ((String, String) -> String)
                    | Fail

instance Show ExpectedResult where
    show (OK _) = "OK <check>"
    show Fail = "FAIL"

-- ExpectedResults are for: [File/File, File/Dir, Dir/File, Dir/Dir]
resList :: [ (PathExistence, [(PathExistence, [ExpectedResult])]) ]
resList =
    [ (N, [ (N, allFail)
          , (U, allFail)
          , (K, allFail)
          , (S, allFail)
          ])
    , (U, [ (N, allFail)
          , (U, allFail)
          , (K, allFail)
          , (S, allFail)
          ])
    , (K, [ (N, allOkSnd)
          , (U, allFail)
          , (K, [Fail, okMoveInto, Fail, okMoveInto])
          , (S, allOkSnd)
          ])
    , (S, [ (N, allFail)
          , (U, [okSnd, Fail, Fail, okSnd])
          , (K, [okSnd, Fail, Fail, okSnd])
          , (S, allFail)
          ])
    ]
  where
    allFail = replicate 4 Fail
    allOkSnd = replicate 4 okSnd
    okSnd = OK snd
    okMoveInto = OK $ \(s,d) -> d</>s

-- Add a newline, else we're not creating a valid textfile!
makeAtPath File = \p -> writeFile p (p ++ "\n")
makeAtPath Dir = createDirectory
removeAtPath File p = do
    removeFile p
    void $ system ("echo 'Removed file " ++ p ++ "'")
removeAtPath Dir p = do
    removeDirectory p
    void $ system ("echo 'Removed dir " ++ p ++ "'")

expectFailure args ExitSuccess =
    fail $ "Unexpected success: " ++ show args
expectFailure args ec = return ()

exists File = doesFileExist
exists Dir = doesDirectoryExist

checkTgtExists pathType mod src tgt args = do
    tgtExists <- exists pathType $ mod (src, tgt)
    unless tgtExists $
        fail $ "Unexpected absence of move tgt: "
                ++ show (mod (src, tgt)) ++ " " ++ show args

checkSrcDoesNotExist pathType src args = do
    srcExists <- exists pathType src
    when srcExists $
        fail $ "Unexpected presence of move src: " ++ show args

expectSuccess args (ExitFailure _) _ =
    fail $ "Unexpected failure: " ++ show args
expectSuccess (U,_,_,_,_,_,_) ExitSuccess _ = return ()
expectSuccess args@(K, srcPathType@File, src, K, Dir, tgt, _) _ mod = do
    checkSrcDoesNotExist srcPathType src args
    -- src is a file, tgt is a dir, so src will be moved inside dir
    checkTgtExists File mod src tgt args
expectSuccess args@(_, srcPathType, src, N, _, tgt, _) _ mod = do
    checkSrcDoesNotExist srcPathType src args
    -- tgt didn't exist so src will simply be renamed to tgt
    checkTgtExists srcPathType mod src tgt args
expectSuccess args@(K, srcPathType, src, S, _, tgt, _) _ mod = do
    checkSrcDoesNotExist srcPathType src args
    -- tgt was shadow, so src will simply be renamed to tgt
    checkTgtExists srcPathType mod src tgt args
expectSuccess args@(_, srcPathType, src,_, tgtPathType, tgt,_) _ mod = do
    checkSrcDoesNotExist srcPathType src args
    checkTgtExists tgtPathType mod src tgt args

type RenameInfo = (PathExistence, PathType, FilePath)

main = do
    -- Don't need to do anything with the nonexistent paths
    let [_, us, ks, ss] = groupBy ((==) `on` (\(x,_,_) -> x)) . sort .
            concatMap (\(x,y) -> [x,y]) . fst . unzip $ pathDetails
    -- Create all files/dirs
    mapM_ (\(_, ptype, p) -> makeAtPath ptype p) $ us ++ ks ++ ss
    -- Let darcs know about all knowns/shadows
    mapM_ (system . ("darcs add " ++) . unwords . map (\(_,_,x)->x)) [ks, ss]
    _ <- system "darcs rec -am 'add everything'"
    -- Remove from working all shadows
    mapM_ (\(_, ptype, p) -> removeAtPath ptype p) ss
    forM_ pathDetails $ \(( (srcType, srcPathType, srcName)
                          , (tgtType, tgtPathType, tgtName)), expected) -> do
        let movePatchName =
                "'move " ++ srcName ++ " -> " ++ tgtName ++ "'"
        ec <- system $ unwords [ "darcs whatsnew"
                               , "&& darcs move", srcName, tgtName
                               , "&& darcs rec -a ", tgtName, srcName
                               , "-m",  movePatchName]
        let args = ( srcType, srcPathType, srcName
                   , tgtType, tgtPathType, tgtName, expected)
        case expected of
            OK check -> expectSuccess args ec check
            Fail -> expectFailure args ec
  where
    fileDirCombos = [(a,b) | a <- [File, Dir], b <- [File, Dir]]

    zipErr [] [] = []
    zipErr (x : xs) (y : ys) = (x, y) : zipErr xs ys
    zipErr _ _ = error "uneven lists in zipErr"

    pathDetails :: [((RenameInfo, RenameInfo), ExpectedResult)]
    pathDetails =
        concatFor resList $ \(srcType, srcResults) ->
            concatFor srcResults $ \(tgtType, expected) ->
                concatFor (zipErr fileDirCombos expected) $
                    \((srcPathType, tgtPathType), expectedResult) ->
                        let nameBase = show srcType ++ show srcPathType
                                        ++ show tgtType ++ show tgtPathType
                            srcName = nameBase ++ "-src"
                            tgtName = nameBase ++ "-dst"
                            src = (srcType, srcPathType, srcName)
                            tgt = (tgtType, tgtPathType, tgtName)
                        in [((src, tgt), expectedResult)]
      where
        concatFor = flip concatMap
