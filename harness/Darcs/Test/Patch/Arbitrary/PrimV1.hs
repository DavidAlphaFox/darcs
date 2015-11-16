{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Test.Patch.Arbitrary.PrimV1 where

import qualified Darcs.Test.Patch.Arbitrary.Generic as T
     ( commuteTripleFromTree, commutePairFromTree, commutePairFromTWFP
     , mergePairFromTree, mergePairFromTWFP
     , patchFromTree )
import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.RepoModel

import Control.Monad ( liftM )
import Test.QuickCheck
import Darcs.Test.Patch.WithState
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( FilePatchType( Hunk, TokReplace ), Prim( FP ), isIdentity )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.FileHunk( IsHunk( isHunk ), FileHunk(..) )

import Darcs.Test.Patch.V1Model
import Darcs.Test.Util.QuickCheck ( alpha, notIn, maybeOf )

import Darcs.UI.Commands.Replace ( defaultToks )
import Darcs.Patch.Prim

import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( isJust )

#include "impossible.h"

patchFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . p wY wZ -> t) -> WithStartState V1Model (Tree Prim) wX -> t
patchFromTree = T.patchFromTree

mergePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . (p :\/: p) wY wZ -> t) -> WithStartState V1Model (Tree Prim) wX -> t
mergePairFromTree = T.mergePairFromTree

mergePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . (p :\/: p) wY wZ -> t) -> WithStartState V1Model (TreeWithFlattenPos Prim) wX -> t
mergePairFromTWFP = T.mergePairFromTWFP

commutePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . (p :> p) wY wZ -> t) -> WithStartState V1Model (TreeWithFlattenPos Prim) wX -> t
commutePairFromTWFP = T.commutePairFromTWFP

commutePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . (p :> p) wY wZ -> t) -> WithStartState V1Model (Tree Prim) wX -> t
commutePairFromTree = T.commutePairFromTree

commuteTripleFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (forall wY wZ . (p :> p :> p) wY wZ -> t) -> WithStartState V1Model (Tree Prim) wX -> t
commuteTripleFromTree = T.commuteTripleFromTree

nonEmptyHunk :: (IsHunk p) => p wX wY -> Bool
nonEmptyHunk p
  | Just (FileHunk _ _ [] []) <- isHunk p = False
  | otherwise                             = True

nonEmptyHunksPair :: (IsHunk p) => (p :> p) wX wY -> Bool
nonEmptyHunksPair (p1 :> p2) = nonEmptyHunk p1 && nonEmptyHunk p2

nonEmptyHunksTriple :: (IsHunk p) => (p :> p :> p) wX wY -> Bool
nonEmptyHunksTriple (p1 :> p2 :> p3) = nonEmptyHunk p1 && nonEmptyHunk p2 && nonEmptyHunk p3

nonEmptyHunksFLPair :: (IsHunk p) => (FL p :> FL p) wX wY -> Bool
nonEmptyHunksFLPair (ps :> qs) = allFL nonEmptyHunk ps && allFL nonEmptyHunk qs

type instance ModelOf Prim = V1Model
instance ArbitraryPrim Prim

instance NullPatch Prim where
  nullPatch (FP _ fp) = nullPatch fp
  nullPatch p | IsEq <- isIdentity p = IsEq
  nullPatch _ = NotEq

instance NullPatch FilePatchType where
  nullPatch (Hunk _ [] []) = unsafeCoerceP IsEq -- is this safe?
  nullPatch _ = NotEq

instance MightBeEmptyHunk Prim where
  isEmptyHunk (FP _ (Hunk _ [] [])) = True
  isEmptyHunk _ = False

instance MightHaveDuplicate Prim

instance Arbitrary (Sealed2 (FL (WithState V1Model Prim))) where
  arbitrary = do repo <- ourSmallRepo
                 liftM (unseal (seal2 . wesPatch)) $ arbitraryState repo

-- instance Show1 (TreeWithFlattenPos Prim) where
--   showDict1 = ShowDictClass

-- WithState and propFail are handy for debugging arbitrary code
propFail :: Int -> Tree Prim wX -> Bool
propFail n xs = sizeTree xs < n

----------------------------------------------------------------------
-- * QuickCheck generators

----------------------------------------------------------------------
-- ** FilePatchType generators

aHunk :: forall wX wY . Content -> Gen (FilePatchType wX wY)
aHunk content
 = sized $ \n ->
     do pos <- choose (1, contentLen+1)
        let prefixLen = pos-1
            restLen   = contentLen-prefixLen
        oldLen <- frequency
                      [ (75, choose (0, min restLen n))
                        -- produces small hunks common in real editing
                      , (25, choose (0, min 10 restLen))
                      ]
        -- newLen choice aims to cover all possibilities, that is,
        -- remove less/the same/more than added and empty the file.
        newLen <- frequency
                      [ ( 54
                        , choose (1,min 1 n)
                        )
                      , ( if oldLen /= 0 then 42 else 0
                        , choose (1,min 1 oldLen)
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return oldLen
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return 0
                        )
                      ]
        new <- vectorOf newLen aLine
        let old = take oldLen $ drop prefixLen $ content
        return $ Hunk pos old new
  where
      contentLen = length content

aTokReplace :: forall wX wY . Content -> Gen (FilePatchType wX wY)
aTokReplace []
  = do w <- vectorOf 1 alpha
       w' <- vectorOf 1 alpha
       return $ TokReplace defaultToks w w'
aTokReplace content
  = do let fileWords = concatMap BC.words content
       wB <- elements fileWords
       w' <- alphaBS `notIn` fileWords
       return $ TokReplace defaultToks (BC.unpack wB) (BC.unpack w')
  where
      alphaBS = do x <- alpha; return $ BC.pack [x]

----------------------------------------------------------------------
-- ** Prim generators

aHunkP :: forall wX wY . (AnchoredPath,File) -> Gen (Prim wX wY)
aHunkP (path,file)
  = do Hunk pos old new <- aHunk content
       return $ hunk (ap2fp path) pos old new
  where
      content = fileContent file

aTokReplaceP :: forall wX wY . (AnchoredPath,File) -> Gen (Prim wX wY)
aTokReplaceP (path,file)
  = do TokReplace tokchars old new <- aTokReplace content
       return $ tokreplace (ap2fp path) tokchars old new
  where
      content = fileContent file

anAddFileP :: forall wX wY . (AnchoredPath,Dir) -> Gen (Prim wX wY)
anAddFileP (path,dir)
  = do newFilename <- aFilename `notIn` existing
       let newPath = path `appendPath` newFilename
       return $ addfile (ap2fp newPath)
  where
      existing = map fst $ filterFiles $ dirContent dir

aRmFileP :: forall wX wY . AnchoredPath   -- ^ Path of an empty file
                          -> Prim wX wY
aRmFileP path = rmfile (ap2fp path)

anAddDirP :: forall wX wY . (AnchoredPath,Dir) -> Gen (Prim wX wY)
anAddDirP (path,dir)
  = do newDirname <- aDirname `notIn` existing
       let newPath = path `appendPath` newDirname
       return $ adddir (ap2fp newPath)
  where
      existing = map fst $ filterDirs $ dirContent dir

aRmDirP :: forall wX wY . AnchoredPath    -- ^ Path of an empty directory
                        -> Prim wX wY
aRmDirP path = rmdir (ap2fp path)

aMoveP :: forall wX wY . Gen Name -> AnchoredPath -> (AnchoredPath,Dir) -> Gen (Prim wX wY)
aMoveP nameGen oldPath (dirPath,dir)
  = do newName <- nameGen `notIn` existing
       let newPath = dirPath `appendPath` newName
       return $ move (ap2fp oldPath) (ap2fp newPath)
  where
      existing = map fst $ dirContent dir

-- | Generates any type of 'Prim' patch, except binary and setpref patches.
aPrim :: forall wX wY . V1Model wX -> Gen (WithEndState V1Model (Prim wX) wY)
aPrim repo
  = do mbFile <- maybeOf repoFiles
       mbEmptyFile <- maybeOf $ filter (isEmpty . snd) repoFiles
       dir  <- elements (rootDir:repoDirs)
       mbOldDir <- maybeOf repoDirs
       mbEmptyDir <- maybeOf $ filter (isEmpty . snd) repoDirs
       patch <- frequency
                  [ ( if isJust mbFile then 12 else 0
                    , aHunkP $ fromJust mbFile
                    )
                  , ( if isJust mbFile then 6 else 0
                    , aTokReplaceP $ fromJust mbFile
                    )
                  , ( 2
                    , anAddFileP dir
                    )
                  , ( if isJust mbEmptyFile then 12 else 0
                    , return $ aRmFileP $ fst $ fromJust mbEmptyFile
                    )
                  , ( 2
                    , anAddDirP dir
                    )
                  , ( if isJust mbEmptyDir then 10 else 0
                    , return $ aRmDirP $ fst $ fromJust mbEmptyDir
                    )
                  , ( if isJust mbFile then 3 else 0
                    , aMoveP aFilename (fst $ fromJust mbFile) dir
                    )
                  , let oldPath = fst $ fromJust mbOldDir in
                    ( if isJust mbOldDir
                         && not (oldPath `isPrefix` fst dir)
                        then 4 else 0
                    , aMoveP aDirname oldPath dir
                    )
                  ]
       let repo' = unFail $ repoApply repo patch
       return $ WithEndState patch repo'
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems
      repoDirs  = filterDirs repoItems
      rootDir   = (anchoredRoot,root repo)

{- [COVERAGE OF aPrim]

  PLEASE,
  if you change something that may affect the coverage of aPrim then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Patch type
  ----------
  42% hunk
  22% tokreplace
  14% move
   6% rmdir
   6% addfile
   6% adddir
   4% rmfile
-}

----------------------------------------------------------------------
-- *** Pairs of primitive patches

-- Try to generate commutable pairs of hunks
hunkPairP :: forall wX wY . (AnchoredPath,File) -> Gen ((Prim :> Prim) wX wY)
hunkPairP (path,file)
  = do h1@(Hunk l1 old1 new1) <- aHunk content
       (delta, content') <- selectChunk h1 content
       Hunk l2' old2 new2 <- aHunk content'
       let l2 = l2'+delta
       return (hunk fpPath l1 old1 new1 :> hunk fpPath l2 old2 new2)
  where
      content = fileContent file
      fpPath = ap2fp path
      selectChunk (Hunk l old new) content_
        = elements [prefix, suffix]
        where
            start = l - 1
            prefix = (0, take start content_)
            suffix = (start + length new, drop (start + length old) content_)
      selectChunk _ _ = impossible

aPrimPair :: forall wX wY . V1Model wX -> Gen (WithEndState V1Model ((Prim :> Prim) wX) wY)
aPrimPair repo
  = do mbFile <- maybeOf repoFiles
       frequency
          [ ( if isJust mbFile then 1 else 0
            , do p1 :> p2 <- hunkPairP $ fromJust mbFile
                 let repo'  = unFail $ repoApply repo p1
                     repo'' = unFail $ repoApply repo' p2
                 return $ WithEndState (p1 :> p2) repo''
            )
          , ( 1
            , do Sealed wesP <- arbitraryState repo
                 return $ unsafeCoerceP1 wesP
            )
          ]
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems

{- [COVERAGE OF aPrimPair]

  PLEASE,
  if you change something that may affect the coverage of aPrimPair then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Rate of ommutable pairs
  -----------------------
  67% commutable

  Commutable coverage (for 1000 tests)
  -------------------
  21% hunks-B
  20% hunks-A
  14% file:>dir
  12% file:>move
   8% trivial-FP
   8% hunk:>tok
   4% hunks-D
   3% tok:>tok
   2% hunks-C
   1% move:>move
   1% dir:>move
   1% dir:>dir
   0% emptyhunk:>file
-}

----------------------------------------------------------------------
-- Arbitrary instances

ourSmallRepo :: Gen (V1Model wX)
ourSmallRepo = aSmallRepo

instance ArbitraryState V1Model Prim where
  arbitraryState s = seal <$> aPrim s


instance Arbitrary (Sealed2 Prim) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed2 (Prim :> Prim)) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp _ <- aPrimPair repo
                 return $ seal2 pp

instance Arbitrary (Sealed ((Prim :> Prim) wA)) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp _ <- aPrimPair repo
                 return $ seal pp

instance Arbitrary (Sealed2 (Prim :> Prim :> Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((Prim :> Prim :> Prim) a)) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (FL Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((FL Prim) wA)) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (FL Prim :> FL Prim)) where
  arbitrary = makeS2Gen ourSmallRepo

instance Arbitrary (Sealed ((FL Prim :> FL Prim) wA)) where
  arbitrary = makeSGen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model Prim)) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model Prim wA)) where
  arbitrary = makeWSGen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model (FL Prim) wA)) where
  arbitrary = makeWSGen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model (Prim :> Prim))) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp repo' <- aPrimPair repo
                 return $ seal2 $ WithState repo pp repo'

instance Arbitrary (Sealed (WithState V1Model (Prim :> Prim) a)) where
  arbitrary = do repo <- ourSmallRepo
                 WithEndState pp repo' <- aPrimPair repo
                 return $ seal $ WithState repo pp repo'


instance Arbitrary (Sealed2 (WithState V1Model (FL Prim))) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed2 (WithState V1Model (FL Prim :> FL Prim))) where
  arbitrary = makeWS2Gen ourSmallRepo

instance Arbitrary (Sealed (WithState V1Model (FL Prim :> FL Prim) a)) where
  arbitrary = makeWSGen ourSmallRepo
