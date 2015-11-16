--  Copyright (C) 2002-2005,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE CPP #-}

module Darcs.Test.Patch.Examples.Set1
       ( knownCommutes, knownCantCommutes, knownMerges
       , knownMergeEquivs, knownCanons, mergePairs2
       , validPatches, commutePairs, mergePairs
       , primitiveTestPatches, testPatches, testPatchesNamed
       , primitiveCommutePairs ) where

import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString.Char8 as BC ( pack )
import qualified Data.ByteString as B ( empty )
import Darcs.Patch
     ( commute, invert, merge
     , Named, namepatch
     , readPatch, fromPrim
     , adddir, addfile, hunk, binary, rmdir, rmfile, tokreplace )
import Darcs.Patch.Prim ( PrimOf, FromPrim )
import Darcs.Patch.Prim.V1 ( Prim )
import qualified Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Test.Patch.Properties.Check( checkAPatch )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed ( unsafeUnseal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )

#include "impossible.h"

type Patch = V1.Patch Prim

-- The unit tester function is really just a glorified map for functions that
-- return lists, in which the lists get concatenated (where map would end up
-- with a list of lists).

quickmerge :: (FL Patch :\/: FL Patch) wX wY -> FL Patch wY wZ
quickmerge (p1:\/:p2) = case merge (p1:\/:p2) of
                        _ :/\: p1' -> unsafeCoercePEnd p1'

-- ----------------------------------------------------------------------
-- * Show/Read tests
-- ----------------------------------------------------------------------

-- | This test involves calling 'show' to print a string describing a patch,
--   and then using readPatch to read it back in, and making sure the patch we
--   read in is the same as the original.  Useful for making sure that I don't
--   have any stupid IO bugs.

-- ----------------------------------------------------------------------
-- * Canonization tests
-- ----------------------------------------------------------------------

knownCanons :: [(FL Patch wX wY,FL Patch wX wY)]
knownCanons =
    [(quickhunk 1 "abcde" "ab" :>: NilFL, quickhunk 3 "cde"   "" :>: NilFL),
     (quickhunk 1 "abcde" "bd" :>: NilFL,
      quickhunk 1 "a" "" :>:
      quickhunk 2 "c" "" :>:
      quickhunk 3 "e" "" :>: NilFL),
     (quickhunk 4 "a" "b" :>:
      quickhunk 1 "c" "d" :>: NilFL,
      quickhunk 1 "c" "d" :>:
      quickhunk 4 "a" "b" :>: NilFL),
     (quickhunk 1 "a" "" :>:
      quickhunk 1 "" "b" :>: NilFL,
      quickhunk 1 "a" "b" :>: NilFL),
     (quickhunk 1 "ab" "c" :>:
      quickhunk 1 "cd" "e" :>: NilFL,
      quickhunk 1 "abd" "e" :>: NilFL),
     (quickhunk 1 "abcde" "cde" :>: NilFL, quickhunk 1 "ab" "" :>: NilFL),
     (quickhunk 1 "abcde" "acde" :>: NilFL, quickhunk 2 "b" "" :>: NilFL)]

quickhunk :: (FromPrim p, PrimOf p ~ Prim) => Int -> String -> String -> p wX wY
quickhunk l o n = fromPrim $ hunk "test" l (map (\c -> BC.pack [c]) o)
                                             (map (\c -> BC.pack [c]) n)

-- ----------------------------------------------------------------------
-- * Merge/unmgerge tests
-- ----------------------------------------------------------------------

-- | It should always be true that if two patches can be unmerged, then merging
--   the resulting patches should give them back again.
mergePairs :: [(FL Patch :\/: FL Patch) wX wY]
mergePairs =
  take 400 [(p1:\/:p2)|
            i <- [0..(length testPatches)-1],
            p1<-[testPatches!!i],
            p2<-drop i testPatches,
            checkAPatch (invert p2 :>: p1 :>: NilFL)]

-- ----------------------------------------------------------------------
-- * Commute/recommute tests
-- ----------------------------------------------------------------------

-- | Here we test to see if commuting patch A and patch B and then commuting
-- the result gives us patch A and patch B again.  The set of patches (A,B)
-- is chosen from the set of all pairs of test patches by selecting those which
-- commute with one another.
commutePairs :: [(FL Patch :> FL Patch) wX wY]
commutePairs =
  take 200 [(p1:>p2)|
            p1<-testPatches,
            p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatches,
            commute (p1:>p2) /= Nothing]

primitiveCommutePairs :: [(FL Patch :> FL Patch) wX wY]
primitiveCommutePairs =
  [(p2:>p1)|
   p1<-primitiveTestPatches,
   p2<-primitiveTestPatches,
   commute (p2:>p1) /= Nothing,
   checkAPatch (p2:>:p1:>:NilFL)]

-- ----------------------------------------------------------------------
-- * Commute tests
-- ----------------------------------------------------------------------

-- | Here we provide a set of known interesting commutes.
knownCommutes :: [((FL Patch:<FL Patch) wX wY,(FL Patch:<FL Patch) wX wY)]
knownCommutes = [
                  (testhunk 1 [] ["A"]:<
                   testhunk 2 [] ["B"],
                   testhunk 3 [] ["B"]:<
                   testhunk 1 [] ["A"]),
                  (fromPrim (tokreplace "test" "A-Za-z_" "old" "new"):<
                   testhunk 2
                   ["hello world all that is old is good old_"]
                   ["I don't like old things"],
                   testhunk 2
                   ["hello world all that is new is good old_"]
                   ["I don't like new things"]:<
                   fromPrim (tokreplace "test" "A-Za-z_" "old" "new")),
                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 2 ["C"] ["D"],
                   testhunk 2 ["C"] ["D"]:<
                   testhunk 1 ["A"] ["B"]),
                  (fromPrim (rmfile "NwNSO"):<
                   (quickmerge (fromPrim (addfile "hello"):\/:fromPrim (addfile "hello"))),
                   (quickmerge (fromPrim (addfile "hello"):\/:fromPrim (addfile "hello"))):<
                   fromPrim (rmfile "NwNSO")),

                  (quickmerge (testhunk 3 ["o"] ["n"]:\/:
                               testhunk 3 ["o"] ["v"]):<
                   testhunk 1 [] ["a"],
                   testhunk 1 [] ["a"]:<
                   quickmerge (testhunk 2 ["o"] ["n"]:\/:
                               testhunk 2 ["o"] ["v"])),

                  (testhunk 1 ["A"] []:<
                   testhunk 3 ["B"] [],
                   testhunk 2 ["B"] []:<
                   testhunk 1 ["A"] []),

                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 2 ["B"] ["C"],
                   testhunk 2 ["B"] ["C"]:<
                   testhunk 1 ["A"] ["B"]),

                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 3 ["B"] ["C"],
                   testhunk 3 ["B"] ["C"]:<
                   testhunk 1 ["A"] ["B"]),

                  (testhunk 1 ["A"] ["B","C"]:<
                   testhunk 2 ["B"] ["C","D"],
                   testhunk 3 ["B"] ["C","D"]:<
                   testhunk 1 ["A"] ["B","C"])]
  where testhunk l o n = fromPrim $ hunk "test" l (map BC.pack o) (map BC.pack n)

knownCantCommutes :: [(FL Patch:< FL Patch) wX wY]
knownCantCommutes = [
                      (testhunk 2 ["o"] ["n"]:<
                       testhunk 1 [] ["A"]),
                      (testhunk 1 [] ["A"]:<
                       testhunk 1 ["o"] ["n"]),
                      (quickmerge (testhunk 2 ["o"] ["n"]:\/:
                                   testhunk 2 ["o"] ["v"]):<
                       testhunk 1 [] ["a"]),
                      (fromPrim (hunk "test" 1 ([BC.pack "a"]) ([BC.pack "b"])):<
                       fromPrim (addfile "test"))]
  where testhunk l o n = fromPrim $ hunk "test" l (map BC.pack o) (map BC.pack n)

-- ----------------------------------------------------------------------
-- * Merge tests
-- ----------------------------------------------------------------------

-- | Here we provide a set of known interesting merges.
knownMerges :: [((FL Patch:\/:FL Patch) wX wY,FL Patch wY wZ)]
knownMerges = [
                (testhunk 2 [BC.pack "c"] [BC.pack "d",BC.pack "e"]:\/:
                 testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"],
                 testhunk 3 [BC.pack "c"] [BC.pack "d",BC.pack "e"]),
                (testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"]:\/:
                 testhunk 2 [BC.pack "c"] [BC.pack "d",BC.pack "e"],
                 testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"]),
                (testhunk 3 [BC.pack "A"] []:\/:
                 testhunk 1 [BC.pack "B"] [],
                 testhunk 2 [BC.pack "A"] []),
                (fromPrim (rmdir "./test/world"):\/:
                 fromPrim (hunk "./world" 3 [BC.pack "A"] []),
                 fromPrim (rmdir "./test/world")),

                ((quickhunk 1 "a" "bc" :>:
                  quickhunk 6 "d" "ef" :>: NilFL):\/:
                 (quickhunk 3 "a" "bc" :>:
                  quickhunk 8 "d" "ef" :>: NilFL),
                 (quickhunk 1 "a" "bc" :>:
                  quickhunk 7 "d" "ef" :>: NilFL)),

                (testhunk 1 [BC.pack "A"] [BC.pack "B"]:\/:
                 testhunk 2 [BC.pack "B"] [BC.pack "C"],
                 testhunk 1 [BC.pack "A"] [BC.pack "B"]),

                (testhunk 2 [BC.pack "A"] [BC.pack "B",BC.pack "C"]:\/:
                 testhunk 1 [BC.pack "B"] [BC.pack "C",BC.pack "D"],
                 testhunk 3 [BC.pack "A"] [BC.pack "B",BC.pack "C"])]
  where testhunk l o n = fromPrim $ hunk "test" l o n

knownMergeEquivs :: [((FL Patch :\/: FL Patch) wX wY, FL Patch wY wZ)]
knownMergeEquivs = [

                     -- The following tests are going to be failed by the
                     -- Conflictor code as a cleanup.

                     --(addfile "test":\/:
                     -- adddir "test",
                     -- joinPatches (adddir "test" :>:
                     --              addfile "test-conflict" :>: NilFL)),
                     --(move "silly" "test":\/:
                     -- adddir "test",
                     -- joinPatches (adddir "test" :>:
                     --              move "silly" "test-conflict" :>: NilFL)),
                     --(addfile "test":\/:
                     -- move "old" "test",
                     -- joinPatches (addfile "test" :>:
                     --              move "old" "test-conflict" :>: NilFL)),
                     --(move "a" "test":\/:
                     -- move "old" "test",
                     -- joinPatches (move "a" "test" :>:
                     --              move "old" "test-conflict" :>: NilFL)),
                     (fromPrim (hunk "test" 1 [] [BC.pack "A"]) :\/:
                       fromPrim (hunk "test" 1 [] [BC.pack "B"]),
                       fromPrim (hunk "test" 1 [] [BC.pack "A", BC.pack "B"])),
                     (fromPrim (hunk "test" 1 [] [BC.pack "a"]):\/:
                      fromPrim (hunk "test" 1 [BC.pack "b"] []),
                      unsafeCoerceP NilFL),
                      --hunk "test" 1 [] [BC.pack "v v v v v v v",
                      --                  BC.pack "*************",
                      --                  BC.pack "a",
                      --                  BC.pack "b",
                      --                  BC.pack "^ ^ ^ ^ ^ ^ ^"]),
                     (quickhunk 4 "a"  "" :\/:
                      quickhunk 3 "a"  "",
                      quickhunk 3 "aa" ""),
                     ((quickhunk 1 "a" "bc" :>:
                       quickhunk 6 "d" "ef" :>: NilFL) :\/:
                       (quickhunk 3 "a" "bc" :>:
                        quickhunk 8 "d" "ef" :>: NilFL),
                      quickhunk 3 "a" "bc" :>:
                      quickhunk 8 "d" "ef" :>:
                      quickhunk 1 "a" "bc" :>:
                      quickhunk 7 "d" "ef" :>: NilFL),
                     (quickmerge (quickhunk 2 "" "bd":\/:quickhunk 2 "" "a") :\/:
                              quickmerge (quickhunk 2 "" "c":\/:quickhunk 2 "" "a"),
                              quickhunk 2 "" "abdc")
                     ]


-- | It also is useful to verify that it doesn't matter which order we specify
--   the patches when we merge.
mergePairs2 :: [(FL Patch wX wY, FL Patch wX wZ)]
mergePairs2 = [(p1, p2) |
               p1<-primitiveTestPatches,
               p2<-primitiveTestPatches,
               checkAPatch (invert p1:>:p2:>:NilFL)
              ]

-- ----------------------------------------------------------------------
-- Patch test data
-- This is where we define the set of patches which we run our tests on.  This
-- should be kept up to date with as many interesting permutations of patch
-- types as possible.
-- ----------------------------------------------------------------------

testPatches :: [FL Patch wX wY]
testPatchesNamed :: [Named Patch wX wY]
testPatchesAddfile :: [FL Patch wX wY]
testPatchesRmfile :: [FL Patch wX wY]
testPatchesHunk :: [FL Patch wX wY]
primitiveTestPatches :: [FL Patch wX wY]
testPatchesBinary :: [FL Patch wX wY]
testPatchesCompositeNocom :: [FL Patch wX wY]
testPatchesComposite :: [FL Patch wX wY]
testPatchesTwoCompositeHunks :: [FL Patch wX wY]
testPatchesCompositeHunks :: [FL Patch wX wY]
testPatchesCompositeFourHunks :: [FL Patch wX wY]
testPatchesMerged :: [FL Patch wX wY]
validPatches :: [FL Patch wX wY]

testPatchesNamed = [unsafePerformIO $
                      namepatch "date is" "patch name" "David Roundy" []
                                (fromPrim $ addfile "test"),
                      unsafePerformIO $
                      namepatch "Sat Oct 19 08:31:13 EDT 2002"
                                "This is another patch" "David Roundy"
                                ["This log file has","two lines in it"]
                                (fromPrim $ rmfile "test")]
testPatchesAddfile = map fromPrim
                       [addfile "test",adddir "test",addfile "test/test"]
testPatchesRmfile = map invert testPatchesAddfile
testPatchesHunk  =
    [fromPrim (hunk file line old new) |
     file <- ["test"],
     line <- [1,2],
     old <- map (map BC.pack) partials,
     new <- map (map BC.pack) partials,
     old /= new
    ]
    where partials  = [["A"],["B"],[],["B","B2"]]

primitiveTestPatches = testPatchesAddfile ++
                         testPatchesRmfile ++
                         testPatchesHunk ++
                         [unsafeUnseal.fromJust.readPatch $
                          BC.pack "move ./test/test ./hello",
                          unsafeUnseal.fromJust.readPatch $
                          BC.pack "move ./test ./hello"] ++
                         testPatchesBinary

testPatchesBinary =
    [fromPrim $ binary "./hello"
     (BC.pack $ "agadshhdhdsa75745457574asdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg")
     (BC.pack $ "adafjttkykrehhtrththrthrthre" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaagg"),
     fromPrim $ binary "./hello"
     B.empty
     (BC.pack "adafjttkykrere")]

testPatchesCompositeNocom =
    take 50 [p1+>+p2|
             p1<-primitiveTestPatches,
             p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) primitiveTestPatches,
             commute (p1:>p2) == Nothing]

testPatchesComposite =
    take 100 [p1+>+p2|
              p1<-primitiveTestPatches,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) primitiveTestPatches,
              commute (p1:>p2) /= Nothing,
              commute (p1:>p2) /= Just (unsafeCoerceP p2:>unsafeCoerceP p1)]

testPatchesTwoCompositeHunks =
    take 100 [p1+>+p2|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk]

testPatchesCompositeHunks =
    take 100 [p1+>+p2+>+p3|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk,
              p3<-filter (\p->checkAPatch (p1:>:p2:>:p:>:NilFL)) testPatchesHunk]

testPatchesCompositeFourHunks =
    take 100 [p1+>+p2+>+p3+>+p4|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk,
              p3<-filter (\p->checkAPatch (p1:>:p2:>:p:>:NilFL)) testPatchesHunk,
              p4<-filter (\p->checkAPatch (p1:>:p2:>:p3:>:p:>:NilFL)) testPatchesHunk]

testPatchesMerged =
  take 200
    [p2+>+quickmerge (p1:\/:p2) |
     p1<-take 10 (drop 15 testPatchesCompositeHunks)++primitiveTestPatches
         ++take 10 (drop 15 testPatchesTwoCompositeHunks)
         ++ take 2 (drop 4 testPatchesCompositeFourHunks),
     p2<-take 10 testPatchesCompositeHunks++primitiveTestPatches
         ++take 10 testPatchesTwoCompositeHunks
         ++take 2 testPatchesCompositeFourHunks,
     checkAPatch (invert p1 :>: p2 :>: NilFL),
     commute (p2:>p1) /= Just (p1:>p2)
    ]

testPatches =  primitiveTestPatches ++
                testPatchesComposite ++
                testPatchesCompositeNocom ++
                testPatchesMerged

-- ----------------------------------------------------------------------
-- * Check patch test
-- ----------------------------------------------------------------------

validPatches = [(quickhunk 4 "a" "b" :>:
                 quickhunk 1 "c" "d" :>: NilFL),
                (quickhunk 1 "a" "bc" :>:
                 quickhunk 1 "b" "d" :>: NilFL),
                (quickhunk 1 "a" "b" :>:
                 quickhunk 1 "b" "d" :>: NilFL)]++testPatches
