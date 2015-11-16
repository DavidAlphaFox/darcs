-- Copyright (C) 2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}


module Darcs.Test.Patch.Examples.Set2Unwitnessed
       ( primPermutables, primPatches
       , commutables, commutablesFL
       , realCommutables , realMergeables, realTriples
       , realNonduplicateTriples, realPatches, realPatchLoopExamples
       ) where

import Data.Maybe ( catMaybes )
import qualified Data.ByteString.Char8 as BC ( pack )
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch ( invert, hunk )
import Darcs.Patch.Patchy ( Invert(..) )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.V2.Real ( prim2real )
-- import Darcs.Test.Patch.Test () -- for instance Eq Patch
-- import Darcs.Test.Patch.Examples.Set2Unwitnessed
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import qualified Darcs.Test.Patch.Arbitrary.Real as W ( notDuplicatestriple )
--import Darcs.Util.Printer ( greenText )
--import Darcs.Util.Printer.Color ( traceDoc )
--import Darcs.Util.Printer.Color ( errorDoc )
import Darcs.Util.Printer.Color () -- for instance Show Doc
import Darcs.Test.Patch.WSub

import qualified Darcs.Patch.Witnesses.Ordered as W ( (:>), (:\/:) )
import qualified Data.ByteString as B ( ByteString )
import Darcs.Test.Patch.V1Model ( V1Model, Content
                                , makeRepo, makeName, makeFile)
import Darcs.Test.Patch.WithState ( WithStartState(..) )
import Darcs.Patch.Prim.V1.Core ( Prim(FP), FilePatchType(Hunk) )
import Darcs.Util.Path ( FileName, fp2fn )
import Darcs.Patch.Prim ( PrimPatchBase(..), FromPrim )
import Darcs.Patch.Merge ( Merge )
import Darcs.Test.Patch.Arbitrary.Generic
    ( Tree(..)
    , TreeWithFlattenPos(..)
    , commutePairFromTree, commuteTripleFromTree
    , mergePairFromCommutePair, commutePairFromTWFP
    , canonizeTree
    )

-- import Debug.Trace
-- #include "impossible.h"

makeSimpleRepo :: String -> Content -> V1Model wX
makeSimpleRepo filename content = makeRepo [(makeName filename, makeFile content)]


w_tripleExamples :: (FromPrim p, Merge p, Invert p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p W.:> p W.:> p)]
w_tripleExamples = [commuteTripleFromTree seal2 $
                   WithStartState (makeSimpleRepo "file" [])
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "g"]))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [] [BC.pack "j"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "s"])) NilTree)))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "e"])) NilTree))
                  ,commuteTripleFromTree seal2 $
                   WithStartState (makeSimpleRepo "file" [BC.pack "j"])
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "s"]))
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "j"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "j"] [])) NilTree)))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree))
                  ]


w_mergeExamples :: (FromPrim p, Merge p, Invert p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p W.:\/: p)]
w_mergeExamples = map (unseal2 (mergePairFromCommutePair seal2)) w_commuteExamples

w_commuteExamples :: (FromPrim p, Merge p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p W.:> p)]
w_commuteExamples = [
                   commutePairFromTWFP seal2 $
                   WithStartState (makeSimpleRepo "file" [])
                   (TWFP 3
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"])) NilTree)
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "b"]))
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"]))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"]))
                           (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "f"] [])) NilTree)))))),
                   commutePairFromTWFP seal2 $
                   WithStartState
                   (makeSimpleRepo "file" [BC.pack "f",BC.pack "s",BC.pack "d"])
                   (TWFP 3
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "d"] [])) NilTree)
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] []))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "s",BC.pack "d"] []))
                          (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree)))))),
{-                   commutePairFromTWFP seal2 $
                   WithStartState
                   (makeSimpleRepo "file" [BC.pack "f",BC.pack "u",
                                            BC.pack "s",BC.pack "d"])
                   (TWFP 5
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 5 [] [BC.pack "x"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 4 [BC.pack "d"] [])) NilTree))
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f",BC.pack "u"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] []))
                       (SeqTree (FP(fp2fn "./file") (Hunk 1 [BC.pack "u",BC.pack "s",BC.pack "d"] []))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "a"]))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "a"] [])) NilTree))))))),-}
                   commutePairFromTree seal2 $
                   WithStartState (makeSimpleRepo "file" [BC.pack "n",BC.pack "t",BC.pack "h"])
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "n",BC.pack "t",BC.pack "h"] []))
                     NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "h"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "n"] []))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t"] [])) NilTree)))),
                  commutePairFromTree seal2 $
                  WithStartState (makeSimpleRepo "file" [])
                  (ParTree
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "n"])) NilTree)
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "i"]))
                                (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "i"])) NilTree))),
                  commutePairFromTree seal2 $
                  WithStartState (makeSimpleRepo "file" [])
                  (ParTree
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "c"]))
                     (ParTree
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "c"] [BC.pack "r"])) NilTree)
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"]))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "d"])) NilTree))))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree)),
                  commutePairFromTWFP seal2 $
                  WithStartState (makeSimpleRepo "file" [])
                  (TWFP 1
                  (ParTree
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "t"])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "t"])) NilTree))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree))),
                   commutePairFromTWFP seal2 $
                   WithStartState (makeSimpleRepo "file" [BC.pack "f",BC.pack " r",
                                                            BC.pack "c",BC.pack "v"])
                   (TWFP 4
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "c",BC.pack "v"] []))
                        (ParTree
                         (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "r"] []))
                          (SeqTree (FP (fp2fn "fi le") (Hunk 1 [BC.pack "f"] [])) NilTree))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f",BC.pack "r"] []))
                          (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "y"])) NilTree))))
                     (SeqTree (FP (fp2fn "./file") (Hunk 4 [BC.pack "v"] [])) NilTree))),
                   commutePairFromTree seal2 $
                   WithStartState (makeSimpleRepo "file" [])
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "z"])) NilTree)
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree)
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "r"])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "d"])) NilTree))))
                 , commutePairFromTree seal2 $
                   WithStartState (makeSimpleRepo "file" [BC.pack "t",BC.pack "r",BC.pack "h"])
                   (ParTree
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t",BC.pack "r",BC.pack "h"] []))
                              NilTree)
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "o"])) NilTree))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "h"] [])) NilTree)))
                 , commutePairFromTWFP seal2 $
                   WithStartState (makeSimpleRepo "file" []) $
                   TWFP 2
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "y"]))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [] [BC.pack "m"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree))))
                 , commutePairFromTree seal2 $
                 WithStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "p"]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "p"] []))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "c"])) NilTree)))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "z"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j" ]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree)
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j" ]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree)))
                 , commutePairFromTree seal2 $
                 WithStartState (makeSimpleRepo "file" [BC.pack "x",BC.pack "c"])
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"]))
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "c"] [])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "x"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j"])) NilTree))))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "l"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "s"))) NilTree)
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "k")))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 (packStringLetters "k") []))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "m")))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 (packStringLetters "m") [])) NilTree)))))
                 ]

packStringLetters :: String -> [B.ByteString]
packStringLetters = map (BC.pack . (:[]))

w_realPatchLoopExamples :: [Sealed (WithStartState V1Model (Tree Prim))]
w_realPatchLoopExamples =
    [Sealed (WithStartState (makeSimpleRepo fx_name [])
     $ canonizeTree
     (ParTree
      (SeqTree (FP fx (Hunk 1 [] (packStringLetters "pkotufogbvdabnmbzajvolwviqebieonxvcvuvigkfgybmqhzuaaurjspd")))
       (ParTree
        (SeqTree (FP fx (Hunk 47 (packStringLetters "qhzu") (packStringLetters "zafybdcokyjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmh")))
         (ParTree
          (ParTree
           NilTree
           (ParTree
            (ParTree
             (ParTree
              (SeqTree (FP fx (Hunk 15 (packStringLetters "mbzajvolwviqebieonxvcvuvigkfgyb") (packStringLetters "vujnxnhvybvpouyciaabszfmgssezlwwjgnethvrpnfrkubphzvdgymjjoacppqps")))
               (ParTree
                NilTree
                (ParTree
                 (SeqTree (FP fx (Hunk 40 (packStringLetters "ssezlwwjgnethvrpnfrkubphzvdgymjjoacppqpsmzafybdcokyjskcgnvhkbz") (packStringLetters "wnesidpccwoiqiichxaaejdsyrhrusqljlcoro")))
                  (ParTree
                   (ParTree
                    (SeqTree (FP fx (Hunk 12 (packStringLetters "abnvujnxnhvybvpouyciaabszfmgwnesidpccwoiqii") (packStringLetters "czfdhqkipdstfjycqaxwnbxrihrufdeyneqiiiafwzlmg"))) NilTree)
                    NilTree)
                   NilTree))
                 (SeqTree (FP fx (Hunk 25 [] (packStringLetters "dihgmsotezucqdgxczvcivijootyvhlwymbiueufnvpwpeukmskqllalfe"))) NilTree))))
              (SeqTree (FP fx (Hunk 56 (packStringLetters "yjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmhaaurjsp") (packStringLetters "xldhrutyhcyaqeezwujiguawfyawjjqlirxshjddvq"))) NilTree))
             (SeqTree (FP fx (Hunk 20 [] (packStringLetters "ooygwiyogqrqnytixqtmvdxx")))
              (SeqTree (FP fx (Hunk 26 (packStringLetters "yogqrqnytixqtmvdxxvolwviqebieonxvcvuvigkfgybmzafybdcokyjskcgnvhkbz") (packStringLetters "akhsmlbkdxnvfoikmiatfbpzdrsyykkpoxvvddeaspzxe")))
               (SeqTree (FP fx (Hunk 39 [] (packStringLetters "ji")))
                (ParTree
                 NilTree
                 (ParTree
                  NilTree
                  (ParTree
                   (ParTree
                    NilTree
                    (SeqTree (FP fx (Hunk 26 (packStringLetters "akhsmlbkdxnvfjioikmiatfbpzdrsyykkpoxvvddeaspzxepysaafnjjhcstgrczplxs") (packStringLetters "onjbhddskcj")))
                     (SeqTree (FP fx (Hunk 39 [] (packStringLetters "fyscunxxxjjtyqpfxeznhtwvlphmp"))) NilTree)))
                   (ParTree
                    NilTree
                    (SeqTree (FP fx (Hunk 44 [] (packStringLetters "xcchzwmzoezxkmkhcmesplnjpqriypshgiqklgdnbmmkldnydiy")))
                     (ParTree
                      NilTree
                      (SeqTree (FP fx (Hunk 64 (packStringLetters "plnjpqriypshgiqklgdnbmmkldnydiymiatfbpzdrsyykkpoxvvddeaspzxepysaafn") (packStringLetters "anjlzfdqbjqbcplvqvkhwjtkigp"))) NilTree)))))))))))
            (ParTree
             NilTree
             NilTree)))
          NilTree))
        NilTree))
      (ParTree
       NilTree
       (SeqTree (FP fx (Hunk 1 [] (packStringLetters "ti")))
        (SeqTree (FP fx (Hunk 1 (packStringLetters "t") (packStringLetters "ybcop")))
         (SeqTree (FP fx (Hunk 2 [] (packStringLetters "dvlhgwqlpaeweerqrhnjtfolczbqbzoccnvdsyqiefqitrqneralf")))
          (SeqTree (FP fx (Hunk 15 [] (packStringLetters "yairbjphwtnaerccdlfewujvjvmjakbc")))
           (SeqTree (FP fx (Hunk 51 [] (packStringLetters "xayvfuwaiiogginufnhsrmktpmlbvxiakjwllddkiyofyfw")))
            (ParTree
             NilTree
             NilTree)))))))))]
  where
      fx_name :: String
      fx_name = "F"

      fx :: FileName
      fx = fp2fn "./F"


mergeExamples :: [Sealed2 (RealPatch Prim :\/: RealPatch Prim)]
mergeExamples = map (mapSeal2 fromW) w_mergeExamples

realPatchLoopExamples :: [Sealed (WithStartState V1Model (Tree Prim))]
realPatchLoopExamples = w_realPatchLoopExamples

commuteExamples :: [Sealed2 (RealPatch Prim :> RealPatch Prim)]
commuteExamples = map (mapSeal2 fromW) w_commuteExamples

tripleExamples :: [Sealed2 (RealPatch Prim :> RealPatch Prim :> RealPatch Prim)]
tripleExamples = map (mapSeal2 fromW) w_tripleExamples

notDuplicatestriple :: (RealPatch Prim :> RealPatch Prim :> RealPatch Prim) wX wY -> Bool
notDuplicatestriple = W.notDuplicatestriple . toW

quickhunk :: PrimPatch prim => Int -> String -> String -> prim wX wY
quickhunk l o n = hunk "test" l (map (\c -> BC.pack [c]) o)
                                (map (\c -> BC.pack [c]) n)

primPermutables :: [(Prim :> Prim :> Prim) wX wY]
primPermutables =
    [quickhunk 0 "e" "bo" :> quickhunk 3 "" "x" :> quickhunk 2 "f" "qljo"]

mergeables :: [(Prim :\/: Prim) wX wY]
mergeables = [quickhunk 1 "a" "b" :\/: quickhunk 1 "a" "c",
              quickhunk 1 "a" "b" :\/: quickhunk 3 "z" "c",
              quickhunk 0 "" "a" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "b" "",
              quickhunk 0 "" "a" :\/: quickhunk 1 "b" ""
             ]

mergeablesFL :: [(FL Prim :\/: FL Prim) wX wY]
mergeablesFL = map (\ (x:\/:y) -> (x :>: NilFL) :\/: (y :>: NilFL)) mergeables ++
           [] --    [(quickhunk 1 "a" "b" :>: quickhunk 3 "z" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL),
              --  (quickhunk 1 "a" "b" :>: quickhunk 1 "b" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL)]

mergeable2commutable :: Invert p => (p :\/: p) wX wY -> (p :> p) wX wY
mergeable2commutable (x :\/: y) = unsafeCoerceP (invert x) :> y

commutablesFL :: [(FL Prim :> FL Prim) wX wY]
commutablesFL = map mergeable2commutable mergeablesFL
commutables :: [(Prim :> Prim) wX wY]
commutables = map mergeable2commutable mergeables

primPatches :: [Prim wX wY]
primPatches = concatMap mergeable2patches mergeables
    where mergeable2patches (x:\/:y) = [x,y]

realPatches :: [RealPatch Prim wX wY]
realPatches = concatMap commutable2patches realCommutables
    where commutable2patches (x:>y) = [x,y]

realTriples :: [(RealPatch Prim :> RealPatch Prim :> RealPatch Prim) wX wY]
realTriples = [ob' :> oa2 :> a2'',
                oa' :> oa2 :> a2'']
               ++ map unsafeUnseal2 tripleExamples
               ++ map unsafeUnseal2 (concatMap getTriples realFLs)
    where oa = prim2real $ quickhunk 1 "o" "aa"
          oa2 = oa
          a2 = prim2real $ quickhunk 2 "a34" "2xx"
          ob = prim2real $ quickhunk 1 "o" "bb"
          ob' :/\: oa' = merge (oa :\/: ob)
          a2' :/\: _ = merge (ob' :\/: a2)
          a2'' :/\: _ = merge (oa2 :\/: a2')

realNonduplicateTriples :: [(RealPatch Prim :> RealPatch Prim :> RealPatch Prim) wX wY]
realNonduplicateTriples = filter (notDuplicatestriple) realTriples

realFLs :: [FL (RealPatch Prim) wX wY]
realFLs = [oa :>: invert oa :>: oa :>: invert oa :>: ps +>+ oa :>: invert oa :>: NilFL]
    where oa = prim2real $ quickhunk 1 "o" "a"
          ps :/\: _ = merge (oa :>: invert oa :>: NilFL :\/: oa :>: invert oa :>: NilFL)

realCommutables :: [(RealPatch Prim :> RealPatch Prim) wX wY]
realCommutables = map unsafeUnseal2 commuteExamples++
                   map mergeable2commutable realMergeables++
                   [invert oa :> ob'] ++ map unsafeUnseal2 (concatMap getPairs realFLs)
    where oa = prim2real $ quickhunk 1 "o" "a"
          ob = prim2real $ quickhunk 1 "o" "b"
          _ :/\: ob' = mergeFL (ob :\/: oa :>: invert oa :>: NilFL)

realMergeables :: [(RealPatch Prim :\/: RealPatch Prim) wX wY]
realMergeables = map (\ (x :\/: y) -> prim2real x :\/: prim2real y) mergeables
                        ++ realIglooMergeables
                        ++ realQuickcheckMergeables
                        ++ map unsafeUnseal2 mergeExamples
                        ++ catMaybes (map pair2m (concatMap getPairs realFLs))
                        ++ [(oa :\/: od),
                            (oa :\/: a2'),
                            (ob' :\/: od''),
                            (oe :\/: od),
                            (of' :\/: oe'),
                            (ob' :\/: oe'),
                            (oa :\/: oe'),
                            (ob' :\/: oc'),
                            (b2' :\/: oc'''),
                            (ob' :\/: a2),
                            (b2' :\/: og'''),
                            (oc''' :\/: og'''),
                            (oc'' :\/: og''),
                            (ob'' :\/: og''),
                            (ob'' :\/: oc''),
                            (oc' :\/: od'')]
    where oa = prim2real $ quickhunk 1 "o" "aa"
          a2 = prim2real $ quickhunk 2 "a34" "2xx"
          og = prim2real $ quickhunk 3 "4" "g"
          ob = prim2real $ quickhunk 1 "o" "bb"
          b2 = prim2real $ quickhunk 2 "b" "2"
          oc = prim2real $ quickhunk 1 "o" "cc"
          od = prim2real $ quickhunk 7 "x" "d"
          oe = prim2real $ quickhunk 7 "x" "e"
          pf = prim2real $ quickhunk 7 "x" "f"
          od'' = prim2real $ quickhunk 8 "x" "d"
          ob' :>: b2' :>: NilFL :/\: _ = mergeFL (oa :\/: ob :>: b2 :>: NilFL)
          a2' :/\: _ = merge (ob' :\/: a2)
          ob'' :/\: _ = merge (a2 :\/: ob')
          og' :/\: _ = merge (oa :\/: og)
          og'' :/\: _ = merge (a2 :\/: og')
          og''' :/\: _ = merge (ob' :\/: og')
          oc' :/\: _ = merge (oa :\/: oc)
          oc'' :/\: _ = merge (a2 :\/: oc)
          oc''' :/\: _ = merge (ob' :\/: oc')
          oe' :/\: _ = merge (od :\/: oe)
          of' :/\: _ = merge (od :\/: pf)
          pair2m :: Sealed2 (RealPatch Prim :> RealPatch Prim)
                 -> Maybe ((RealPatch Prim :\/: RealPatch Prim) wX wY)
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

realIglooMergeables :: [(RealPatch Prim :\/: RealPatch Prim) wX wY]
realIglooMergeables = [(a :\/: b),
                    (b :\/: c),
                    (a :\/: c),
                    (x :\/: a),
                    (y :\/: b),
                    (z :\/: c),
                    (x' :\/: y'),
                    (z' :\/: y'),
                    (x' :\/: z'),
                    (a :\/: a)]
    where a = prim2real $ quickhunk 1 "1" "A"
          b = prim2real $ quickhunk 2 "2" "B"
          c = prim2real $ quickhunk 3 "3" "C"
          x = prim2real $ quickhunk 1 "1BC" "xbc"
          y = prim2real $ quickhunk 1 "A2C" "ayc"
          z = prim2real $ quickhunk 1 "AB3" "abz"
          x' :/\: _ = merge (a :\/: x)
          y' :/\: _ = merge (b :\/: y)
          z' :/\: _ = merge (c :\/: z)

realQuickcheckMergeables :: [(RealPatch Prim :\/: RealPatch Prim) wX wY]
realQuickcheckMergeables = [-- invert k1 :\/: n1
                             --, invert k2 :\/: n2
                               hb :\/: k
                             , b' :\/: b'
                             , n' :\/: n'
                             , b :\/: d
                             , k' :\/: k'
                             , k3 :\/: k3
                             ] ++ catMaybes (map pair2m pairs)
    where hb = prim2real $ quickhunk 0 "" "hb"
          k = prim2real $ quickhunk 0 "" "k"
          n = prim2real $ quickhunk 0 "" "n"
          b = prim2real $ quickhunk 1 "b" ""
          d = prim2real $ quickhunk 2 "" "d"
          d':/\:_ = merge (b :\/: d)
          --k1 :>: n1 :>: NilFL :/\: _ = mergeFL (hb :\/: k :>: n :>: NilFL)
          --k2 :>: n2 :>: NilFL :/\: _ =
          --    merge (hb :>: b :>: NilFL :\/: k :>: n :>: NilFL)
          k' :>: n' :>: NilFL :/\: _ :>: b' :>: _ = merge (hb :>: b :>: d' :>: NilFL :\/: k :>: n :>: NilFL)
          pairs = getPairs (hb :>: b :>: d' :>: k' :>: n' :>: NilFL)
          pair2m :: Sealed2 (RealPatch Prim :> RealPatch Prim)
                 -> Maybe ((RealPatch Prim :\/: RealPatch Prim) wX wY)
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

          i = prim2real $ quickhunk 0 "" "i"
          x = prim2real $ quickhunk 0 "" "x"
          xi = prim2real $ quickhunk 0 "xi" ""
          d3 :/\: _ = merge (xi :\/: d)
          _ :/\: k3 = mergeFL (k :\/: i :>: x :>: xi :>: d3 :>: NilFL)

