{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-overlapping-patterns #-}
{-# LANGUAGE CPP #-}
module Darcs.Patch.Prim.V3.Commute
    ( CommuteMonad(..) )
    where

import Data.List ( intersect )

import qualified Data.ByteString as BS (length)

import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Prim.V3.Core ( Prim(..), Hunk(..), touches )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Permutations () -- for Invert instance of FL

#include "impossible.h"

class Monad m => CommuteMonad m where
  commuteFail :: m a
  -- TODO we eventually have to get rid of runCommute with this signature,
  -- since m might involve IO at some point, which we can't "run";
  -- alternatively, for IO it could always yield Nothing, having a separate
  -- IO-specific function to "run" commutes in IO

instance CommuteMonad Maybe where
  commuteFail = Nothing

instance Commute Prim where
    commute = commute'

class Commute' p where
  commute' :: (CommuteMonad m) => (p :> p) wX wY -> m ((p :> p) wX wY)

typematch :: Prim wX wY -> Prim wY wZ -> Bool
typematch _ _ = True -- TODO

instance Commute' Prim where
  commute' (a :> b) | null (touches a `intersect` touches b) = return (unsafeCoerceP b :> unsafeCoerceP a)
                    | not (a `typematch` b) = commuteFail
                    | otherwise = commuteOverlapping (a :> b)

-- Commute patches that have actual overlap in terms of touched objects, and their types allow
commuteOverlapping :: (CommuteMonad m) => (Prim :> Prim) wX wY -> m ((Prim :> Prim) wX wY)
commuteOverlapping (BinaryHunk a x :> BinaryHunk _ y) =
  do (y' :> x') <- commuteHunk (x :> y)
     return $ unsafeCoerceP (BinaryHunk a y' :> BinaryHunk a x')
commuteOverlapping (TextHunk a x :> TextHunk _ y) =
  do (y' :> x') <- commuteHunk (x :> y)
     return $ unsafeCoerceP (TextHunk a y' :> TextHunk a x')
commuteOverlapping _ = commuteFail

commuteHunk :: (CommuteMonad m) => (Hunk :> Hunk) wX wY -> m ((Hunk :> Hunk) wY wX)
commuteHunk (Hunk off1 old1 new1 :> Hunk off2 old2 new2)
  | off1 + lengthnew1 < off2 =
    return $ Hunk (off2 - lengthnew1 + lengthold1) old2 new2 :> Hunk off1 old1 new1
  | off2 + lengthold2 < off1 =
    return $ Hunk off2 old2 new2 :> Hunk (off1 + lengthnew2 - lengthold2) old1 new1
  | off1 + lengthnew1 == off2 &&
    lengthold2 /= 0 && lengthold1 /= 0 && lengthnew2 /= 0 && lengthnew1 /= 0 =
      return $ Hunk (off2 - lengthnew1 + lengthold1) old2 new2 :> Hunk off1 old1 new1
  | off2 + lengthold2 == off1 &&
    lengthold2 /= 0 && lengthold1 /= 0 && lengthnew2 /= 0 && lengthnew1 /= 0 =
      return $ Hunk off2 old2 new2 :> Hunk (off1 + lengthnew2 - lengthold2) old1 new1
  | otherwise = commuteFail
  where lengthnew1 = BS.length new1
        lengthnew2 = BS.length new2
        lengthold1 = BS.length old1
        lengthold2 = BS.length old2
commuteHunk _ = impossible
