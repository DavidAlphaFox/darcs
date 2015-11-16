--  Copyright (C) 2007 David Roundy
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

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}


module Darcs.Test.Patch.Properties.Generic
    ( invertSymmetry, inverseComposition, invertRollback,
      recommute, commuteInverses, effectPreserving,
      permutivity, partialPermutivity,
      patchAndInverseCommute, mergeEitherWay,
      show_read,
      mergeCommute, mergeConsistent, mergeArgumentsConsistent,
      coalesceEffectPreserving, coalesceCommute, propIsMergeable
    ) where

import Darcs.Test.Util.TestResult ( TestResult, succeeded, failed, rejected,
                                    (<&&>), fromMaybe )
import Darcs.Test.Patch.RepoModel ( RepoModel, RepoState, repoApply, eqModel, showModel
                                  , maybeFail )
import Darcs.Test.Patch.WithState ( WithState(..), WithStartState(..) )
import Darcs.Test.Patch.Arbitrary.Generic ( Tree, flattenOne, MightBeEmptyHunk(..), MightHaveDuplicate(..) )

import Control.Monad ( msum )
import Darcs.Patch.Witnesses.Show ( Show2(..), show2 )
import Darcs.Patch.Patchy ( Patchy, showPatch, commute, invert )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Prim.Class ( PrimPatch, PrimOf, FromPrim )
import Darcs.Patch ()
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Commute ( commuteFLorComplain )
import Darcs.Patch.Merge ( Merge(merge) )
import Darcs.Patch.Read ( readPatch )
import Darcs.Patch.Invert ( invertFL )
import Darcs.Patch.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..), (:\/:)(..), (:/\:)(..), lengthFL, eqFL, reverseRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal2, Sealed2 )
import Darcs.Util.Printer ( Doc, renderPS, redText, greenText, ($$), text, RenderMode(..) )
--import Darcs.ColorPrinter ( traceDoc )

propIsMergeable :: forall model p wX . (FromPrim p, Merge p, RepoModel model)
                  => Sealed (WithStartState model (Tree (PrimOf p)))
                  -> Maybe (Tree p wX)
propIsMergeable (Sealed (WithStartState _ t))
   = case flattenOne t of
        Sealed ps -> let _ = seal2 ps :: Sealed2 (FL p)
                     in case lengthFL ps of
                       _ -> Nothing

-- | invert symmetry   inv(inv(p)) = p
invertSymmetry :: (Patchy p, MyEq p) => p wA wB -> TestResult
invertSymmetry p = case invert (invert p) =\/= p of
                        IsEq  -> succeeded
                        NotEq -> failed $ redText "p /= inv(inv(p))"

inverseComposition :: (Patchy p, MyEq p) => (p :> p) wX wY -> TestResult
inverseComposition (a :> b) =
    case eqFL (reverseRL (invertFL (a:>:b:>:NilFL))) (invert b:>:invert a:>:NilFL) of
      IsEq -> succeeded
      NotEq -> failed $ redText "inv(a :>: b :>: NilFL) /= inv(b) :>: inv(a) :>: NilFL"

-- | invert rollback   if b = A(a) then a = A'(b)
invertRollback :: (ApplyState p ~ RepoState model, Patchy p, ShowPatchBasic p, RepoModel model)
                => WithState model p wA wB -> TestResult
invertRollback (WithState a x b)
  = case maybeFail $ repoApply b (invert x) of
         Nothing -> failed $ redText "x' not applicable to b."
         Just a1 -> if a1 `eqModel` a
                       then succeeded
                       else failed $ redText "a1: " $$ text (showModel a1)
                                  $$ redText " ---- is not equals to a:" $$ text (showModel a)
                                  $$ redText "where a was" $$ text (showModel b)
                                  $$ redText "with (invert x) on top:" $$ showPatch (invert x)

-- | recommute   AB ↔ B′A′ if and only if B′A′ ↔ AB
recommute :: (Patchy p, ShowPatchBasic p, MyEq p, MightHaveDuplicate p)
          => (forall wX wY . ((p :> p) wX wY -> Maybe ((p :> p) wX wY)))
          -> (p :> p) wA wB -> TestResult
recommute c (x :> y) =
    case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x')
      -- this test unfortunately fails on some V2 patches that contain duplicates
      -- after the commute. While in theory the underlying bug should be fixed,
      -- we don't know how to and even if we did, it would probably involve a repository
      -- migration to a new patch type.
      | hasDuplicate y' || hasDuplicate x' -> rejected
      | otherwise ->
       case c (y' :> x') of
         Nothing -> failed (redText "failed" $$ showPatch y' $$ redText ":>" $$ showPatch x')
         Just (x'' :> y'') ->
             case y'' =/\= y of
             NotEq -> failed (redText "y'' =/\\= y failed, where x" $$ showPatch x $$
                              redText ":> y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText ":> x'" $$ showPatch x' $$
                              redText "x''" $$ showPatch x'' $$
                              redText ":> y''" $$ showPatch y'')
             IsEq -> case x'' =/\= x of
                     NotEq -> failed (redText "x'' /= x" $$ showPatch x'' $$ redText ":>" $$ showPatch y'')
                     IsEq -> succeeded

-- | commuteInverses   AB ↔ B′A′ if and only if B⁻¹A⁻¹ ↔ A′⁻¹B′⁻¹
commuteInverses :: (Patchy p, ShowPatchBasic p, MyEq p) => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                 -> (p :> p) wA wB -> TestResult
commuteInverses c (x :> y) =
    case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x') ->
        case c (invert y :> invert x) of
        Nothing -> failed $ redText "second commute failed" $$
                            redText "x" $$ showPatch x $$ redText "y" $$ showPatch y $$
                            redText "y'" $$ showPatch y' $$ redText "x'" $$ showPatch x'
        Just (ix' :> iy') ->
            case invert ix' =/\= x' of
            NotEq -> failed $ redText "invert ix' /= x'" $$
                              redText "x" $$ showPatch x $$
                              redText "y" $$ showPatch y $$
                              redText "y'" $$ showPatch y' $$
                              redText "x'" $$ showPatch x' $$
                              redText "ix'" $$ showPatch ix' $$
                              redText "iy'" $$ showPatch iy' $$
                              redText "invert ix'" $$ showPatch (invert ix') $$
                              redText "invert iy'" $$ showPatch (invert iy')
            IsEq -> case y' =\/= invert iy' of
                    NotEq -> failed $ redText "y' /= invert iy'" $$ showPatch iy' $$ showPatch y'
                    IsEq -> succeeded

-- | effect preserving  AB <--> B'A' then effect(AB) = effect(B'A')
effectPreserving :: (Patchy p, MightBeEmptyHunk p, RepoModel model, ApplyState p ~ RepoState model) =>
                        (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                     -> WithState model (p :> p) wA wB -> TestResult
effectPreserving _ (WithState _ (x :> _) _) | isEmptyHunk x = rejected
effectPreserving c (WithState r (x :> y) r')
  = case c (x :> y) of
    Nothing         -> rejected
    Just (y' :> x') ->
      case maybeFail $ repoApply r y' of
      Nothing  -> failed $ redText "y' is not applicable to r."
      Just r_y' ->
        case maybeFail $ repoApply r_y' x' of
        Nothing     -> failed $ redText "x' is not applicable to r_y'."
        Just r_y'x' -> if r_y'x' `eqModel` r'
                          then succeeded
                          else failed $ redText "r_y'x' is not equal to r'."

-- | patchAndInverseCommute   If AB ↔ B′A′ then A⁻¹B′ ↔ BA′⁻¹
patchAndInverseCommute :: (Patchy p, ShowPatchBasic p, MyEq p) =>
                             (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                          -> (p :> p) wA wB -> TestResult
patchAndInverseCommute c (x :> y) =
  case c (x :> y) of
  Nothing -> rejected
  Just (y' :> x') ->
     case c (invert x :> y') of
       Nothing -> failed (redText ""
                          $$ redText "-------- original commute (x :> y):"
                          $$ showPatch x $$ redText ":>" $$ showPatch y
                          $$ redText "-------- result (y' :> x'):"
                          $$ showPatch y' $$ redText ":>" $$ showPatch x'
                          $$ redText "-------- bad commute (invert x :> y'):"
                          $$ showPatch (invert x) $$ redText ":>" $$ showPatch y')
       Just (y'' :> ix') ->
           case y'' =\/= y of
           NotEq -> failed (redText "y'' /= y" $$
                            redText "x" $$ showPatch x $$
                            redText "y" $$ showPatch y $$
                            redText "x'" $$ showPatch x' $$
                            redText "y'" $$ showPatch y' $$
                            redText "y''" $$ showPatch y'' $$
                            redText ":> x'" $$ showPatch x')
           IsEq -> case x' =\/= invert ix' of
                   NotEq -> failed (redText "x' /= invert ix'" $$
                                    redText "y''" $$ showPatch y'' $$
                                    redText ":> x'" $$ showPatch x' $$
                                    redText "invert x" $$ showPatch (invert x) $$
                                    redText ":> y" $$ showPatch y $$
                                    redText "y'" $$ showPatch y' $$
                                    redText "ix'" $$ showPatch ix')
                   IsEq -> succeeded

permutivity :: (Patchy p, ShowPatchBasic p, MyEq p) => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
            -> (p :> p :> p) wA wB -> TestResult
permutivity c (x:>y:>z) =
  case c (x :> y) of
  Nothing -> rejected
  Just (y1 :> x1) ->
    case c (y :> z) of
    Nothing -> rejected
    Just (z2 :> y2) ->
      case c (x :> z2) of
      Nothing -> rejected
      Just (z3 :> x3) ->
        case c (x1 :> z) of
          Nothing -> failed $ redText "permutivity1"
          Just (z4 :> x4) ->
            --traceDoc (greenText "third commuted" $$
            --          greenText "about to commute" $$
            --          greenText "y1" $$ showPatch y1 $$
            --          greenText "z4" $$ showPatch z4) $
            case c (y1 :> z4) of
            Nothing -> failed $ redText "permutivity2"
            Just (z3_ :> y4)
                | IsEq <- z3_ =\/= z3 ->
                     --traceDoc (greenText "passed z3") $ error "foobar test" $
                     case c (y4 :> x4) of
                     Nothing -> failed $ redText "permutivity5: input was" $$
                                         redText "x" $$ showPatch x $$
                                         redText "y" $$ showPatch y $$
                                         redText "z" $$ showPatch z $$
                                         redText "z3" $$ showPatch z3 $$
                                         redText "failed commute of" $$
                                         redText "y4" $$ showPatch y4 $$
                                         redText "x4" $$ showPatch x4 $$
                                         redText "whereas commute of x and y give" $$
                                         redText "y1" $$ showPatch y1 $$
                                         redText "x1" $$ showPatch x1
                     Just (x3_ :> y2_)
                          | NotEq <- x3_ =\/= x3 -> failed $ redText "permutivity6"
                          | NotEq <- y2_ =/\= y2 -> failed $ redText "permutivity7"
                          | otherwise -> succeeded
                | otherwise ->
                    failed $ redText "permutivity failed" $$
                             redText "z3" $$ showPatch z3 $$
                             redText "z3_" $$ showPatch z3_

partialPermutivity :: (Patchy p, ShowPatchBasic p) => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                    -> (p :> p :> p) wA wB -> TestResult
partialPermutivity c (xx:>yy:>zz) = pp (xx:>yy:>zz) <&&> pp (invert zz:>invert yy:>invert xx)
    where pp (x:>y:>z) =
            case c (y :> z) of
            Nothing -> rejected
            Just (z1 :> y1) ->
              case c (x :> z1) of
              Nothing -> rejected
              Just (_ :> x1) ->
                case c (x :> y) of
                  Just _ -> rejected -- this is covered by full permutivity test above
                  Nothing ->
                      case c (x1 :> y1) of
                      Nothing -> succeeded
                      Just _ -> failed $ greenText "partialPermutivity error" $$
                                         greenText "x" $$ showPatch x $$
                                         greenText "y" $$ showPatch y $$
                                         greenText "z" $$ showPatch z

mergeArgumentsConsistent :: (Patchy p, ShowPatchBasic p) =>
                              (forall wX wY . p wX wY -> Maybe Doc)
                           -> (p :\/: p) wA wB -> TestResult
mergeArgumentsConsistent isConsistent (x :\/: y) =
  fromMaybe $
    msum [(\z -> redText "mergeArgumentsConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
          (\z -> redText "mergeArgumentsConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y]

mergeConsistent :: (Patchy p, ShowPatchBasic p, Merge p) =>
                           (forall wX wY . p wX wY -> Maybe Doc)
                        -> (p :\/: p) wA wB -> TestResult
mergeConsistent isConsistent (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' ->
      fromMaybe $
        msum [(\z -> redText "mergeConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
              (\z -> redText "mergeConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y,
              (\z -> redText "mergeConsistent x'" $$ showPatch x' $$ z $$
                     redText "where x' comes from x" $$ showPatch x $$
                     redText "and y" $$ showPatch y) `fmap` isConsistent x',
              (\z -> redText "mergeConsistent y'" $$ showPatch y' $$ z) `fmap` isConsistent y']

mergeEitherWay :: (Patchy p, MyEq p, Merge p) => (p :\/: p) wX wY -> TestResult
mergeEitherWay (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' -> case merge (y :\/: x) of
                  x'' :/\: y'' | IsEq <- x'' =\/= x',
                                 IsEq <- y'' =\/= y' -> succeeded
                               | otherwise -> failed $ redText "mergeEitherWay bug"

mergeCommute :: (Patchy p, MyEq p, ShowPatchBasic p, Merge p, MightHaveDuplicate p)
             => (p :\/: p) wX wY -> TestResult
mergeCommute (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x'
     -- this test unfortunately fails on some V2 patches that contain duplicates
     -- after the merge. While in theory the underlying bug should be fixed,
     -- we don't know how to and even if we did, it would probably involve a repository
     -- migration to a new patch type.
     | hasDuplicate x' || hasDuplicate y' -> rejected
     | otherwise ->
        case commute (x :> y') of
        Nothing -> failed $ redText "mergeCommute 1" $$
                            redText "x" $$ showPatch x $$
                            redText "y" $$ showPatch y $$
                            redText "x'" $$ showPatch x' $$
                            redText "y'" $$ showPatch y'
        Just (y_ :> x'_)
            | IsEq <- y_ =\/= y,
              IsEq <- x'_ =\/= x' ->
                      case commute (y :> x') of
                      Nothing -> failed $ redText "mergeCommute 2 failed" $$
                                          redText "x" $$ showPatch x $$
                                          redText "y" $$ showPatch y $$
                                          redText "x'" $$ showPatch x' $$
                                          redText "y'" $$ showPatch y'
                      Just (x_ :> y'_)
                           | IsEq <- x_ =\/= x,
                             IsEq <- y'_ =\/= y' -> succeeded
                           | otherwise -> failed $ redText "mergeCommute 3" $$
                                                   redText "x" $$ showPatch x $$
                                                   redText "y" $$ showPatch y $$
                                                   redText "x'" $$ showPatch x' $$
                                                   redText "y'" $$ showPatch y' $$
                                                   redText "x_" $$ showPatch x_ $$
                                                   redText "y'_" $$ showPatch y'_
            | otherwise -> failed $ redText "mergeCommute 4" $$
                                    redText "x" $$ showPatch x $$
                                    redText "y" $$ showPatch y $$
                                    redText "x'" $$ showPatch x' $$
                                    redText "y'" $$ showPatch y' $$
                                    redText "x'_" $$ showPatch x'_ $$
                                    redText "y_" $$ showPatch y_


-- | coalesce effect preserving
coalesceEffectPreserving
            :: (PrimPatch prim, RepoModel model, ApplyState prim ~ RepoState model )
            => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
            -> WithState model (prim :> prim) wA wB -> TestResult
coalesceEffectPreserving j (WithState r (a :> b) r') =
  case j (a :> b) of
       Nothing -> rejected
       Just x  -> case maybeFail $ repoApply r x of
                       Nothing  -> failed $ redText "x is not applicable to r."
                       Just r_x -> if r_x `eqModel` r'
                                      then succeeded
                                      else failed $ redText "r_x /= r'"

coalesceCommute
          :: (PrimPatch prim, MightBeEmptyHunk prim)
          => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
          -> (prim :> prim :> prim) wA wB -> TestResult
coalesceCommute _ (a :> _ :> _) | isEmptyHunk a = rejected
coalesceCommute j (a :> b :> c) =
    case j (b :> c) of
    Nothing -> rejected
    Just x  ->
       case commuteFLorComplain (a :> b :>: c :>: NilFL) of
        Right (b' :>: c' :>: NilFL :> a') ->
           case commute (a:>:NilFL :> x) of
             Just (x' :> a'':>:NilFL) ->
                 case a'' =/\= a' of
                 NotEq -> failed $ greenText "coalesceCommute 3"
                 IsEq -> case j (b' :> c') of
                         Nothing -> failed $ greenText "coalesceCommute 4"
                         Just x'' -> case x' =\/= x'' of
                                     NotEq -> failed $ greenText "coalesceCommute 5"
                                     IsEq -> succeeded
             _ -> failed $ greenText "coalesceCommute 1"
        _ -> rejected

show_read :: (Show2 p, MyEq p, ReadPatch p, ShowPatchBasic p, Patchy p) => p wA wB -> TestResult
show_read p = let ps = renderPS Standard (showPatch p)
              in case readPatch ps of
                 Nothing -> failed (redText "unable to read " $$ showPatch p)
                 Just (Sealed p'  ) | IsEq <- p' =\/= p -> succeeded
                                    | otherwise -> failed $ redText "trouble reading patch p" $$
                                                            showPatch p $$
                                                            redText "reads as p'" $$
                                                            showPatch p' $$
                                                            redText "aka" $$
                                                            greenText (show2 p) $$
                                                            redText "and" $$
                                                            greenText (show2 p')

-- vim: fileencoding=utf-8 :
