--  Copyright (C) 2011-2 Ganesh Sittampalam
--
--  BSD3

{-# LANGUAGE CPP #-}
module Darcs.Patch.Rebase.Name
    ( RebaseName(..)
    , commuteNamePrim, commutePrimName
    , commuteNameNamed, commuteNamedName
    ) where

import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Info ( PatchInfo, isInverted, showPatchInfo, readPatchInfo )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Patchy
    ( Invert(..), Commute(..), Patchy, Apply(..)
    , ShowPatch(..), ReadPatch(..)
    )
import Darcs.Patch.Permutations ( inverseCommuter )
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf )
import Darcs.Patch.ReadMonads ( lexString )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Witnesses.Eq ( MyEq(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..)
    , ShowDict(ShowDictClass)
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.Printer ( empty, blueText, ($$) )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString.Char8 as BC ( pack )

#include "impossible.h"

-- Note: in principle this is a general concept not limited to
-- rebase, and we might be able to generalise this type and
-- refactor named patches to use it too.
-- | A 'RebaseName' encapsulates the concept of the name of a patch,
-- without any contents. This allows us to track explicit dependencies
-- in the rebase state, changing them to follow uses of amend-record
-- or unsuspend on a depended-on patch, and warning the user if any
-- are lost entirely.
data RebaseName (p :: * -> * -> *) wX wY where
  AddName :: PatchInfo -> RebaseName p wX wY
  DelName :: PatchInfo -> RebaseName p wX wY
  Rename :: PatchInfo -> PatchInfo -> RebaseName p wX wY
    deriving Show

instance Show1 (RebaseName p wX) where
    showDict1 = ShowDictClass

instance Show2 (RebaseName p) where
    showDict2 = ShowDictClass

instance ShowPatchBasic (RebaseName p) where
   showPatch (AddName n) = blueText "addname" $$ showPatchInfo n
   showPatch (DelName n) = blueText "delname" $$ showPatchInfo n
   showPatch (Rename old new) = blueText "rename" $$ showPatchInfo old $$ showPatchInfo new

instance ShowPatch (RebaseName p) where
   summary _ = empty -- TODO improve this?
   summaryFL _ = empty

instance ReadPatch (RebaseName p) where
   readPatch' = readAddName <|> readDelName <|> readRename
     where
       readAddName = do lexString (BC.pack "addname")
                        n <- readPatchInfo
                        return (Sealed (AddName n))
       readDelName = do lexString (BC.pack "delname")
                        n <- readPatchInfo
                        return (Sealed (DelName n))
       readRename  = do lexString (BC.pack "rename")
                        old <- readPatchInfo
                        new <- readPatchInfo
                        return (Sealed (Rename old new))

instance Commute (RebaseName p) where
   commute (AddName n1 :> AddName n2)
      | n1 == n2 = impossible
      | otherwise = Just (AddName n2 :> AddName n1)
   commute (DelName n1 :> DelName n2)
      | n1 == n2 = impossible
      | otherwise = Just (DelName n2 :> DelName n1)
   commute (AddName n1 :> DelName n2)
      | n1 /= n2 = Just (DelName n2 :> AddName n1)
      | otherwise = Nothing
   commute (DelName n1 :> AddName n2)
      | n1 /= n2 = Just (AddName n2 :> DelName n1)
      | otherwise = Nothing
   commute (Rename old new :> AddName n)
      | n == old = Nothing
      | n == new = impossible -- precondition of Add is that n doesn't exist
      | otherwise = Just (AddName n :> Rename old new)
   commute (AddName n :> Rename old new)
      | n == old = Nothing
      | n == new = impossible -- precondition of Rename is that new doesn't exist
      | otherwise = Just (Rename old new :> AddName n)
   commute (Rename old new :> DelName n)
      | n == old = impossible -- precondition of Del is that n does exist
      | n == new = Nothing
      | otherwise = Just (DelName n :> Rename old new)
   commute (DelName n :> Rename old new)
      | n == old = impossible -- precondition of Rename is that old does exist
      | n == new = Nothing
      | otherwise = Just (Rename old new :> DelName n)
   commute (Rename old1 new1 :> Rename old2 new2)
      | old1 == old2 = impossible
      | new1 == new2 = impossible
      | old1 == new2 = Nothing
      | new1 == old2 = Nothing
      | otherwise = Just (Rename old2 new2 :> Rename old1 new1)

instance Invert (RebaseName p) where
   invert (AddName n) = DelName n
   invert (DelName n) = AddName n
   invert (Rename old new) = Rename new old

instance PatchInspect (RebaseName p) where
    listTouchedFiles _ = []
    hunkMatches _ _ = False

instance Apply p => Apply (RebaseName p) where
   type ApplyState (RebaseName p) = ApplyState p
   apply _ = return ()

instance Apply p => Patchy (RebaseName p)

instance PrimPatchBase p => PrimPatchBase (RebaseName p) where
   type PrimOf (RebaseName p) = PrimOf p

instance Effect (RebaseName p) where
   effect _ = unsafeCoerceP NilFL

instance MyEq (RebaseName p) where
   AddName n1 `unsafeCompare` AddName n2 = n1 == n2
   AddName _ `unsafeCompare` _ = False
   _ `unsafeCompare` AddName _ = False

   DelName n1 `unsafeCompare` DelName n2 = n1 == n2
   DelName _ `unsafeCompare` _ = False
   _ `unsafeCompare` DelName _ = False

   Rename old1 new1 `unsafeCompare` Rename old2 new2 = old1 == old2 && new1 == new2
   -- Rename _ _  `unsafeCompare` _ = False
   -- _ `unsafeCompare` Rename _ _  = False


-- |Commute a name patch and a primitive patch. They trivially
-- commute so this just involves changing the witnesses.
commuteNamePrim :: PrimPatchBase p => (RebaseName p :> PrimOf p) wX wY -> (PrimOf p :> RebaseName p) wX wY
commuteNamePrim (n :> f) = unsafeCoerceP f :> unsafeCoerceP n

-- |Commute a primitive patch and a name patch. They trivially
-- commute so this just involves changing the witnesses.
commutePrimName :: PrimPatchBase p => (PrimOf p :> RebaseName p) wX wY -> (RebaseName p :> PrimOf p) wX wY
commutePrimName (f :> n) = unsafeCoerceP n :> unsafeCoerceP f

-- |Commute a name patch and a named patch. In most cases this is
-- trivial but we do need to check explicit dependencies.
commuteNameNamed :: Invert p => CommuteFn (RebaseName p) (Named p)
commuteNameNamed pair@(_ :> NamedP pn _ _)
  | isInverted pn = inverseCommuter commuteNamedName pair
commuteNameNamed (AddName an :> p@(NamedP pn deps _))
  | an == pn = impossible
  | an `elem` deps = Nothing
  | otherwise = Just (unsafeCoerceP p :> AddName an)
commuteNameNamed (DelName dn :> p@(NamedP pn deps _))
  | dn == pn = impossible
  | dn `elem` deps = impossible
  | otherwise = Just (unsafeCoerceP p :> DelName dn)
commuteNameNamed (Rename old new :> NamedP pn deps body)
  | old == pn = impossible
  | new == pn = impossible
  | old `elem` deps = impossible
  | otherwise =
      let newdeps = map (\dep -> if new == dep then old else dep) deps
      in Just (NamedP pn newdeps (unsafeCoerceP body) :> Rename old new)

-- |Commute a named patch and a name patch. In most cases this is
-- trivial but we do need to check explicit dependencies.
commuteNamedName :: Invert p => CommuteFn (Named p) (RebaseName p)
commuteNamedName pair@(NamedP pn _ _ :> _)
  | isInverted pn = inverseCommuter commuteNameNamed pair
commuteNamedName (p@(NamedP pn deps _) :> AddName an)
  | an == pn = impossible  -- the NamedP introduces pn, then AddName introduces it again
  | an `elem` deps = impossible -- the NamedP depends on an before it is introduced
  | otherwise = Just (AddName an :> unsafeCoerceP p)
commuteNamedName (p@(NamedP pn deps _) :> DelName dn)
  | dn == pn = Nothing
  | dn `elem` deps = Nothing
  | otherwise = Just (DelName dn :> unsafeCoerceP p)
commuteNamedName (NamedP pn deps body :> Rename old new)
  | old == pn = Nothing
  | new == pn = impossible
  | new `elem` deps = impossible
  | otherwise =
      let newdeps = map (\dep -> if old == dep then new else dep) deps
      in Just (Rename old new :> NamedP pn newdeps (unsafeCoerceP body))

