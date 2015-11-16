--  Copyright (C) 2011-2 Ganesh Sittampalam 
--
--  BSD3

{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Fixup
    ( RebaseFixup(..)
    , commuteNamedFixup, commuteFixupNamed, commuteNamedFixups
    , flToNamesPrims
    ) where

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..), selfCommuter )
import Darcs.Patch.CommuteFn ( totalCommuterIdFL )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Prim ( FromPrim(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Named ( Named(..), commuterNamedId, commuterIdNamed )
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf )
import Darcs.Patch.Rebase.Name
    ( RebaseName
    , commuteNamedName, commuteNameNamed
    , commutePrimName, commuteNamePrim
    )
import Darcs.Patch.Witnesses.Eq ( MyEq(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), mapFL_FL, (:>)(..), (+>+) )
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..), showsPrec2
    , ShowDict(ShowDictClass), appPrec
    )

-- |A single rebase fixup, needed to ensure that the actual patches
-- being stored in the rebase state have the correct context.
data RebaseFixup p wX wY where
  PrimFixup :: PrimOf p wX wY -> RebaseFixup p wX wY
  NameFixup :: RebaseName p wX wY -> RebaseFixup p wX wY

instance Show2 (PrimOf p) => Show (RebaseFixup p wX wY) where
    showsPrec d (PrimFixup p) =
        showParen (d > appPrec) $ showString "PrimFixup " . showsPrec2 (appPrec + 1) p
    showsPrec d (NameFixup p) =
        showParen (d > appPrec) $ showString "NameFixup " . showsPrec2 (appPrec + 1) p

instance Show2 (PrimOf p) => Show1 (RebaseFixup p wX) where
    showDict1 = ShowDictClass

instance Show2 (PrimOf p) => Show2 (RebaseFixup p) where
    showDict2 = ShowDictClass

instance PrimPatchBase p => PrimPatchBase (RebaseFixup p) where
    type PrimOf (RebaseFixup p) = PrimOf p

instance (PrimPatchBase p, Apply p, ApplyState p ~ ApplyState (PrimOf p)) => Apply (RebaseFixup p) where
    type ApplyState (RebaseFixup p) = ApplyState p
    apply (PrimFixup p) = apply p
    apply (NameFixup p) = apply p

instance Effect (RebaseFixup p) where
    effect (PrimFixup p) = p :>: NilFL
    effect (NameFixup p) = effect p

instance MyEq (PrimOf p) => MyEq (RebaseFixup p) where
    PrimFixup p1 `unsafeCompare` PrimFixup p2 = p1 `unsafeCompare` p2
    PrimFixup _ `unsafeCompare` _ = False
    _ `unsafeCompare` PrimFixup _ = False

    NameFixup n1 `unsafeCompare` NameFixup n2 = n1 `unsafeCompare` n2
    -- NameFixup _ `unsafeCompare` _ = False
    -- _ `unsafeCompare` NameFixup _ = False

instance Invert (PrimOf p) => Invert (RebaseFixup p) where
    invert (PrimFixup p) = PrimFixup (invert p)
    invert (NameFixup n) = NameFixup (invert n)

instance PatchInspect (PrimOf p) => PatchInspect (RebaseFixup p) where
    listTouchedFiles (PrimFixup p) = listTouchedFiles p
    listTouchedFiles (NameFixup n) = listTouchedFiles n

    hunkMatches f (PrimFixup p) = hunkMatches f p
    hunkMatches f (NameFixup n) = hunkMatches f n

instance PrimPatchBase p => Commute (RebaseFixup p) where
    commute (PrimFixup p :> PrimFixup q) = do
        q' :> p' <- commute (p :> q)
        return (PrimFixup q' :> PrimFixup p')

    commute (NameFixup p :> NameFixup q) = do
        q' :> p' <- commute (p :> q)
        return (NameFixup q' :> NameFixup p')

    commute (PrimFixup p :> NameFixup q) = do
        q' :> p' <- return $ commutePrimName (p :> q)
        return (NameFixup q' :> PrimFixup p')

    commute (NameFixup p :> PrimFixup q) = do
        q' :> p' <- return $ commuteNamePrim (p :> q)
        return (PrimFixup q' :> NameFixup p')

-- |Split a sequence of fixups into names and prims
flToNamesPrims :: PrimPatchBase p
               => FL (RebaseFixup p) wX wY
               -> (FL (RebaseName p) :> FL (PrimOf p)) wX wY
flToNamesPrims NilFL = NilFL :> NilFL
flToNamesPrims (NameFixup n :>: fs) =
    case flToNamesPrims fs of
        names :> prims -> (n :>: names) :> prims
flToNamesPrims (PrimFixup p :>: fs) =
    case flToNamesPrims fs of
        names :> prims ->
            case totalCommuterIdFL commutePrimName (p :> names) of
                names' :> p' -> names' :> (p' :>: prims)

-- Note that this produces a list result because of the need to use effect to
-- extract the result.
-- Some general infrastructure for commuting p with PrimOf p would be helpful here,
commuteNamedPrim :: (FromPrim p, Effect p, Commute p)
                 => (Named p :> PrimOf p) wX wY
                 -> Maybe ((FL (PrimOf p) :> Named p) wX wY)
commuteNamedPrim (p :> q) = do
    q' :> p' <- commuterNamedId selfCommuter (p :> fromPrim q)
    return (effect q' :> p')

commutePrimNamed :: (FromPrim p, Effect p, Commute p)
                 => (PrimOf p :> Named p) wX wY
                 -> Maybe ((Named p :> FL (PrimOf p)) wX wY)
commutePrimNamed (p :> q) = do
    q' :> p' <- commuterIdNamed selfCommuter (fromPrim p :> q)
    return (q' :> effect p')

commuteNamedFixup :: (FromPrim p, Effect p, Commute p, Invert p)
                  => (Named p :> RebaseFixup p) wX wY
                  -> Maybe ((FL (RebaseFixup p) :> Named p) wX wY)
commuteNamedFixup (p :> PrimFixup q) = do
    qs' :> p' <- commuteNamedPrim (p :> q)
    return (mapFL_FL PrimFixup qs' :> p')
commuteNamedFixup (p :> NameFixup n) = do
    n' :> p' <- commuteNamedName (p :> n)
    return ((NameFixup n' :>: NilFL) :> p')


commuteNamedFixups :: (FromPrim p, Effect p, Commute p, Invert p)
                   => (Named p :> FL (RebaseFixup p)) wX wY
                   -> Maybe ((FL (RebaseFixup p) :> Named p) wX wY)
commuteNamedFixups (p :> NilFL) = return (NilFL :> p)
commuteNamedFixups (p :> (q :>: rs)) = do
    qs' :> p' <- commuteNamedFixup (p :> q)
    rs' :> p'' <- commuteNamedFixups (p' :> rs)
    return ((qs' +>+ rs') :> p'')


commuteFixupNamed :: (FromPrim p, Effect p, Commute p, Invert p)
                  => (RebaseFixup p :> Named p) wX wY
                  -> Maybe ((Named p :> FL (RebaseFixup p)) wX wY)
commuteFixupNamed (PrimFixup p :> q) = do
    q' :> ps' <- commutePrimNamed (p :> q)
    return (q' :> mapFL_FL PrimFixup ps')
commuteFixupNamed (NameFixup n :> q) = do
    q' :> n' <- commuteNameNamed (n :> q)
    return (q' :> (NameFixup n' :>: NilFL))