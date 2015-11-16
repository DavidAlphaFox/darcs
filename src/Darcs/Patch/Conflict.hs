module Darcs.Patch.Conflict
    ( Conflict(..), CommuteNoConflicts(..)
    , IsConflictedPrim(..), ConflictState(..) )
    where

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations ()
import Darcs.Patch.Prim.Class ( PrimOf )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..)
    , mapFL, reverseFL, mapRL, reverseRL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed, unseal )
import Darcs.Patch.Witnesses.Show ( Show2, showsPrec2 )
import Darcs.Util.Show ( appPrec )
import Data.List.Ordered ( nubSort )


class (Effect p, PatchInspect (PrimOf p)) => Conflict p where
    listConflictedFiles :: p wX wY -> [FilePath]
    listConflictedFiles p =
        nubSort $ concatMap (unseal listTouchedFiles) $ concat $ resolveConflicts p
    resolveConflicts :: p wX wY -> [[Sealed (FL (PrimOf p) wY)]]

    conflictedEffect :: p wX wY -> [IsConflictedPrim (PrimOf p)]
    conflictedEffect x = case listConflictedFiles x of
                         [] -> mapFL (IsC Okay) $ effect x
                         _ -> mapFL (IsC Conflicted) $ effect x

class CommuteNoConflicts p where
    -- | If 'commuteNoConflicts' @x :> y@ succeeds, we know that that @x@ commutes
    --   past @y@ without any conflicts.   This function is useful for patch types
    --   for which 'commute' is defined to always succeed; so we need some way to
    --   pick out the specific cases where commutation succeeds without any conflicts.
    commuteNoConflicts :: (p :> p) wX wY -> Maybe ((p :> p) wX wY)

instance (CommuteNoConflicts p, Conflict p) => Conflict (FL p) where
    listConflictedFiles = nubSort . concat . mapFL listConflictedFiles
    resolveConflicts NilFL = []
    resolveConflicts x = resolveConflicts $ reverseFL x
    conflictedEffect = concat . mapFL conflictedEffect

instance CommuteNoConflicts p => CommuteNoConflicts (FL p) where
    commuteNoConflicts (NilFL :> x) = Just (x :> NilFL)
    commuteNoConflicts (x :> NilFL) = Just (NilFL :> x)
    commuteNoConflicts (xs :> ys) =   do ys' :> rxs' <- commuteNoConflictsRLFL (reverseFL xs :> ys)
                                         return $ ys' :> reverseRL rxs'

instance (CommuteNoConflicts p, Conflict p) => Conflict (RL p) where
    listConflictedFiles = nubSort . concat . mapRL listConflictedFiles
    resolveConflicts x = rcs x NilFL
        where rcs :: RL p wX wY -> FL p wY wW -> [[Sealed (FL (PrimOf p) wW)]]
              rcs NilRL _ = []
              rcs (p:<:ps) passedby | (_:_) <- resolveConflicts p =
                  case commuteNoConflictsFL (p:>passedby) of
                    Just (_:> p') -> resolveConflicts p' ++ rcs ps (p:>:passedby)
                    Nothing -> rcs ps (p:>:passedby)
              rcs (p:<:ps) passedby = seq passedby $ rcs ps (p:>:passedby)
    conflictedEffect = concat . reverse . mapRL conflictedEffect

instance CommuteNoConflicts p => CommuteNoConflicts (RL p) where
    commuteNoConflicts (NilRL :> x) = Just (x :> NilRL)
    commuteNoConflicts (x :> NilRL) = Just (NilRL :> x)
    commuteNoConflicts (xs :> ys) =   do ys' :> rxs' <- commuteNoConflictsRLFL (xs :> reverseRL ys)
                                         return $ reverseFL ys' :> rxs'

data IsConflictedPrim prim where
    IsC :: !ConflictState -> !(prim wX wY) -> IsConflictedPrim prim
data ConflictState = Okay | Conflicted | Duplicated deriving ( Eq, Ord, Show, Read)

instance Show2 prim => Show (IsConflictedPrim prim) where
    showsPrec d (IsC cs prim) =
        showParen (d > appPrec) $
            showString "IsC " . showsPrec (appPrec + 1) cs .
            showString " " . showsPrec2 (appPrec + 1) prim

commuteNoConflictsFL :: CommuteNoConflicts p => (p :> FL p) wX wY -> Maybe ((FL p :> p) wX wY)
commuteNoConflictsFL (p :> NilFL) = Just (NilFL :> p)
commuteNoConflictsFL (q :> p :>: ps) =   do p' :> q' <- commuteNoConflicts (q :> p)
                                            ps' :> q'' <- commuteNoConflictsFL (q' :> ps)
                                            return (p' :>: ps' :> q'')

commuteNoConflictsRL :: CommuteNoConflicts p => (RL p :> p) wX wY -> Maybe ((p :> RL p) wX wY)
commuteNoConflictsRL (NilRL :> p) = Just (p :> NilRL)
commuteNoConflictsRL (p :<: ps :> q) =   do q' :> p' <- commuteNoConflicts (p :> q)
                                            q'' :> ps' <- commuteNoConflictsRL (ps :> q')
                                            return (q'' :> p' :<: ps')

commuteNoConflictsRLFL :: CommuteNoConflicts p => (RL p :> FL p) wX wY -> Maybe ((FL p :> RL p) wX wY)
commuteNoConflictsRLFL (NilRL :> ys) = Just (ys :> NilRL)
commuteNoConflictsRLFL (xs :> NilFL) = Just (NilFL :> xs)
commuteNoConflictsRLFL (xs :> y :>: ys) =   do y' :> xs' <- commuteNoConflictsRL (xs :> y)
                                               ys' :> xs'' <- commuteNoConflictsRLFL (xs' :> ys)
                                               return (y' :>: ys' :> xs'')

