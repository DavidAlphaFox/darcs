--  Copyright (C) 2009 Ganesh Sittampalam
--
--  BSD3
{-# LANGUAGE CPP, GADTs, PatternGuards, TypeOperators, NoMonomorphismRestriction, ViewPatterns, UndecidableInstances #-}
module Darcs.Patch.Rebase
    ( Rebasing(..), RebaseItem(..), RebaseName(..), RebaseFixup(..)
    , simplifyPush, simplifyPushes
    , mkSuspended
    , takeHeadRebase, takeHeadRebaseFL, takeHeadRebaseRL
    , takeAnyRebase, takeAnyRebaseAndTrailingPatches
    , countToEdit
    ) where

import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat, copyListFormat )
import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.MaybeInternal ( MaybeInternal(..), InternalChecker(..) )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Named ( Named(..), patchcontents, namepatch
                         , commuterIdNamed
                         )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Patch.Patchy ( Invert(..), Commute(..), Patchy, Apply(..),
                            ShowPatch(..), ReadPatch(..),
                            PatchInspect(..)
                          )
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf, FromPrim(..), FromPrim(..), canonizeFL )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.Rebase.Name
    ( RebaseName(..)
    , commutePrimName, commuteNamePrim
    )
import Darcs.Patch.Rebase.NameHack ( NameHack(..) )
import Darcs.Patch.Rebase.Recontext ( RecontextRebase(..), RecontextRebase1(..), RecontextRebase2(..) )
import Darcs.Patch.Repair ( Check(..), RepairToFL(..) )
import Darcs.Patch.Set ( PatchSet(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.ReadMonads ( ParserM, lexString, myLex' )
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
    ( Show1(..), Show2(..), showsPrec2
    , ShowDict(ShowDictClass), appPrec
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm(MyersDiff) )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.Text ( formatParas )
import Darcs.Util.Printer ( vcat, text, blueText, ($$), (<+>) )

import Prelude hiding ( pi )
import Control.Applicative ( (<$>), (<|>) )
import Control.Arrow ( (***), second )
import Control.Monad ( when )
import Data.Maybe ( catMaybes )
import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

#include "impossible.h"

{- Notes

Note [Rebase representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The entire rebase state is stored in a single Suspended patch.

This is both unnatural and inefficient:

 - Unnatural because the rebase state is not really a patch and
   treating it as one requires various hacks:

   - It has to be given a fake name: see mkSuspended

   - Since 'Named p' actually contains 'FL p', we have to
     assume/assert that the FL either contains a sequence of Normals
     or a single Suspended

   - When 'Named ps' commutes past 'Named (Suspended items :> NilFL)',
     we need to inject the name from 'Named ps' into 'items', which
     is a layering violation: see Darcs.Patch.Rebase.NameHack

   - We need to hide the patch in the UI: see Darcs.Patch.MaybeInternal

   - We need a conditional hook so that amend-record can change the
     Suspended patch itself: see Darcs.Patch.Rebase.Recontext
     (something like this might be necessary no matter what the
     representation)

 - Inefficient because we need to write the entire rebase state out
   each time, even though most operations will only affect a small
   portion near the beginning.

   - This also means that we need to commute the rebase patch back
     to the head of the repo lazily: we only do so when a rebase
     operation requires it. Otherwise, pulling in 100 patches
     would entail writing out the entire rebase patch to disk 100
     times.

The obvious alternative is to store the rebase state at the repository
level, using inventories in some appropriate way.

The main reason this wasn't done is that the repository handling code
is quite fragile and hard to modify safely.

Also, rebase relies heavily on witnesses to check correctness, and
the witnesses on the Repository type are not as reliable as those
on patch types, partly because of the cruft in the repository code,
and partly because it's inherently harder to track witnesses when
the objects being manipulated are stored on disk and being changed
imperatively.

If and when the repository code becomes easier to work with, rebase
should be changed accordingly.

-}

-- TODO: move some of the docs of types to individual constructors
-- once http://trac.haskell.org/haddock/ticket/43 is fixed.

-- |A patch that lives in a repository where a rebase is in
-- progress. Such a repository will consist of @Normal@ patches
-- along with exactly one @Suspended@ patch.
--
-- Most rebase operations will require the @Suspended@ patch
-- to be at the end of the repository.
--
-- @Normal@ represents a normal patch within a respository where a
-- rebase is in progress. @Normal p@ is given the same on-disk
-- representation as @p@, so a repository can be switched into
-- and out of rebasing mode simply by adding or removing a
-- @Suspended@ patch and setting the appropriate format flag.
--
-- The single @Suspended@ patch contains the entire rebase
-- state, in the form of 'RebaseItem's.
--
-- Note that the witnesses are such that the @Suspended@
-- patch has no effect on the context of the rest of the
-- repository; in a sense the patches within it are
-- dangling off to one side from the main repository.
--
-- See Note [Rebase representation] in the source for a discussion
-- of the design choice to embed the rebase state in a single patch.
data Rebasing p wX wY where
    Normal    :: p wX wY -> Rebasing p wX wY
    Suspended :: FL (RebaseItem p) wX wY -> Rebasing p wX wX

instance (Show2 p, Show2 (PrimOf p)) => Show (Rebasing p wX wY) where
    showsPrec d (Normal p) =
        showParen (d > appPrec) $ showString "Darcs.Patch.Rebase.Normal " . showsPrec2 (appPrec + 1) p
    showsPrec d (Suspended p) =
        showParen (d > appPrec) $ showString "Darcs.Patch.Rebase.Suspended " . showsPrec2 (appPrec + 1) p

instance (Show2 p, Show2 (PrimOf p)) => Show1 (Rebasing p wX) where
    showDict1 = ShowDictClass

instance (Show2 p, Show2 (PrimOf p)) => Show2 (Rebasing p) where
    showDict2 = ShowDictClass

-- |A single item in the rebase state consists of either
-- a patch that is being edited, or a fixup that adjusts
-- the context so that a subsequent patch that is being edited
-- \"makes sense\".
--
-- @ToEdit@ holds a patch that is being edited. The name ('PatchInfo') of
-- the patch will typically be the name the patch had before
-- it was added to the rebase state; if it is moved back
-- into the repository it must be given a fresh name to account
-- for the fact that it will not necessarily have the same
-- dependencies as the original patch. This is typically
-- done by changing the @Ignore-This@ junk.
--
-- @Fixup@ adjusts the context so that a subsequent @ToEdit@ patch
-- is correct. Where possible, @Fixup@ changes are commuted
-- as far as possible into the rebase state, so any remaining
-- ones will typically cause a conflict when the @ToEdit@ patch
-- is moved back into the repository.
data RebaseItem p wX wY where
    ToEdit :: Named p wX wY -> RebaseItem p wX wY
    Fixup :: RebaseFixup p wX wY -> RebaseItem p wX wY

instance (Show2 p, Show2 (PrimOf p)) => Show (RebaseItem p wX wY) where
    showsPrec d (ToEdit p) =
        showParen (d > appPrec) $ showString "ToEdit " . showsPrec2 (appPrec + 1) p
    showsPrec d (Fixup p) =
        showParen (d > appPrec) $ showString "Fixup " . showsPrec2 (appPrec + 1) p

instance (Show2 p, Show2 (PrimOf p)) => Show1 (RebaseItem p wX) where
    showDict1 = ShowDictClass

instance (Show2 p, Show2 (PrimOf p)) => Show2 (RebaseItem p) where
    showDict2 = ShowDictClass

countToEdit :: FL (RebaseItem p) wX wY -> Int
countToEdit NilFL = 0
countToEdit (ToEdit _ :>: ps) = 1 + countToEdit ps
countToEdit (_ :>: ps) = countToEdit ps

commuterRebasing :: (PrimPatchBase p, Commute p, Invert p, FromPrim p, Effect p)
                 => D.DiffAlgorithm -> CommuteFn p p
                 -> CommuteFn (Rebasing p) (Rebasing p)
commuterRebasing _ commuter (Normal p :> Normal q) = do
    q' :> p' <- commuter (p :> q)
    return (Normal q' :> Normal p')

-- Two rebases in sequence must have the same starting context,
-- so they should trivially commute.
-- This case shouldn't actually happen since each repo only has
-- a single Suspended patch.
commuterRebasing _ _ (p@(Suspended _) :> q@(Suspended _)) =
    return (q :> p)

commuterRebasing da _ (Normal p :> Suspended qs) =
    return (unseal Suspended (addFixup da p qs) :> Normal p)

commuterRebasing da _ (Suspended ps :> Normal q) =
    return (Normal q :> unseal Suspended (addFixup da (invert q) ps))


instance (PrimPatchBase p, FromPrim p, Effect p, Invert p, Commute p) => Commute (Rebasing p) where
  commute = commuterRebasing D.MyersDiff commute

instance (PrimPatchBase p, FromPrim p, Effect p, Commute p) => NameHack (Rebasing p) where
  nameHack da = Just (pushIn . AddName, pushIn . DelName)
     where
           pushIn :: RebaseName p wX wX -> FL (Rebasing p) wX wY -> FL (Rebasing p) wX wY
           pushIn n (Suspended ps :>: NilFL) = unseal (\qs -> Suspended qs :>: NilFL) (simplifyPush da (NameFixup n) ps)
           pushIn _ ps = ps

instance (PrimPatchBase p, FromPrim p, Effect p, Invert p, Commute p, CommuteNoConflicts p) => CommuteNoConflicts (Rebasing p) where
  commuteNoConflicts = commuterRebasing D.MyersDiff commuteNoConflicts

instance (PrimPatchBase p, FromPrim p, Effect p, Invert p, Merge p) => Merge (Rebasing p) where
  merge (Normal p :\/: Normal q) = case merge (p :\/: q) of
                                     q' :/\: p' -> Normal q' :/\: Normal p'
  merge (p@(Suspended _) :\/: q@(Suspended _)) = q :/\: p
  merge (Normal p :\/: Suspended qs) = unseal Suspended (addFixup D.MyersDiff (invert p) qs) :/\: Normal p
  merge (Suspended ps :\/: Normal q) = Normal q :/\: unseal Suspended (addFixup D.MyersDiff (invert q) ps)

instance (PrimPatchBase p, PatchInspect p) => PatchInspect (Rebasing p) where
  listTouchedFiles (Normal p) = listTouchedFiles p
  listTouchedFiles (Suspended ps) = concat $ mapFL ltfItem ps
              where ltfItem :: RebaseItem p wX wY -> [FilePath]
                    ltfItem (ToEdit q) = listTouchedFiles q
                    ltfItem (Fixup (PrimFixup q)) = listTouchedFiles q
                    ltfItem (Fixup (NameFixup _)) = []
  hunkMatches f (Normal p) = hunkMatches f p
  hunkMatches f (Suspended ps) = or $ mapFL hmItem ps
              where hmItem :: RebaseItem p wA wB -> Bool
                    hmItem (ToEdit q) = hunkMatches f q
                    hmItem (Fixup (PrimFixup q)) = hunkMatches f q
                    hmItem (Fixup (NameFixup _)) = False

instance Invert p => Invert (Rebasing p) where
  invert (Normal p) = Normal (invert p)
  invert (Suspended ps) = Suspended ps -- TODO is this sensible?

instance Effect p => Effect (Rebasing p) where
  effect (Normal p) = effect p
  effect (Suspended _) = NilFL

instance (PrimPatchBase p, PatchListFormat p, Patchy p, FromPrim p, Conflict p, Effect p, CommuteNoConflicts p, IsHunk p)
    => Patchy (Rebasing p)

instance PatchDebug p => PatchDebug (Rebasing p)

instance ( PrimPatchBase p, PatchListFormat p, Patchy p
         , FromPrim p, Conflict p, Effect p
         , PatchInspect p
         , CommuteNoConflicts p, IsHunk p
         )
    => Matchable (Rebasing p)

instance (Conflict p, FromPrim p, Effect p, Invert p, Commute p) => Conflict (Rebasing p) where
   resolveConflicts (Normal p) = resolveConflicts p
   resolveConflicts (Suspended _) = []

instance Apply p => Apply (Rebasing p) where
   type ApplyState (Rebasing p) = ApplyState p
   apply (Normal p) = apply p
   apply (Suspended _) = return ()

instance (PrimPatchBase p, PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Rebasing p) where
   showPatch (Normal p) = showPatch p
   showPatch (Suspended ps) = blueText "rebase" <+> text "0.0" <+> blueText "{"
                              $$ vcat (mapFL showPatch ps)
                              $$ blueText "}"

instance (PrimPatchBase p, PatchListFormat p, Apply p, CommuteNoConflicts p, Conflict p, IsHunk p, ShowPatch p)
    => ShowPatch (Rebasing p) where

   summary (Normal p) = summary p
   summary (Suspended ps) = summaryFL ps
   summaryFL ps = vcat (mapFL summary ps) -- TODO sort out summaries properly, considering expected conflicts

instance (PrimPatchBase p, PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (RebaseItem p) where
   showPatch (ToEdit p) = blueText "rebase-toedit" <+> blueText "(" $$ showPatch p $$ blueText ")"
   showPatch (Fixup (PrimFixup p)) = blueText "rebase-fixup" <+> blueText "(" $$ showPatch p $$ blueText ")"
   showPatch (Fixup (NameFixup p)) = blueText "rebase-name" <+> blueText "(" $$ showPatch p $$ blueText ")"

instance (PrimPatchBase p, PatchListFormat p, Apply p, CommuteNoConflicts p, Conflict p, IsHunk p, ShowPatch p)
    => ShowPatch (RebaseItem p) where

   summary (ToEdit p) = summary p
   summary (Fixup (PrimFixup p)) = summary p
   summary (Fixup (NameFixup n)) = summary n
   summaryFL ps = vcat (mapFL summary ps) -- TODO sort out summaries properly, considering expected conflicts


instance (PrimPatchBase p, PatchListFormat p, ReadPatch p) => ReadPatch (RebaseItem p) where
   readPatch' = mapSeal ToEdit              <$> readWith (BC.pack "rebase-toedit") <|>
                mapSeal (Fixup . PrimFixup) <$> readWith (BC.pack "rebase-fixup" ) <|>
                mapSeal (Fixup . NameFixup) <$> readWith (BC.pack "rebase-name"  )
     where readWith :: forall m q wX . (ParserM m, ReadPatch q) => B.ByteString -> m (Sealed (q wX))
           readWith str = do lexString str
                             lexString (BC.pack "(")
                             res <- readPatch'
                             lexString (BC.pack ")")
                             return res

instance PrimPatchBase p => PrimPatchBase (Rebasing p) where
   type PrimOf (Rebasing p) = PrimOf p

instance (PrimPatchBase p, PatchListFormat p, ReadPatch p) => ReadPatch (Rebasing p) where
   readPatch' =
    do lexString (BC.pack "rebase")
       version <- myLex'
       when (version /= BC.pack "0.0") $ error $ "can't handle rebase version " ++ show version
       (lexString (BC.pack "{}") >> return (seal (Suspended NilFL)))
         <|>
         (unseal (Sealed . Suspended) <$> bracketedFL readPatch' '{' '}')
    <|> mapSeal Normal <$> readPatch'

instance IsHunk p => IsHunk (Rebasing p) where
   isHunk (Normal p) = isHunk p
   isHunk (Suspended _) = Nothing

instance FromPrim p => FromPrim (Rebasing p) where
   fromPrim p = Normal (fromPrim p)

instance Check p => Check (Rebasing p) where
   isInconsistent (Normal p) = isInconsistent p
   isInconsistent (Suspended ps) =
       case catMaybes (mapFL isInconsistent ps) of
         [] -> Nothing
         xs -> Just (vcat xs)

instance Check p => Check (RebaseItem p) where
   isInconsistent (Fixup _) = Nothing
   isInconsistent (ToEdit p) = isInconsistent p

instance RepairToFL p => RepairToFL (Rebasing p) where
   applyAndTryToFixFL (Normal p) = fmap (second $ mapFL_FL Normal) <$> applyAndTryToFixFL p
   -- TODO: ideally we would apply ps in a sandbox to check the individual patches
   -- are consistent with each other.
   applyAndTryToFixFL (Suspended ps) =
       return . fmap (unlines *** ((:>: NilFL) . Suspended)) $ repairInternal ps

-- Just repair the internals of the patch, without applying it to anything
-- or checking against an external context.
-- Included for the internal implementation of applyAndTryToFixFL for Rebasing,
-- consider either generalising it for use everywhere, or removing once
-- the implementation works in a sandbox and thus can use the "full" Repair on the
-- contained patches.
class RepairInternalFL p where
   repairInternalFL :: p wX wY -> Maybe ([String], FL p wX wY)

class RepairInternal p where
   repairInternal :: p wX wY -> Maybe ([String], p wX wY)

instance RepairInternalFL p => RepairInternal (FL p) where
   repairInternal NilFL = Nothing
   repairInternal (x :>: ys) =
     case (repairInternalFL x, repairInternal ys) of
       (Nothing      , Nothing)        -> Nothing
       (Just (e, rxs), Nothing)        -> Just (e      , rxs +>+ ys )
       (Nothing      , Just (e', rys)) -> Just (e'     , x   :>: rys)
       (Just (e, rxs), Just (e', rys)) -> Just (e ++ e', rxs +>+ rys)

instance RepairInternalFL (RebaseItem p) where
   repairInternalFL (ToEdit _) = Nothing
   repairInternalFL (Fixup p) = fmap (second $ mapFL_FL Fixup) $ repairInternalFL p

instance RepairInternalFL (RebaseFixup p) where
   repairInternalFL (PrimFixup _) = Nothing
   repairInternalFL (NameFixup _) = Nothing

instance PatchListFormat p => PatchListFormat (Rebasing p) where
   patchListFormat = copyListFormat (patchListFormat :: ListFormat p)

instance RepoPatch p => RepoPatch (Rebasing p)

instance (Commute p, PrimPatchBase p, FromPrim p, Effect p) => RecontextRebase (Rebasing p) where
   recontextRebase = Just (RecontextRebase1 recontext)
    where
          recontext :: forall wY wZ . Named (Rebasing p) wY wZ -> (EqCheck wY wZ, RecontextRebase2 (Rebasing p) wY wZ)

          recontext (patchcontents -> (Suspended ps :>: NilFL))
              = (IsEq, RecontextRebase2 (\fixups -> unseal mkSuspended(simplifyPushes D.MyersDiff (mapFL_FL translateFixup fixups) ps)))

          recontext _ = (NotEq, bug "trying to recontext rebase without rebase patch at head")

          translateFixup :: RebaseFixup (Rebasing p) wX wY -> RebaseFixup p wX wY
          translateFixup (PrimFixup p) = PrimFixup p
          translateFixup (NameFixup n) = NameFixup (translateName n)

          translateName :: RebaseName (Rebasing p) wX wY -> RebaseName p wX wY
          translateName (AddName name) = AddName name
          translateName (DelName name) = DelName name
          translateName (Rename old new) = Rename old new

instance MaybeInternal (Rebasing p) where
   patchInternalChecker = Just (InternalChecker rebaseIsInternal)
      where rebaseIsInternal :: FL (Rebasing p) wX wY -> EqCheck wX wY
            rebaseIsInternal (Suspended _ :>: NilFL) = IsEq
            rebaseIsInternal _ = NotEq

addFixup :: (PrimPatchBase p, Commute p, FromPrim p, Effect p) => D.DiffAlgorithm -> p wX wY -> FL (RebaseItem p) wY wZ -> Sealed (FL (RebaseItem p) wX)
addFixup da p = simplifyPushes da (mapFL_FL PrimFixup (effect p))

canonizeNamePair :: (RebaseName p :> RebaseName p) wX wY -> FL (RebaseName p) wX wY
canonizeNamePair (AddName n :> Rename old new) | n == old = AddName new :>: NilFL
canonizeNamePair (Rename old new :> DelName n) | n == new = DelName old :>: NilFL
canonizeNamePair (Rename old1 new1 :> Rename old2 new2) | new1 == old2 = Rename old1 new2 :>: NilFL
canonizeNamePair (n1 :> n2) = n1 :>: n2 :>: NilFL

-- |Given a list of rebase items, try to push a new fixup as far as possible into
-- the list as possible, using both commutation and coalescing. If the fixup
-- commutes past all the 'ToEdit' patches then it is dropped entirely.
simplifyPush :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
             => D.DiffAlgorithm -> RebaseFixup p wX wY -> FL (RebaseItem p) wY wZ -> Sealed (FL (RebaseItem p) wX)

simplifyPush _ _f NilFL = Sealed NilFL

simplifyPush da (PrimFixup f1) (Fixup (PrimFixup f2) :>: ps)
 | IsEq <- isInverse = Sealed ps
 | otherwise
   = case commute (f1 :> f2) of
       Nothing -> Sealed (mapFL_FL (Fixup . PrimFixup) (canonizeFL da (f1 :>: f2 :>: NilFL)) +>+ ps)
       Just (f2' :> f1') -> mapSeal (Fixup (PrimFixup f2') :>:) (simplifyPush da (PrimFixup f1') ps)
  where isInverse = invert f1 =\/= f2

simplifyPush da (PrimFixup f) (Fixup (NameFixup n) :>: ps)
    = case commutePrimName (f :> n) of
        n' :> f' -> mapSeal (Fixup (NameFixup n') :>:) (simplifyPush da (PrimFixup f') ps)

simplifyPush da (PrimFixup f) (ToEdit e :>: ps)
   = case commuterIdNamed selfCommuter (fromPrim f :> e) of
       Nothing -> Sealed (Fixup (PrimFixup f) :>: ToEdit e :>: ps)
       Just (e' :> f') -> mapSeal (ToEdit e' :>:) (simplifyPushes da (mapFL_FL PrimFixup (effect f')) ps)

simplifyPush da (NameFixup n1) (Fixup (NameFixup n2) :>: ps)
 | IsEq <- isInverse = Sealed ps
 | otherwise
   = case commute (n1 :> n2) of
       Nothing -> Sealed (mapFL_FL (Fixup . NameFixup) (canonizeNamePair (n1 :> n2)) +>+ ps)
       Just (n2' :> n1') -> mapSeal (Fixup (NameFixup n2') :>:) (simplifyPush da (NameFixup n1') ps)
  where isInverse = invert n1 =\/= n2

simplifyPush da (NameFixup n) (Fixup (PrimFixup f) :>: ps) =
    case commuteNamePrim (n :> f) of
      f' :> n' -> mapSeal (Fixup (PrimFixup f') :>:) (simplifyPush da (NameFixup n') ps)

simplifyPush da (NameFixup (AddName an)) (p@(ToEdit (NamedP pn deps _)) :>: ps)
  | an == pn = impossible
  | an `elem` deps = Sealed (Fixup (NameFixup (AddName an)) :>: p :>: ps)
  | otherwise = mapSeal (unsafeCoerceP p :>:) (simplifyPush da (NameFixup (AddName an)) ps)
simplifyPush da (NameFixup (DelName dn)) (p@(ToEdit (NamedP pn deps _)) :>: ps)
  -- this case can arise if a patch is suspended then a fresh copy is pulled from another repo
  | dn == pn = Sealed (Fixup (NameFixup (DelName dn)) :>: p :>: ps)
  | dn `elem` deps = impossible
  | otherwise = mapSeal (unsafeCoerceP p :>:) (simplifyPush da (NameFixup (DelName dn)) ps)
simplifyPush da (NameFixup (Rename old new)) (p@(ToEdit (NamedP pn deps body)) :>: ps)
  | old == pn = impossible
  | new == pn = impossible
  | old `elem` deps = impossible
  | new `elem` deps =
      let newdeps = map (\dep -> if new == dep then old else dep) deps
      in mapSeal (ToEdit (NamedP pn newdeps (unsafeCoerceP body)) :>:) (simplifyPush da (NameFixup (Rename old new)) ps)
  | otherwise = mapSeal (unsafeCoerceP p :>:) (simplifyPush da (NameFixup (Rename old new)) ps)

-- |Like 'simplifyPush' but for a list of fixups.
simplifyPushes :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
               => D.DiffAlgorithm -> FL (RebaseFixup p) wX wY -> FL (RebaseItem p) wY wZ -> Sealed (FL (RebaseItem p) wX)
simplifyPushes _ NilFL ps = Sealed ps
simplifyPushes da (f :>: fs) ps = unseal (simplifyPush da f) (simplifyPushes da fs ps)

mkSuspended :: FL (RebaseItem p) wX wY -> IO (Named (Rebasing p) wX wX)
mkSuspended ps = do
     let name = "DO NOT TOUCH: Rebase patch"
     let desc = formatParas 72
                ["This patch is an internal implementation detail of rebase, used to store suspended patches, " ++
                 "and should not be visible in the user interface. Please report a bug if a darcs " ++
                 "command is showing you this patch."]
     date <- getIsoDateTime
     let author = "Invalid <invalid@invalid>"
     namepatch date name author desc (Suspended ps :>: NilFL)

-- |given the repository contents, get the rebase container patch, and its contents
-- The rebase patch can be anywhere in the repository and is returned without being
-- commuted to the end.
takeAnyRebase ::  PatchSet (Rebasing p) wA wB
              -> (Sealed2 (PatchInfoAnd (Rebasing p)),
                  Sealed2 (FL (RebaseItem p)))
takeAnyRebase (PatchSet NilRL _) =
   -- it should never be behind a tag so we can stop now
   error "internal error: no suspended patch found"
takeAnyRebase (PatchSet (p :<: ps) pss)
    | Suspended rs :>: NilFL <- patchcontents (hopefully p) = (Sealed2 p, Sealed2 rs)
    | otherwise = takeAnyRebase (PatchSet ps pss)

-- |given the repository contents, get the rebase container patch, its contents, and the
-- rest of the repository contents. Commutes the patch to the end of the repository
-- if necessary. The rebase patch must be at the head of the repository.
takeAnyRebaseAndTrailingPatches
               :: PatchSet (Rebasing p) wA wB
               -> FlippedSeal (PatchInfoAnd (Rebasing p) :> RL (PatchInfoAnd (Rebasing p))) wB
takeAnyRebaseAndTrailingPatches (PatchSet NilRL _) =
   -- it should never be behind a tag so we can stop now
   error "internal error: no suspended patch found"
takeAnyRebaseAndTrailingPatches (PatchSet (p :<: ps) pss)
    | Suspended _ :>: NilFL <- patchcontents (hopefully p) = FlippedSeal (p :> NilRL)
    | otherwise = case takeAnyRebaseAndTrailingPatches (PatchSet ps pss) of
                     FlippedSeal (r :> ps') -> FlippedSeal (r :> (p :<: ps'))

-- |given the repository contents, get the rebase container patch, its contents, and the
-- rest of the repository contents. The rebase patch must be at the head of the repository.
takeHeadRebase :: PatchSet (Rebasing p) wA wB
               -> (PatchInfoAnd (Rebasing p) wB wB,
                   Sealed (FL (RebaseItem p) wB),
                   PatchSet (Rebasing p) wA wB)
takeHeadRebase (PatchSet NilRL _) = error "internal error: must have a rebase container patch at end of repository"
takeHeadRebase (PatchSet (p :<: ps) pss)
    | Suspended rs :>: NilFL <- patchcontents (hopefully p) = (p, Sealed rs, PatchSet ps pss)
    | otherwise = error "internal error: must have a rebase container patch at end of repository"

takeHeadRebaseRL :: RL (PatchInfoAnd (Rebasing p)) wA wB
                 -> (PatchInfoAnd (Rebasing p) wB wB,
                     Sealed (FL (RebaseItem p) wB),
                     RL (PatchInfoAnd (Rebasing p)) wA wB)
takeHeadRebaseRL NilRL = error "internal error: must have a suspended patch at end of repository"
takeHeadRebaseRL (p :<: ps)
    | Suspended rs :>: NilFL <- patchcontents (hopefully p) = (p, Sealed rs, ps)
    | otherwise = error "internal error: must have a suspended patch at end of repository"

takeHeadRebaseFL :: FL (PatchInfoAnd (Rebasing p)) wA wB
                 -> (PatchInfoAnd (Rebasing p) wB wB,
                     Sealed (FL (RebaseItem p) wB),
                     FL (PatchInfoAnd (Rebasing p)) wA wB)
takeHeadRebaseFL ps = let (a, b, c) = takeHeadRebaseRL (reverseFL ps) in (a, b, reverseRL c)

