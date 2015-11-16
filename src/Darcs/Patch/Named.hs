--  Copyright (C) 2002-2003 David Roundy
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


module Darcs.Patch.Named
       ( Named(..),
         infopatch,
         adddeps, namepatch, anonymous,
         getdeps,
         patch2patchinfo, patchname, patchcontents,
         fmapNamed, fmapFL_Named,
         commuterIdNamed, commuterNamedId,
         mergerIdNamed
       )
       where

import Prelude hiding ( pi )
import Darcs.Patch.CommuteFn ( CommuteFn, commuterIdFL, commuterFLId
                             , MergeFn, mergerIdFL )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(effect, effectRL) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo, patchinfo,
                          showPatchInfoUI, makePatchname, invertName )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Commute(..), Invert(..), Apply(..),
                            PatchInspect(..), ReadPatch(..) )
import Darcs.Patch.Prim ( PrimOf, PrimPatchBase )
import Darcs.Patch.ReadMonads ( ParserM, option, lexChar,
                                choice, skipWhile, anyChar )
import Darcs.Patch.Rebase.NameHack ( NameHack(..) )
import Darcs.Patch.Repair ( mapMaybeSnd, Repair(..), RepairToFL, Check(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), showNamedPrefix )
import Darcs.Patch.Summary ( plainSummary )
import Darcs.Patch.Viewing () -- for ShowPatch FL instances

import Darcs.Patch.Witnesses.Eq ( MyEq(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), (:\/:)(..), (:/\:)(..), FL, mapFL, mapFL_FL )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Patch.Witnesses.Show ( ShowDict(..), Show1(..), Show2(..) )

import Darcs.Util.Diff ( DiffAlgorithm(MyersDiff) )
import Darcs.Util.Printer ( ($$), (<+>), (<>), prefix, text, vcat )

import Data.Maybe ( fromMaybe )

-- | The @Named@ type adds a patch info about a patch, that is a name.
data Named p wX wY where
    NamedP :: !PatchInfo
           -> ![PatchInfo]
           -> !(FL p wX wY)
           -> Named p wX wY
   deriving Show
-- ^ @NamedP info deps p@ represents patch @p@ with name
-- @info@. @deps@ is a list of dependencies added at the named patch
-- level, compared with the unnamed level (ie, dependencies added with
-- @darcs record --ask-deps@).

instance PrimPatchBase p => PrimPatchBase (Named p) where
    type PrimOf (Named p) = PrimOf p

instance Effect p => Effect (Named p) where
    effect (NamedP _ _ p) = effect p
    effectRL (NamedP _ _ p) = effectRL p

instance IsHunk (Named p) where
    isHunk _ = Nothing

instance PatchListFormat (Named p)

instance (ReadPatch p, PatchListFormat p) => ReadPatch (Named p) where
 readPatch' = readNamed

readNamed :: (ReadPatch p, PatchListFormat p, ParserM m) => m (Sealed (Named p wX))
readNamed
          = do n <- readPatchInfo
               d <- readDepends
               p <- readPatch'
               return $ (NamedP n d) `mapSeal` p

readDepends :: ParserM m => m [PatchInfo]
readDepends =
  option [] $ do lexChar '<'
                 readPis

readPis :: ParserM m => m [PatchInfo]
readPis = choice [ do pi <- readPatchInfo
                      pis <- readPis
                      return (pi:pis)
                 , do skipWhile (/= '>')
                      _ <- anyChar
                      return [] ]

instance Apply p => Apply (Named p) where
    type ApplyState (Named p) = ApplyState p
    apply (NamedP _ _ p) = apply p

instance RepairToFL p => Repair (Named p) where
    applyAndTryToFix (NamedP n d p) = mapMaybeSnd (NamedP n d) `fmap` applyAndTryToFix p

namepatch :: String -> String -> String -> [String] -> FL p wX wY -> IO (Named p wX wY)
namepatch date name author desc p
    | '\n' `elem` name = error "Patch names cannot contain newlines."
    | otherwise = do pinf <- patchinfo date name author desc
                     return $ NamedP pinf [] p

anonymous :: FL p wX wY -> IO (Named p wX wY)
anonymous p = namepatch "today" "anonymous" "unknown" ["anonymous"] p

infopatch :: PatchInfo -> FL p wX wY -> Named p wX wY
infopatch pi p = NamedP pi [] p

adddeps :: Named p wX wY -> [PatchInfo] -> Named p wX wY
adddeps (NamedP pi _ p) ds = NamedP pi ds p

getdeps :: Named p wX wY -> [PatchInfo]
getdeps (NamedP _ ds _) = ds

patch2patchinfo :: Named p wX wY -> PatchInfo
patch2patchinfo (NamedP i _ _) = i

patchname :: Named p wX wY -> String
patchname (NamedP i _ _) = show $ makePatchname i

patchcontents :: Named p wX wY -> FL p wX wY
patchcontents (NamedP _ _ p) = p

fmapNamed :: (forall wA wB . p wA wB -> q wA wB) -> Named p wX wY -> Named q wX wY
fmapNamed f (NamedP i deps p) = NamedP i deps (mapFL_FL f p)

fmapFL_Named :: (FL p wA wB -> FL q wC wD) -> Named p wA wB -> Named q wC wD
fmapFL_Named f (NamedP i deps p) = NamedP i deps (f p)

instance (Commute p, MyEq p) => MyEq (Named p) where
    unsafeCompare (NamedP n1 d1 p1) (NamedP n2 d2 p2) =
        n1 == n2 && d1 == d2 && unsafeCompare p1 p2

instance Invert p => Invert (Named p) where
    invert (NamedP n d p)  = NamedP (invertName n) (map invertName d) (invert p)


instance (Commute p, NameHack p) => Commute (Named p) where
    commute (NamedP n1 d1 p1 :> NamedP n2 d2 p2) =
        if n2 `elem` d1 || n1 `elem` d2
        then Nothing
        else do (p2' :> p1') <- commute (p1 :> p2)
                let (informAdd, informDel) = fromMaybe (const id, const id) (nameHack MyersDiff)
                return (NamedP n2 d2 (informAdd n1 p2') :> NamedP n1 d1 (informDel n2 p1'))

commuterIdNamed :: CommuteFn p1 p2 -> CommuteFn p1 (Named p2)
commuterIdNamed commuter (p1 :> NamedP n2 d2 p2) =
   do p2' :> p1' <- commuterIdFL commuter (p1 :> p2)
      return (NamedP n2 d2 p2' :> p1')

commuterNamedId :: CommuteFn p1 p2 -> CommuteFn (Named p1) p2
commuterNamedId commuter (NamedP n1 d1 p1 :> p2) =
   do p2' :> p1' <- commuterFLId commuter (p1 :> p2)
      return (p2' :> NamedP n1 d1 p1')

instance (Merge p, NameHack p) => Merge (Named p) where
    merge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
        = case merge (p1 :\/: p2) of
          (p2' :/\: p1') -> NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

mergerIdNamed :: MergeFn p1 p2 -> MergeFn p1 (Named p2)
mergerIdNamed merger (p1 :\/: NamedP n2 d2 p2) =
   case mergerIdFL merger (p1 :\/: p2) of
     p2' :/\: p1' -> NamedP n2 d2 p2' :/\: p1'

instance PatchInspect p => PatchInspect (Named p) where
    listTouchedFiles (NamedP _ _ p) = listTouchedFiles p
    hunkMatches f (NamedP _ _ p) = hunkMatches f p

instance (CommuteNoConflicts p, Conflict p) => Conflict (Named p) where
    listConflictedFiles (NamedP _ _ p) = listConflictedFiles p
    resolveConflicts (NamedP _ _ p) = resolveConflicts p

instance Check p => Check (Named p) where
    isInconsistent (NamedP _ _ p) = isInconsistent p

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Named p) where
    showPatch (NamedP n [] p) = showPatchInfo n <> showPatch p
    showPatch (NamedP n d p) = showNamedPrefix n d <+> showPatch p

instance (Apply p, CommuteNoConflicts p, Conflict p, IsHunk p, PatchListFormat p,
          PrimPatchBase p, ShowPatch p) => ShowPatch (Named p) where
    showContextPatch (NamedP n [] p) = showContextPatch p >>= return . (showPatchInfo n <>)
    showContextPatch (NamedP n d p) = showContextPatch p >>= return . (showNamedPrefix n d <+>)
    description (NamedP n _ _) = showPatchInfoUI n
    summary p = description p $$ text "" $$
                prefix "    " (plainSummary p) -- this isn't summary because summary does the
                                            -- wrong thing with (Named (FL p)) so that it can
                                            -- get the summary of a sequence of named patches
                                            -- right.
    summaryFL = vcat . mapFL summary
    showNicely p@(NamedP _ _ pt) = description p $$
                                   prefix "    " (showNicely pt)

instance Show2 p => Show1 (Named p wX) where
    showDict1 = ShowDictClass

instance Show2 p => Show2 (Named p) where
    showDict2 = ShowDictClass

instance PatchDebug p => PatchDebug (Named p)

