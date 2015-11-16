module Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimShow(..), showPrimFL, PrimRead(..)
    , PrimApply(..)
    , PrimPatch, PrimPatchBase(..)
    , FromPrim(..), FromPrims(..), ToFromPrim(..)
    )
    where

import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.FileHunk ( FileHunk, IsHunk )
import Darcs.Util.Path ( FileName )
import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.ReadMonads ( ParserM )
import Darcs.Patch.Repair ( RepairToFL )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.SummaryData ( SummDetail )
import Darcs.Patch.Witnesses.Eq ( MyEq(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL, (:>), mapFL, mapFL_FL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed )

import Darcs.Util.Printer ( Doc, vcat )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )

import qualified Data.ByteString as B ( ByteString )


class (Patchy prim, MyEq prim
      ,PatchListFormat prim, IsHunk prim, RepairToFL prim
      ,PatchInspect prim, ReadPatch prim, ShowPatch prim
      ,PrimConstruct prim, PrimCanonize prim
      ,PrimClassify prim, PrimDetails prim
      ,PrimShow prim, PrimRead prim, PrimApply prim
      )
    => PrimPatch prim

class PrimPatch (PrimOf p) => PrimPatchBase p where
    type PrimOf (p :: (* -> * -> *)) :: (* -> * -> *)

instance PrimPatchBase p => PrimPatchBase (FL p) where
    type PrimOf (FL p) = PrimOf p

instance PrimPatchBase p => PrimPatchBase (RL p) where
    type PrimOf (RL p) = PrimOf p

class FromPrim p where
   fromPrim :: PrimOf p wX wY -> p wX wY

class FromPrim p => ToFromPrim p where
    toPrim :: p wX wY -> Maybe (PrimOf p wX wY)

class FromPrims p where
    fromPrims :: FL (PrimOf p) wX wY -> p wX wY

instance FromPrim p => FromPrim (FL p) where
    fromPrim p = fromPrim p :>: NilFL

instance FromPrim p => FromPrims (FL p) where
    fromPrims = mapFL_FL fromPrim

instance FromPrim p => FromPrims (RL p) where
    fromPrims = reverseFL . mapFL_FL fromPrim

class PrimClassify prim where
   primIsAddfile :: prim wX wY -> Bool
   primIsRmfile :: prim wX wY -> Bool
   primIsAdddir :: prim wX wY -> Bool
   primIsRmdir :: prim wX wY -> Bool
   primIsMove :: prim wX wY -> Bool
   primIsHunk :: prim wX wY -> Bool
   primIsTokReplace :: prim wX wY -> Bool
   primIsBinary :: prim wX wY -> Bool
   primIsSetpref :: prim wX wY -> Bool
   is_filepatch :: prim wX wY -> Maybe FileName

class PrimConstruct prim where
   addfile :: FilePath -> prim wX wY
   rmfile :: FilePath -> prim wX wY
   adddir :: FilePath -> prim wX wY
   rmdir :: FilePath -> prim wX wY
   move :: FilePath -> FilePath -> prim wX wY
   changepref :: String -> String -> String -> prim wX wY
   hunk :: FilePath -> Int -> [B.ByteString] -> [B.ByteString] -> prim wX wY
   tokreplace :: FilePath -> String -> String -> String -> prim wX wY
   binary :: FilePath -> B.ByteString -> B.ByteString -> prim wX wY
   primFromHunk :: FileHunk wX wY -> prim wX wY
   anIdentity :: prim wX wX

class PrimCanonize prim where
   -- | @tryToShrink ps@ simplifies @ps@ by getting rid of self-cancellations
   --   or coalescing patches
   --
   --   Question (Eric Kow): what properties should this have?  For example,
   --   the prim1 implementation only gets rid of the first self-cancellation
   --   it finds (as far as I can tell).  Is that OK? Can we try harder?
   tryToShrink :: FL prim wX wY -> FL prim wX wY

   -- | @tryShrinkingInverse ps@ deletes the first subsequence of
   --   primitive patches that is followed by the inverse subsequence,
   --   if one exists.  If not, it returns @Nothing@
   tryShrinkingInverse :: FL prim wX wY -> Maybe (FL prim wX wY)

   -- | 'sortCoalesceFL' @ps@ coalesces as many patches in @ps@ as
   --   possible, sorting the results in some standard order.
   sortCoalesceFL :: FL prim wX wY -> FL prim wX wY

   -- | It can sometimes be handy to have a canonical representation of a given
   -- patch.  We achieve this by defining a canonical form for each patch type,
   -- and a function 'canonize' which takes a patch and puts it into
   -- canonical form.  This routine is used by the diff function to create an
   -- optimal patch (based on an LCS algorithm) from a simple hunk describing the
   -- old and new version of a file.
   canonize :: D.DiffAlgorithm -> prim wX wY -> FL prim wX wY

   -- | 'canonizeFL' @ps@ puts a sequence of primitive patches into
   -- canonical form. Even if the patches are just hunk patches,
   -- this is not necessarily the same set of results as you would get
   -- if you applied the sequence to a specific tree and recalculated
   -- a diff.
   --
   -- Note that this process does not preserve the commutation behaviour
   -- of the patches and is therefore not appropriate for use when
   -- working with already recorded patches (unless doing amend-record
   -- or the like).
   canonizeFL :: D.DiffAlgorithm -> FL prim wX wY -> FL prim wX wY

   coalesce :: (prim :> prim) wX wY -> Maybe (FL prim wX wY)


class PrimDetails prim where
   summarizePrim :: prim wX wY -> [SummDetail]

class PrimShow prim where
   showPrim :: FileNameFormat -> prim wA wB -> Doc

showPrimFL :: PrimShow prim => FileNameFormat -> FL prim wA wB -> Doc
showPrimFL f xs = vcat (mapFL (showPrim f) xs)

class PrimRead prim where
   readPrim :: ParserM m => FileNameFormat -> m (Sealed (prim wX))

class PrimApply prim where
   applyPrimFL :: ApplyMonad m (ApplyState prim) => FL prim wX wY -> m ()
