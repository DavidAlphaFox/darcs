{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim
       ( showPrim, showPrimFL,
         primIsAddfile, primIsHunk, primIsBinary, primIsSetpref,
         primIsAdddir, is_filepatch,
         canonize, tryToShrink,
         sortCoalesceFL, coalesce, canonizeFL,
         tryShrinkingInverse,
         summarizePrim,
         applyPrimFL,
         readPrim,
         FromPrim(..), FromPrims(..), ToFromPrim(..),
         PrimPatch, PrimPatchBase(..), PrimConstruct(..)
       )
       where

import Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimShow(..), showPrimFL, PrimRead(..)
    , PrimApply(..)
    , FromPrim(..), FromPrims(..), ToFromPrim(..)
    , PrimPatchBase(..), PrimPatch
    )

