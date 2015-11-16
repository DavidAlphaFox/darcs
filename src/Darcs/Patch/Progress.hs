{-# LANGUAGE CPP, GADTs #-}

module Darcs.Patch.Progress
    ( progressRL
    , progressFL
    , progressRLShowTags
    ) where

import System.IO.Unsafe ( unsafePerformIO )

import Darcs.Patch.Info ( justName, isTag )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..), lengthRL, lengthFL )

import Darcs.Util.Progress ( minlist, beginTedious, endTedious, progress,
                  progressKeepLatest, tediousSize, finishedOne )

startProgress :: a -> String -> Int -> a
startProgress x k len = unsafePerformIO $ do beginTedious k
                                             tediousSize k len
                                             return x

-- | Evaluate an 'FL' list and report progress.
progressFL :: String -> FL a wX wY -> FL a wX wY
progressFL _ NilFL = NilFL
progressFL k xxs@(x :>: xs) = if xxsLen < minlist
                                  then xxs
                                  else startProgress x k xxsLen :>: pl xs
  where
    xxsLen = lengthFL xxs

    pl :: FL a wX wY -> FL a wX wY
    pl NilFL = NilFL
    pl (y :>: NilFL) = unsafePerformIO $ do endTedious k
                                            return (y :>: NilFL)
    pl (y :>: ys) = progress k y :>: pl ys

-- | Evaluate an 'RL' list and report progress.
progressRL :: String -> RL a wX wY -> RL a wX wY
progressRL _ NilRL = NilRL
progressRL k xxs@(x :<: xs) =
    if xxsLen < minlist
        then xxs
        else startProgress x k xxsLen :<: pl xs
  where
    xxsLen = lengthRL xxs
    pl :: RL a wX wY -> RL a wX wY
    pl NilRL = NilRL
    pl (y:<:NilRL) = unsafePerformIO $ do endTedious k
                                          return (y:<:NilRL)
    pl (y:<:ys) = progress k y :<: pl ys

-- | Evaluate an 'RL' list and report progress. In addition to printing
-- the number of patches we got, show the name of the last tag we got.
progressRLShowTags :: String -> RL (PatchInfoAnd p) wX wY
                   -> RL (PatchInfoAnd p) wX wY
progressRLShowTags _ NilRL = NilRL
progressRLShowTags k xxs@(x :<: xs) =
    if xxsLen < minlist
        then xxs
        else startProgress x k xxsLen :<: pl xs
  where
    xxsLen = lengthRL xxs

    pl :: RL (PatchInfoAnd p) wX wY -> RL (PatchInfoAnd p) wX wY
    pl NilRL = NilRL
    pl (y :<: NilRL) = unsafePerformIO $ do endTedious k
                                            return (y :<: NilRL)
    pl (y :<: ys) =
        if isTag iy
            then finishedOne k ("back to "++ justName iy) y :<: pl ys
            else progressKeepLatest k y :<: pl ys
      where
        iy = info y
