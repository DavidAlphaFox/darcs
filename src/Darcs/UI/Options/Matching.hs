{-# LANGUAGE RecordWildCards #-}
{-| Patch matching options.

These are all of the same type 'MatchOption' defined below.

Multiple flags per option are allowed and do not raise a conflict error.
This is how Darcs currently operates, even though I suspect that it ignores
all but the first 'MatchFlag' (since it does so for many other options).

Given a suitable semantics (and documentation thereof), for instance \"all
the given patterns must match\", this could be turned into a useful feature.

-}
module Darcs.UI.Options.Matching
    ( MatchFlag(..) -- re-export
    , matchOne
    , matchOneContext
    , matchOneNontag
    , matchSeveral
    , matchSeveralOrFirst
    , matchSeveralOrLast
    , matchRange
    , matchSeveralOrRange
    , matchAny -- temporary, for toMatchFlags
    , context -- temporary, for getContext
    ) where

import Prelude hiding ( last )
import Data.Char ( isDigit )
import Data.Monoid ( (<>), mconcat )

import Darcs.Patch.Match ( MatchFlag(..) )
import qualified Darcs.UI.Options.Flags as F ( DarcsFlag(..) )
import Darcs.UI.Options.Core
import Darcs.UI.Options.Util

-- * Type instantiations

type MatchOption = PrimDarcsOption [MatchFlag]

-- * Combined matching options

matchOne :: MatchOption -- ^ amend, show files/contents, dist, annotate
matchOne = mconcat [match, patch, hash, tag, index]

-- | Used by: clone
matchOneContext :: MatchOption
matchOneContext = mconcat [toMatch, toPatch, toHash, tag, context]

-- [NOTE --index removed from matchOneNontag because issue1926]
-- The --index option was removed for 2.5 release because it isn't handled
-- by amend-record (see issue1926).
--
-- At this moment, amend-record is the only command that uses 'matchOneNontag',
-- so there is no other command affected.

-- | Used by: amend
matchOneNontag :: MatchOption
matchOneNontag =  match <> patch <> hash

-- | Used by: rebase pull, apply, send, push, pull, fetch
matchSeveral :: MatchOption
matchSeveral = matches <> patches <> tags <> hash

-- | Used by: rebase unsuspend/reify
matchSeveralOrFirst :: MatchOption
matchSeveralOrFirst = mconcat [ matchTo, last, matches, patches, tags, hash ]

-- | Used by: unrecord, obliterate, rebase suspend, rollback
matchSeveralOrLast :: MatchOption
matchSeveralOrLast = mconcat [ matchFrom, last, matches, patches, tags, hash ]

-- | Used by: diff
matchRange :: MatchOption
matchRange = mconcat [ matchTo, matchFrom, match, patch, hash, last, indexes ]

-- | Used by: log
matchSeveralOrRange :: MatchOption
matchSeveralOrRange = mconcat
  [ matchTo, matchFrom, last, indexes, matches, patches, tags, hash ]

matchTo :: MatchOption
matchTo = toMatch <> toPatch <> toHash <> toTag

matchFrom :: MatchOption
matchFrom = fromMatch <> fromPatch <> fromHash <> fromTag

matchAny :: MatchOption
matchAny = mconcat [ toMatch, toPatch, toHash, toTag,
  fromMatch, fromPatch, fromHash, fromTag,
  tag, tags, patch, patches, match, matches, index, indexes, context, last ]

-- * Primitive matching options

toMatch, toPatch, toHash, toTag,
  fromMatch, fromPatch, fromHash, fromTag,
  tag, tags,
  patch, patches,
  hash,
  match, matches,
  index, indexes,
  context, last :: MatchOption

toMatch = OptSpec {..} where
  ounparse k mfs = k [ F.UpToPattern s | UpToPattern s <- mfs ]
  oparse k fs = k [ UpToPattern s | F.UpToPattern s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["to-match"] F.UpToPattern "PATTERN"
    "select changes up to a patch matching PATTERN" ]

toPatch = OptSpec {..} where
  ounparse k mfs = k [ F.UpToPatch s | UpToPatch s <- mfs ]
  oparse k fs = k [ UpToPatch s | F.UpToPatch s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["to-patch"] F.UpToPatch "REGEXP"
    "select changes up to a patch matching REGEXP" ]

toHash = OptSpec {..} where
  ounparse k mfs = k [ F.UpToHash s | UpToHash s <- mfs ]
  oparse k fs = k [ UpToHash s | F.UpToHash s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["to-hash"] F.UpToHash "HASH"
    "select changes up to a patch with HASH" ]

context = OptSpec {..} where
  ounparse k mfs = k [ F.Context p | Context p <- mfs ]
  oparse k fs = k [ Context p | F.Context p <- fs ]
  ocheck _ = []
  odesc = [ absPathArg [] ["context"] F.Context "FILENAME"
    "version specified by the context in FILENAME" ]

toTag = OptSpec {..} where
  ounparse k mfs = k [ F.UpToTag s | UpToTag s <- mfs ]
  oparse k fs = k [ UpToTag s | F.UpToTag s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["to-tag"] F.UpToTag "REGEXP"
    "select changes up to a tag matching REGEXP" ]

fromMatch = OptSpec {..} where
  ounparse k mfs = k [ F.AfterPattern s | AfterPattern s <- mfs ]
  oparse k fs = k [ AfterPattern s | F.AfterPattern s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["from-match"] F.AfterPattern "PATTERN"
    "select changes starting with a patch matching PATTERN" ]

fromPatch = OptSpec {..} where
  ounparse k mfs = k [ F.AfterPatch s | AfterPatch s <- mfs ]
  oparse k fs = k [ AfterPatch s | F.AfterPatch s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["from-patch"] F.AfterPatch "REGEXP"
    "select changes starting with a patch matching REGEXP" ]

fromHash = OptSpec {..} where
  ounparse k mfs = k [ F.AfterHash s | AfterHash s <- mfs ]
  oparse k fs = k [ AfterHash s | F.AfterHash s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["from-hash"] F.AfterHash "HASH"
    "select changes starting with a patch with HASH" ]

fromTag = OptSpec {..} where
  ounparse k mfs = k [ F.AfterTag s | AfterTag s <- mfs ]
  oparse k fs = k [ AfterTag s | F.AfterTag s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["from-tag"] F.AfterTag "REGEXP"
    "select changes starting with a tag matching REGEXP" ]

tag = OptSpec {..} where
  ounparse k mfs = k [ F.OneTag s | OneTag s <- mfs ]
  oparse k fs = k [ OneTag s | F.OneTag s <- fs ]
  ocheck _ = []
  odesc = [ strArg ['t'] ["tag"] F.OneTag "REGEXP" "select tag matching REGEXP" ]

tags = OptSpec {..} where
  ounparse k mfs = k [ F.OneTag s | OneTag s <- mfs ]
  oparse k fs = k [ OneTag s | F.OneTag s <- fs ]
  ocheck _ = []
  odesc = [ strArg ['t'] ["tags"] F.OneTag "REGEXP" "select tags matching REGEXP" ]

patch = OptSpec {..} where
  ounparse k mfs = k [ F.OnePatch s | OnePatch s <- mfs ]
  oparse k fs = k [ OnePatch s | F.OnePatch s <- fs ]
  ocheck _ = []
  odesc = [ strArg ['p'] ["patch"] F.OnePatch "REGEXP"
    "select a single patch matching REGEXP" ]

patches = OptSpec {..} where
  ounparse k mfs = k [ F.SeveralPatch s | SeveralPatch s <- mfs ]
  oparse k fs = k [ SeveralPatch s | F.SeveralPatch s <- fs ]
  ocheck _ = []
  odesc = [ strArg ['p'] ["patches"] F.SeveralPatch "REGEXP"
    "select patches matching REGEXP" ]

hash = OptSpec {..} where
  ounparse k mfs = k [ F.OneHash s | OneHash s <- mfs ]
  oparse k fs = k [ OneHash s | F.OneHash s <- fs ]
  ocheck _ = []
  odesc = [ strArg ['h'] ["hash"] F.OneHash "HASH"
    "select a single patch with HASH" ]

match = OptSpec {..} where
  ounparse k mfs = k [ F.OnePattern s | OnePattern s <- mfs ]
  oparse k fs = k [ OnePattern s | F.OnePattern s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["match"] F.OnePattern "PATTERN"
    "select a single patch matching PATTERN" ]

matches = OptSpec {..} where
  ounparse k mfs = k [ F.SeveralPattern s | SeveralPattern s <- mfs ]
  oparse k fs = k [ SeveralPattern s | F.SeveralPattern s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["matches"] F.SeveralPattern "PATTERN"
    "select patches matching PATTERN" ]

last = OptSpec {..} where
  ounparse k mfs = k [ F.LastN s | LastN s <- mfs ]
  oparse k fs = k [ LastN s | F.LastN s <- fs ]
  ocheck _ = []
  odesc = [ strArg [] ["last"] (F.LastN . toInt) "NUMBER"
    "select the last NUMBER patches" ]
  toInt s = if not (null s) && all isDigit s then read s else (-1)

-- | TODO: see 'Darcs.UI.Options.matchMaxcount'.
index = OptSpec {..} where
  ounparse k mfs = k [ F.PatchIndexRange n m | PatchIndexRange n m <- mfs ]
  oparse k fs = k [ PatchIndexRange n m | F.PatchIndexRange n m <- fs ]
  ocheck _ = []
  odesc = [ strArg ['n'] ["index"] indexrange "N" "select one patch" ]
  indexrange s = if all isDigit s
                 then F.PatchIndexRange (read s) (read s)
                 else F.PatchIndexRange 0 0

-- | TODO: see 'Darcs.UI.Options.matchMaxcount'.
indexes = OptSpec {..} where
  ounparse k mfs = k [ F.PatchIndexRange n m | PatchIndexRange n m <- mfs ]
  oparse k fs = k [ PatchIndexRange n m | F.PatchIndexRange n m <- fs ]
  ocheck _ = []
  odesc = [ strArg ['n'] ["index"] indexrange "N-M" "select a range of patches" ]
  indexrange s = if all isokay s
                 then if '-' `elem` s
                      then let x1 = takeWhile (/= '-') s
                               x2 = reverse $ takeWhile (/= '-') $ reverse s
                           in F.PatchIndexRange (read x1) (read x2)
                      else F.PatchIndexRange (read s) (read s)
                 else F.PatchIndexRange 0 0
  isokay c = isDigit c || c == '-'
