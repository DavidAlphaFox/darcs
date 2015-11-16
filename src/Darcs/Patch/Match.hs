--  Copyright (C) 2004-2005 David Roundy
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

{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

-- | /First matcher, Second matcher and Nonrange matcher/
--
-- When we match for patches, we have a PatchSet, of which we want a
-- subset. This subset is formed by the patches in a given interval
-- which match a given criterion. If we represent time going left to
-- right, then we have (up to) three 'Matcher's:
--
-- * the 'firstMatcher' is the left bound of the interval,
--
-- * the 'secondMatcher' is the right bound, and
--
-- * the 'nonrangeMatcher' is the criterion we use to select among
--   patches in the interval.
---
-- Each of these matchers can be present or not according to the
-- options. The patches we want would then be the ones that all
-- present matchers have in common.
--
-- (Implementation note: keep in mind that the PatchSet is written
-- backwards with respect to the timeline, ie., from right to left)
module Darcs.Patch.Match
    (
      matchParser
    , helpOnMatchers
    , addInternalMatcher
    , matchFirstPatchset
    , matchSecondPatchset
    , splitSecondFL
    , matchPatch
    , matchAPatch
    , matchAPatchread
    , getNonrangeMatchS
    , firstMatch
    , secondMatch
    , haveNonrangeMatch
    , havePatchsetMatch
    , checkMatchSyntax
    , applyInvToMatcher
    , nonrangeMatcher
    , InclusiveOrExclusive(..)
    , matchExists
    , applyNInv
    , hasIndexRange
    , getMatchingTag
    , matchAPatchset
    , getFirstMatchS
    , nonrangeMatcherIsTag
    , MatchFlag(..)
    ) where

import Text.ParserCombinators.Parsec
    ( parse
    , CharParser
    , (<?>)
    , (<|>)
    , noneOf
    , option
    , eof
    , many
    , try
    , between
    , spaces
    , char
    , oneOf
    , string
    , choice
    )
import Text.ParserCombinators.Parsec.Expr
    ( OperatorTable
    , Assoc( AssocLeft )
    , Operator ( Infix, Prefix )
    , buildExpressionParser
    )
import Text.Regex ( mkRegex, matchRegex )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad ( when )
import Data.List ( isPrefixOf, intercalate )
import Data.Char ( toLower )

import Darcs.Util.Path ( AbsolutePath )
import Darcs.Patch
    ( Patchy
    , hunkMatches
    , listTouchedFiles
    , patchcontents
    , Named
    , invert
    , invertRL
    , patch2patchinfo
    , apply
    )
import Darcs.Patch.Info ( justName, justAuthor, justLog, makePatchname,
                          piDate )

import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Dummy ( DummyPatch )

import Darcs.Patch.Matchable ( Matchable )
import Darcs.Patch.MaybeInternal ( MaybeInternal, patchInternalChecker, isInternal, flIsInternal )
import Darcs.Patch.MonadProgress ( MonadProgress )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, conscientiously, hopefully )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, newset2RL, Origin )
import Darcs.Patch.Type ( PatchType(..) )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.ApplyPatches( applyPatches )
import Darcs.Patch.Depends ( getPatchesBeyondTag, splitOnTag )

import Darcs.Patch.Witnesses.Eq ( isIsEq )
import Darcs.Patch.Witnesses.Ordered ( RL(..), consRLSealed, FL(..), (:>)(..) )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..), Sealed2(..),
    seal, flipSeal, seal2, unsealFlipped, unseal2, unseal )
import Darcs.Util.Printer ( text, ($$) )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )

import Darcs.Util.DateMatcher ( parseDateMatcher )

import Storage.Hashed.Tree ( Tree )
#include "impossible.h"

-- | A type for predicates over patches which do not care about
-- contexts
type MatchFun p = Sealed2 (PatchInfoAnd p) -> Bool

-- | A @Matcher@ is made of a 'MatchFun' which we will use to match
-- patches and a @String@ representing it.
data Matcher p = MATCH String (MatchFun p)

instance Show (Matcher p) where
    show (MATCH s _) = '"':s ++ "\""


data MatchFlag =
                 OnePattern      String
               | SeveralPattern  String
               | AfterPattern    String
               | UpToPattern     String
               | OnePatch        String
               | OneHash         String
               | AfterHash       String
               | UpToHash        String
               | SeveralPatch    String
               | AfterPatch      String
               | UpToPatch       String
               | OneTag          String
               | AfterTag        String
               | UpToTag         String
               | LastN           Int
               | PatchIndexRange Int Int
               | Context AbsolutePath
                 deriving ( Show )


makeMatcher :: String -> (Sealed2 (PatchInfoAnd p) -> Bool) -> Matcher p
makeMatcher = MATCH

-- | @applyMatcher@ applies a matcher to a patch.
applyMatcher :: Matcher p -> PatchInfoAnd p wX wY -> Bool
applyMatcher (MATCH _ m) = m . seal2

parseMatch :: Matchable p => String -> Either String (MatchFun p)
parseMatch pattern =
    case parse matchParser "match" pattern of
    Left err -> Left $ "Invalid --match pattern '"++ pattern ++
                "'.\n"++ unlines (map ("    "++) $ lines $ show err) -- indent
    Right m -> Right m

matchPattern :: Matchable p => String -> Matcher p
matchPattern pattern =
    case parseMatch pattern of
    Left err -> error err
    Right m -> makeMatcher pattern m

addInternalMatcher :: Matchable p => Maybe (Matcher p) -> Maybe (Matcher p)
addInternalMatcher om =
  case patchInternalChecker of
    Nothing -> om
    Just f ->
         let matchFun = unseal2 (not . isIsEq . isInternal f . patchcontents . hopefully)
         in case om of
            Nothing -> Just (MATCH "internal patch" matchFun)
            Just (MATCH s oldFun) -> Just (MATCH s (\p -> matchFun p && oldFun p))

matchParser :: Matchable p => CharParser st (MatchFun p)
matchParser = submatcher <?> helpfulErrorMsg
  where
    submatcher = do
        m <- option matchAnyPatch submatch
        eof
        return m

    -- When using <?>, Parsec prepends "expecting " to the given error message,
    -- so the phrasing below makes sense.
    helpfulErrorMsg = "valid expressions over: "
                      ++ intercalate ", " (map (\(name, _, _, _) -> name) ps)
                      ++ "\nfor more help, see `darcs help patterns`."

    -- This type signature is just to bind an ambiguous type var.
    ps :: [(String, String, [String], String -> MatchFun DummyPatch)]
    ps = primitiveMatchers

    -- matchAnyPatch is returned if submatch fails without consuming any
    -- input, i.e. if we pass --match '', we want to match anything.
    matchAnyPatch :: Matchable p => MatchFun p
    matchAnyPatch = const True

submatch :: Matchable p => CharParser st (MatchFun p)
submatch = buildExpressionParser table match

table :: OperatorTable Char st (MatchFun p)
table   = [ [prefix "not" negate_match,
             prefix "!" negate_match ]
          , [binary "||" or_match,
             binary "or" or_match,
             binary "&&" and_match,
            binary "and" and_match ]
          ]
    where binary name fun = Infix (tryNameAndUseFun name fun) AssocLeft
          prefix name fun = Prefix $ tryNameAndUseFun name fun
          tryNameAndUseFun name fun = do _ <- trystring name
                                         spaces
                                         return fun
          negate_match a p = not (a p)
          or_match m1 m2 p = m1 p || m2 p
          and_match m1 m2 p = m1 p && m2 p

trystring :: String -> CharParser st String
trystring s = try $ string s

match :: Matchable p => CharParser st (MatchFun p)
match = between spaces spaces (parens submatch <|> choice matchers_)
  where
    matchers_ = map createMatchHelper primitiveMatchers

createMatchHelper :: (String, String, [String], String -> MatchFun p)
                  -> CharParser st (MatchFun p)
createMatchHelper (key,_,_,matcher) =
  do _ <- trystring key
     spaces
     q <- quoted
     return $ matcher q

-- FIXME: would this be better defined in Darcs.Commands.Help?
-- | The string that is emitted when the user runs @darcs help --match@.
helpOnMatchers :: [String]
helpOnMatchers =
  ["Selecting Patches:",
   "",
   "The --patches option yields patches with names matching an *extended*",
   "regular expression.  See regex(7) for details.  The --matches option",
   "yields patches that match a logical (Boolean) expression: one or more",
   "primitive expressions combined by grouping (parentheses) and the",
   "complement (not), conjunction (and) and disjunction (or) operators.",
   "The C notation for logic operators (!, && and ||) can also be used.",
   "",
   "- --patches=regex is a synonym for --matches='name regex'",
   "- --hash=HASH is a synonym for --matches='hash HASH'",
   "- --from-patch and --to-patch are synonyms for --from-match='name... and --to-match='name...",
   "- --from-patch and --to-match can be unproblematically combined:",
   "  `darcs changes --from-patch='html.*documentation' --to-match='date 20040212'`",
   "",
   "The following primitive Boolean expressions are supported:"
   ,""]
  ++ keywords
  ++ ["", "Here are some examples:", ""]
  ++ examples
  where -- This type signature exists to appease GHC.
        ps :: [(String, String, [String], String -> MatchFun DummyPatch)]
        ps = primitiveMatchers
        keywords = [showKeyword k d | (k,d,_,_) <- ps]
        examples = [showExample k e | (k,_,es,_) <- ps, e <- es]
        showKeyword keyword description =
            -- FIXME: it would be nice to have a variable name here:
            -- "author REGEX - match against author (email address)"
            -- or "exact STRING - match against exact patch name".
            "- " ++ keyword ++ " - " ++ description ++ "."
        showExample keyword example =
            -- FIXME: this string is long, and its not a use case I've
            -- ever seen in practice.  Can we use something else,
            -- like "darcs changes --matches"? --twb, 2008-12-28
            "    darcs annotate --summary --match "
            ++ "'" ++ keyword ++ " " ++ example ++ "'"

primitiveMatchers :: Matchable p => [(String, String, [String], String -> MatchFun p)]
                     -- ^ keyword (operator), help description, list
                     -- of examples, matcher function
primitiveMatchers =
 [ ("exact", "check a literal string against the patch name"
           , ["\"Resolve issue17: use dynamic memory allocation.\""]
           , exactmatch )
 , ("name", "check a regular expression against the patch name"
          , ["issue17", "\"^[Rr]esolve issue17\\>\""]
          , namematch )
 , ("author", "check a regular expression against the author name"
            , ["\"David Roundy\"", "droundy", "droundy@darcs.net"]
            , authormatch )
 , ("hunk", "check a regular expression against the contents of a hunk patch"
            , ["\"foo = 2\"", "\"^instance .* Foo where$\""]
            , hunkmatch )
 , ("comment", "check a regular expression against the log message"
         , ["\"prevent deadlocks\""]
         , logmatch )
 , ("hash",  "match a full hash or a prefix for a patch"
          ,  ["c719567e92c3b0ab9eddd5290b705712b8b918ef","c7195"]
          ,  hashmatch )
 , ("date", "match the patch date"
          , ["\"2006-04-02 22:41\"", "\"tea time yesterday\""]
          , datematch )
 , ("touch", "match file paths for a patch"
          , ["src/foo.c", "src/", "\"src/*.(c|h)\""]
          , touchmatch ) ]

parens :: CharParser st (MatchFun p)
       -> CharParser st (MatchFun p)
parens = between (string "(") (string ")")

quoted :: CharParser st String
quoted = between (char '"') (char '"')
                 (many $ do { _ <- char '\\' -- allow escapes
                            ; try (oneOf "\\\"") <|> return '\\'
                            }
                         <|>  noneOf "\"")
         <|> between spaces spaces (many $ noneOf " ()")
         <?> "string"

namematch, exactmatch, authormatch, hunkmatch, hashmatch, datematch, touchmatch
  :: Matchable p => String -> MatchFun p

namematch r (Sealed2 hp) = isJust $ matchRegex (mkRegex r) $ justName (info hp)

exactmatch r (Sealed2 hp) = r == justName (info hp)

authormatch a (Sealed2 hp) = isJust $ matchRegex (mkRegex a) $ justAuthor (info hp)

logmatch :: Matchable p => String -> MatchFun p
logmatch l (Sealed2 hp) = isJust $ matchRegex (mkRegex l) $ justLog (info hp)

hunkmatch r (Sealed2 hp) = let patch = patchcontents $ hopefully hp
                               regexMatcher = isJust . matchRegex (mkRegex r) . BC.unpack
                           in hunkMatches regexMatcher patch

hashmatch h (Sealed2 hp) = let rh = show $ makePatchname (info hp)
                               lh = map toLower h
                           in (lh `isPrefixOf` rh) || (lh == rh ++ ".gz")

datematch d (Sealed2 hp) = let dm = unsafePerformIO $ parseDateMatcher d
                                  in dm $ piDate (info hp)

touchmatch r (Sealed2 hp) = let files = listTouchedFiles $ patchcontents $ hopefully hp
                            in any (isJust . matchRegex (mkRegex r)) files

data InclusiveOrExclusive = Inclusive | Exclusive deriving Eq

-- | @haveNonrangeMatch flags@ tells whether there is a flag in
-- @flags@ which corresponds to a match that is "non-range". Thus,
-- @--match@, @--patch@, @--hash@ and @--index@ make @haveNonrangeMatch@
-- true, but not @--from-patch@ or @--to-patch@.
haveNonrangeMatch :: forall p . Matchable p => PatchType p -> [MatchFlag] -> Bool
haveNonrangeMatch _ fs =
     case hasIndexRange fs of Just (m,n) | m == n -> True; _ -> False
  || isJust (nonrangeMatcher fs::Maybe (Matcher p))

-- | @havePatchsetMatch flags@ tells whether there is a "patchset
-- match" in the flag list. A patchset match is @--match@ or
-- @--patch@, or @--context@, but not @--from-patch@ nor (!)
-- @--index@.
-- Question: Is it supposed not to be a subset of @haveNonrangeMatch@?
havePatchsetMatch :: [MatchFlag] -> Bool
havePatchsetMatch fs = isJust (nonrangeMatcher fs::Maybe (Matcher DummyPatch)) || hasC fs
    where hasC [] = False
          hasC (Context _:_) = True
          hasC (_:xs) = hasC xs

getNonrangeMatchS :: (ApplyMonad m (ApplyState p), MonadProgress m, Matchable p, ApplyState p ~ Tree)
                  => [MatchFlag]
                  -> PatchSet p Origin wX
                  -> m ()
getNonrangeMatchS fs repo =
    case nonrangeMatcher fs of
        Just m -> if nonrangeMatcherIsTag fs
                        then getTagS m repo
                        else getMatcherS Exclusive m repo
        Nothing -> fail "Pattern not specified in getNonrangeMatch."

-- | @firstMatch fs@ tells whether @fs@ implies a "first match", that
-- is if we match against patches from a point in the past on, rather
-- than against all patches since the creation of the repository.
firstMatch :: [MatchFlag] -> Bool
firstMatch fs = isJust (hasLastn fs)
                 || isJust (firstMatcher fs::Maybe (Matcher DummyPatch))
                 || isJust (hasIndexRange fs)

getFirstMatchS :: (ApplyMonad m (ApplyState p), MonadProgress m, Matchable p)
               => [MatchFlag] -> PatchSet p Origin wX -> m ()
getFirstMatchS fs repo =
    case hasLastn fs of
    Just n -> unpullLastN repo n
    Nothing ->
     case hasIndexRange fs of
     Just (_,b) -> unpullLastN repo b -- b is chronologically earlier than a
     Nothing    ->
      case firstMatcher fs of
               Nothing -> fail "Pattern not specified in getFirstMatchS."
               Just m -> if firstMatcherIsTag fs
                         then getTagS m repo
                         else getMatcherS Inclusive m repo

-- | @secondMatch fs@ tells whether @fs@ implies a "second match", that
-- is if we match against patches up to a point in the past on, rather
-- than against all patches until now.
secondMatch :: [MatchFlag] -> Bool
secondMatch fs = isJust (secondMatcher fs::Maybe (Matcher DummyPatch)) || isJust (hasIndexRange fs)

unpullLastN :: (ApplyMonad m (ApplyState p), MonadProgress m, Patchy p, MaybeInternal p)
            => PatchSet p wX wY
            -> Int
            -> m ()
unpullLastN repo n = applyInvRL `unsealFlipped` safetake n (newset2RL repo)

checkMatchSyntax :: [MatchFlag] -> IO ()
checkMatchSyntax opts =
 case getMatchPattern opts of
  Nothing -> return ()
  Just p  -> either fail (const $ return ()) (parseMatch p::Either String (MatchFun DummyPatch))

getMatchPattern :: [MatchFlag] -> Maybe String
getMatchPattern [] = Nothing
getMatchPattern (OnePattern m:_) = Just m
getMatchPattern (SeveralPattern m:_) = Just m
getMatchPattern (_:fs) = getMatchPattern fs

tagmatch :: Matchable p => String -> Matcher p
tagmatch r = makeMatcher ("tag-name "++r) tm
    where tm (Sealed2 p) =
              let n = justName (info p) in
              "TAG " `isPrefixOf` n && isJust (matchRegex (mkRegex r) $ drop 4 n)

patchmatch :: Matchable p => String -> Matcher p
patchmatch r = makeMatcher ("patch-name "++r) (namematch r)

hashmatch' :: Matchable p => String -> Matcher p
hashmatch' r = makeMatcher ("hash "++r) (hashmatch r)


-- | strictJust is a strict version of the Just constructor, used to ensure
-- that if we claim we've got a pattern match, that the pattern will
-- actually match (rathern than fail to compile properly).
strictJust :: a -> Maybe a
strictJust x = Just $! x

-- | @nonrangeMatcher@ is the criterion that is used to match against
-- patches in the interval. It is 'Just m' when the @--patch@, @--match@,
-- @--tag@ options are passed (or their plural variants).
nonrangeMatcher, nonrangeMatcherArgs :: Matchable p => [MatchFlag] -> Maybe (Matcher p)

nonrangeMatcher fs = addInternalMatcher $ nonrangeMatcherArgs fs

nonrangeMatcherArgs [] = Nothing
nonrangeMatcherArgs (OnePattern m:_) = strictJust $ matchPattern m
nonrangeMatcherArgs (OneTag t:_) = strictJust $ tagmatch t
nonrangeMatcherArgs (OnePatch p:_) = strictJust $ patchmatch p
nonrangeMatcherArgs (OneHash h:_) = strictJust $ hashmatch' h
nonrangeMatcherArgs (SeveralPattern m:_) = strictJust $ matchPattern m
nonrangeMatcherArgs (SeveralPatch p:_) = strictJust $ patchmatch p
nonrangeMatcherArgs (_:fs) = nonrangeMatcherArgs fs

-- | @nonrangeMatcherIsTag@ returns true if the matching option was
-- '--tag'
nonrangeMatcherIsTag :: [MatchFlag] -> Bool
nonrangeMatcherIsTag [] = False
nonrangeMatcherIsTag (OneTag _:_) = True
nonrangeMatcherIsTag (_:fs) = nonrangeMatcherIsTag fs

-- | @firstMatcher@ returns the left bound of the matched interval.
-- This left bound is also specified when we use the singular versions
-- of @--patch@, @--match@ and @--tag@. Otherwise, @firstMatcher@
-- returns @Nothing@.
firstMatcher :: Matchable p => [MatchFlag] -> Maybe (Matcher p)
firstMatcher [] = Nothing
firstMatcher (OnePattern m:_) = strictJust $ matchPattern m
firstMatcher (AfterPattern m:_) = strictJust $ matchPattern m
firstMatcher (AfterTag t:_) = strictJust $ tagmatch t
firstMatcher (OnePatch p:_) = strictJust $ patchmatch p
firstMatcher (AfterPatch p:_) = strictJust $ patchmatch p
firstMatcher (OneHash h:_) = strictJust $ hashmatch' h
firstMatcher (AfterHash h:_) = strictJust $ hashmatch' h
firstMatcher (_:fs) = firstMatcher fs

firstMatcherIsTag :: [MatchFlag] -> Bool
firstMatcherIsTag [] = False
firstMatcherIsTag (AfterTag _:_) = True
firstMatcherIsTag (_:fs) = firstMatcherIsTag fs

secondMatcher :: Matchable p => [MatchFlag] -> Maybe (Matcher p)
secondMatcher [] = Nothing
secondMatcher (OnePattern m:_) = strictJust $ matchPattern m
secondMatcher (UpToPattern m:_) = strictJust $ matchPattern m
secondMatcher (OnePatch p:_) = strictJust $ patchmatch p
secondMatcher (UpToPatch p:_) = strictJust $ patchmatch p
secondMatcher (OneHash h:_) = strictJust $ hashmatch' h
secondMatcher (UpToHash h:_) = strictJust $ hashmatch' h
secondMatcher (UpToTag t:_) = strictJust $ tagmatch t
secondMatcher (_:fs) = secondMatcher fs

secondMatcherIsTag :: [MatchFlag] -> Bool
secondMatcherIsTag [] = False
secondMatcherIsTag (UpToTag _:_) = True
secondMatcherIsTag (_:fs) = secondMatcherIsTag fs

-- | @matchAPatchread fs p@ tells whether @p@ matches the matchers in
-- the flags listed in @fs@.
matchAPatchread :: Matchable p => [MatchFlag] -> PatchInfoAnd p wX wY -> Bool
matchAPatchread fs = case nonrangeMatcher fs of
                       Nothing -> const True
                       Just m -> applyMatcher m

-- | @matchAPatch fs p@ tells whether @p@ matches the matchers in
-- the flags @fs@
matchAPatch :: Matchable p => [MatchFlag] -> PatchInfoAnd p wX wY -> Bool
matchAPatch fs p =
    case nonrangeMatcher fs of
    Nothing -> True
    Just m -> applyMatcher m p

matchPatch :: Matchable p => [MatchFlag] -> PatchSet p wStart wX -> Sealed2 (Named p)
matchPatch fs ps =
    case hasIndexRange fs of
    Just (a,a') | a == a' -> case unseal myhead $ dropn (a-1) ps of
                             Just (Sealed2 p) -> seal2 $ hopefully p
                             Nothing -> error "Patch out of range!"
                | otherwise -> bug ("Invalid index range match given to matchPatch: "++
                                    show (PatchIndexRange a a'))
                where myhead :: PatchSet p wStart wX -> Maybe (Sealed2 (PatchInfoAnd p))
                      myhead (PatchSet NilRL (Tagged t _ _ :<: _)) = Just $ seal2 t
                      myhead (PatchSet (x:<:_) _) = Just $ seal2 x
                      myhead _ = Nothing
    Nothing -> case nonrangeMatcher fs of
                    Nothing -> bug "Couldn't matchPatch"
                    Just m -> findAPatch m ps

-- | @hasLastn fs@ return the @--last@ argument in @fs@, if any.
hasLastn :: [MatchFlag] -> Maybe Int
hasLastn [] = Nothing
hasLastn (LastN (-1):_) = error "--last requires a positive integer argument."
hasLastn (LastN n:_) = Just n
hasLastn (_:fs) = hasLastn fs

hasIndexRange :: [MatchFlag] -> Maybe (Int,Int)
hasIndexRange [] = Nothing
hasIndexRange (PatchIndexRange x y:_) = Just (x,y)
hasIndexRange (_:fs) = hasIndexRange fs

-- | @matchFirstPatchset fs ps@ returns the part of @ps@ before its
-- first matcher, ie the one that comes first dependencywise. Hence,
-- patches in @matchFirstPatchset fs ps@ are the context for the ones
-- we don't want.
matchFirstPatchset :: Matchable p => [MatchFlag] -> PatchSet p wStart wX
                   -> SealedPatchSet p wStart
matchFirstPatchset fs patchset =
    case hasLastn fs of
    Just n -> dropn n patchset
    Nothing ->
        case hasIndexRange fs of
        Just (_,b) -> dropn b patchset
        Nothing ->
               case firstMatcher fs of
               Nothing -> bug "Couldn't matchFirstPatchset"
               Just m -> unseal (dropn 1) $ if firstMatcherIsTag fs
                                            then getMatchingTag m patchset
                                            else matchAPatchset m patchset

-- | @dropn n ps@ drops the @n@ last patches from @ps@.
dropn :: MaybeInternal p => Int -> PatchSet p wStart wX -> SealedPatchSet p wStart
dropn n ps | n <= 0 = seal ps
dropn n (PatchSet NilRL (Tagged t _ ps :<: ts)) = dropn n $ PatchSet (t:<:ps) ts
dropn _ (PatchSet NilRL NilRL) = seal $ PatchSet NilRL NilRL
dropn n (PatchSet (p:<:ps) ts)
    | isIsEq (flIsInternal (patchcontents (hopefully p)))
   = dropn n $ PatchSet ps ts
dropn n (PatchSet (_:<:ps) ts) = dropn (n-1) $ PatchSet ps ts

-- | @matchSecondPatchset fs ps@ returns the part of @ps@ before its
-- second matcher, ie the one that comes last dependencywise.
matchSecondPatchset :: Matchable p => [MatchFlag] -> PatchSet p wStart wX
                    -> SealedPatchSet p wStart
matchSecondPatchset fs ps =
  case hasIndexRange fs of
  Just (a,_) -> dropn (a-1) ps
  Nothing ->
    case secondMatcher fs of
    Nothing -> bug "Couldn't matchSecondPatchset"
    Just m -> if secondMatcherIsTag fs
              then getMatchingTag m ps
              else matchAPatchset m ps

-- | Split on the second matcher. Note that this picks up the first match starting from
-- the earliest patch in a sequence, as opposed to 'matchSecondPatchset' which picks up the
-- first match starting from the latest patch
splitSecondFL :: Matchable p
              => (forall wA wB . q wA wB -> Sealed2 (PatchInfoAnd p))
              -> [MatchFlag]
              -> FL q wX wY
              -> (FL q :> FL q) wX wY -- ^The first element is the patches before and including the first patch matching the second matcher,
                                      --  the second element is the patches after it
splitSecondFL extract fs ps =
   case hasIndexRange fs of
   Just _ -> -- selecting the last n doesn't really make sense if we're starting from the earliest patches
             bug "index matches not supported by splitSecondPatchesFL"
   Nothing ->
     case secondMatcher fs of
     Nothing -> bug "Couldn't splitSecondPatches"
     Just m -> splitMatchFL extract m ps

-- | @findAPatch m ps@ returns the last patch in @ps@ matching @m@, and
-- calls 'error' if there is none.
findAPatch :: Matchable p => Matcher p -> PatchSet p wStart wX -> Sealed2 (Named p)
findAPatch m (PatchSet NilRL NilRL) = error $ "Couldn't find patch matching " ++ show m
findAPatch m (PatchSet NilRL (Tagged t _ ps :<: ts)) = findAPatch m (PatchSet (t:<:ps) ts)
findAPatch m (PatchSet (p:<:ps) ts) | applyMatcher m p = seal2 $ hopefully p
                                    | otherwise = findAPatch m (PatchSet ps ts)

-- | @matchAPatchset m ps@ returns a (the largest?) subset of @ps@
-- ending in patch which matches @m@. Calls 'error' if there is none.
matchAPatchset :: Matchable p => Matcher p -> PatchSet p wStart wX
               -> SealedPatchSet p wStart
matchAPatchset m (PatchSet NilRL NilRL) = error $ "Couldn't find patch matching " ++ show m
matchAPatchset m (PatchSet NilRL (Tagged t _ ps :<: ts)) = matchAPatchset m (PatchSet (t:<:ps) ts)
matchAPatchset m (PatchSet (p:<:ps) ts) | applyMatcher m p = seal (PatchSet (p:<:ps) ts)
                                        | otherwise = matchAPatchset m (PatchSet ps ts)

-- | @getMatchingTag m ps@, where @m@ is a 'Matcher' which matches tags
-- returns a 'SealedPatchSet' containing all patches in the last tag which
-- matches @m@. Last tag means the most recent tag in repository order,
-- i.e. the last one you'd see if you ran darcs changes -t @m@. Calls
-- 'error' if there is no matching tag.
getMatchingTag :: Matchable p => Matcher p -> PatchSet p wStart wX -> SealedPatchSet p wStart
getMatchingTag m (PatchSet NilRL NilRL) = error $ "Couldn't find a tag matching " ++ show m
getMatchingTag m (PatchSet NilRL (Tagged t _ ps :<: ts)) = getMatchingTag m (PatchSet (t:<:ps) ts)
getMatchingTag m (PatchSet (p:<:ps) ts)
    | applyMatcher m p =
        -- found a non-clean tag, need to commute out the things that it doesn't depend on
        case splitOnTag (info p) (PatchSet (p:<:ps) ts) of
            Nothing -> bug "splitOnTag couldn't find tag we explicitly provided!"
            Just (patchSet :> _) -> seal patchSet
    | otherwise = getMatchingTag m (PatchSet ps ts)

splitMatchFL :: Matchable p => (forall wA wB . q wA wB -> Sealed2 (PatchInfoAnd p)) -> Matcher p -> FL q wX wY -> (FL q :> FL q) wX wY
splitMatchFL _extract m NilFL = error $ "Couldn't find patch matching " ++ show m
splitMatchFL extract m (p :>: ps)
   | unseal2 (applyMatcher m) . extract $ p = (p :>: NilFL) :> ps
   | otherwise = case splitMatchFL extract m ps of
                    before :> after -> (p :>: before) :> after

-- | @matchExists m ps@ tells whether there is a patch matching
-- @m@ in @ps@
matchExists :: Matcher p -> PatchSet p wStart wX -> Bool
matchExists _ (PatchSet NilRL NilRL) = False
matchExists m (PatchSet NilRL (Tagged t _ ps :<: ts)) = matchExists m (PatchSet (t:<:ps) ts)
matchExists m (PatchSet (p:<:ps) ts) | applyMatcher m p = True
                                     | otherwise = matchExists m (PatchSet ps ts)

applyInvToMatcher :: (Matchable p, ApplyMonad m (ApplyState p))
                  => InclusiveOrExclusive -> Matcher p -> PatchSet p Origin wX -> m ()
applyInvToMatcher _ _ (PatchSet NilRL NilRL) = impossible
applyInvToMatcher ioe m (PatchSet NilRL (Tagged t _ ps :<: ts)) = applyInvToMatcher ioe m
                                                                  (PatchSet (t:<:ps) ts)
applyInvToMatcher ioe m (PatchSet (p:<:ps) xs)
    | applyMatcher m p = when (ioe == Inclusive) (applyInvp p)
    | otherwise = applyInvp p >> applyInvToMatcher ioe m (PatchSet ps xs)

-- | @applyNInv@ n ps applies the inverse of the last @n@ patches of @ps@.
applyNInv :: (Matchable p, ApplyMonad m (ApplyState p)) => Int -> PatchSet p Origin wX -> m ()
applyNInv n _ | n <= 0 = return ()
applyNInv _ (PatchSet NilRL NilRL) = error "Index out of range."
applyNInv n (PatchSet NilRL (Tagged t _ ps :<: ts)) =
  applyNInv n (PatchSet (t :<: ps) ts)
applyNInv n (PatchSet (p :<: ps) xs) =
  applyInvp p >> applyNInv (n - 1) (PatchSet ps xs)


getMatcherS :: (ApplyMonad m (ApplyState p), Matchable p) =>
                 InclusiveOrExclusive -> Matcher p -> PatchSet p Origin wX -> m ()
getMatcherS ioe m repo =
    if matchExists m repo
    then applyInvToMatcher ioe m repo
    else fail $ "Couldn't match pattern "++ show m

getTagS :: (ApplyMonad m (ApplyState p), MonadProgress m, Matchable p) =>
             Matcher p -> PatchSet p Origin wX -> m ()
getTagS matcher repo = do
    let pinfo = patch2patchinfo `unseal2` findAPatch matcher repo
    case getPatchesBeyondTag pinfo repo of
        FlippedSeal extras -> applyInvRL extras

-- | @applyInvp@ tries to get the patch that's in a 'PatchInfoAnd
-- patch', and to apply its inverse. If we fail to fetch the patch
-- (presumably in a partial repositiory), then we share our sorrow
-- with the user.
applyInvp :: (Patchy p, ApplyMonad m (ApplyState p)) => PatchInfoAnd p wX wY -> m ()
applyInvp hp = apply (invert $ fromHopefully hp)
    where fromHopefully = conscientiously $ \e ->
                     text "Sorry, partial repository problem.  Patch not available:"
                     $$ e
                     $$ text ""
                     $$ text "If you think what you're trying to do is ok then"
                     $$ text "report this as a bug on the darcs-user list."

-- | a version of 'take' for 'RL' lists that cater for contexts.
safetake :: MaybeInternal p => Int -> RL (PatchInfoAnd p) wX wY -> FlippedSeal (RL (PatchInfoAnd p)) wY
safetake 0 _ = flipSeal NilRL
safetake _ NilRL = error "There aren't that many patches..."
safetake i (a:<:as) | isIsEq (flIsInternal (patchcontents (hopefully a))) = a `consRLSealed` safetake i as
safetake i (a:<:as) = a `consRLSealed` safetake (i-1) as

applyInvRL :: (ApplyMonad m (ApplyState p), MonadProgress m, Patchy p) => RL (PatchInfoAnd p) wX wR -> m ()
applyInvRL = applyPatches . invertRL -- this gives nicer feedback
