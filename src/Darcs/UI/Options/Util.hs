{-# LANGUAGE RecordWildCards #-}
-- | Constructing 'OptSpec's and 'OptDescr's
module Darcs.UI.Options.Util
    ( Flag
    , PrimDarcsOption
    , DarcsOptDescr
    , noArg
    , strArg
    , optStrArg
    , absPathArg
    , absPathOrStdArg
    , optAbsPathArg
    , RawOptSpec(..)
    , withDefault
    , singleNoArg
    , singleStrArg
    , multiStrArg
    , multiOptStrArg
    , singleAbsPathArg
    , multiAbsPathArg
    , deprecated
    -- Re-exports
    , AbsolutePath
    , AbsolutePathOrStd
    , makeAbsolute
    , makeAbsoluteOrStd
    ) where

import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )
import Data.Functor.Compose
import Data.List ( intercalate )
import Data.Maybe ( maybeToList, fromMaybe )
import Darcs.UI.Options.Core
import Darcs.UI.Options.Flags ( DarcsFlag )
import Darcs.UI.Options.Iso
import Darcs.Util.Path
    ( AbsolutePath
    , AbsolutePathOrStd
    , makeAbsolute
    , makeAbsoluteOrStd
    )

-- * Instantiating 'OptSpec' and 'PrimOptSpec'

-- | This type synonym is here for brevity and because we want to import
-- the data constructors (but not the type) of 'DarcsFlag' qualified.
type Flag = DarcsFlag

{- | We do not instantiate the @d@ in @'OptSpec' d f@ directly with
'System.Console.GetOpt.OptDescr'. Instead we (post-) compose it with @(->)
'DarcsUtil.Path.AbsolutePath'@. Modulo newtype noise, this is the same as

@ type 'DarcsOptDescr f = 'System.Console.GetOpt.OptDescr' ('AbsolutePath' -> f)@

This is so we can pass a directory relative to which an option argument is
interpreted (if it has the form of a relative path).
-}
type DarcsOptDescr = Compose OptDescr ((->) AbsolutePath)

-- | This is 'PrimOptSpec' instantiated with 'DarcsOptDescr and 'Flag'.
type PrimDarcsOption v = forall a. PrimOptSpec DarcsOptDescr Flag a v

-- * Constructing 'OptDescr's

-- | Construct an 'DarcsOptDescr with no arguments.
noArg :: [Char] -> [String] -> f -> String -> DarcsOptDescr f
noArg s l f h = Compose $ Option s l (NoArg (const f)) h

-- | A 'DarcsOptDescr' that requires a single argument of type 'a' and handles
-- flags of type 'f'.
type SingleArgOptDescr a f =
        [Char] -> [String] -> (a -> f) -> String -> String -> DarcsOptDescr f

-- | Construct an 'DarcsOptDescr with a 'String' argument.
strArg :: SingleArgOptDescr String f
strArg s l f a h = Compose $ Option s l (ReqArg (\x _ -> f x) a) h

-- | Construct an 'DarcsOptDescr with an optional 'String' argument.
optStrArg :: SingleArgOptDescr (Maybe String) f
optStrArg s l f a h = Compose $ Option s l (OptArg (\x _ -> f x) a) h

-- | Construct an 'DarcsOptDescr with an 'AbsolutePath'
-- argument.
absPathArg :: SingleArgOptDescr AbsolutePath f
absPathArg s l f a h = Compose $ Option s l (ReqArg (\x wd -> f $ makeAbsolute wd x) a) h

-- | Construct an 'DarcsOptDescr with an 'AbsolutePathOrStd'
-- argument.
absPathOrStdArg :: SingleArgOptDescr AbsolutePathOrStd f
absPathOrStdArg s l f a h = Compose $ Option s l (ReqArg (\x wd -> f $ makeAbsoluteOrStd wd x) a) h

-- | Construct an 'DarcsOptDescr with an optional 'AbsolutePath'
-- argument.
optAbsPathArg :: [Char] -> [String] -> String -> (AbsolutePath -> f)
              -> String -> String -> DarcsOptDescr f
optAbsPathArg s l d f a h = Compose $ Option s l (OptArg (\x wd -> f $ makeAbsolute wd $ fromMaybe d x) a) h

-- * Raw option specs

-- | The raw material from which multi-valued options are built. See 'withDefault'.
data RawOptSpec f v
  = RawNoArg [Char] [String] f v String
  | RawStrArg [Char] [String] (String -> f) (f -> [String]) (String -> v) (v -> [String])
      String String
  | RawAbsPathArg [Char] [String] (AbsolutePath -> f) (f -> [AbsolutePath])
      (AbsolutePath -> v) (v -> [AbsolutePath]) String String
  | RawAbsPathOrStdArg [Char] [String] (AbsolutePathOrStd -> f) (f -> [AbsolutePathOrStd])
      (AbsolutePathOrStd -> v) (v -> [AbsolutePathOrStd]) String String
  | RawOptAbsPathArg [Char] [String] (AbsolutePath -> f) (f -> [AbsolutePath])
      (AbsolutePath -> v) (v -> [AbsolutePath]) String String String

instance IsoFunctor (RawOptSpec f) where
  imap (Iso fw _)  (RawNoArg s l f v h) = RawNoArg s l f (fw v) h
  imap (Iso fw bw) (RawStrArg s l mkF unF mkV unV n h) = RawStrArg s l mkF unF (fw . mkV) (unV . bw) n h
  imap (Iso fw bw) (RawAbsPathArg s l mkF unF mkV unV n h) = RawAbsPathArg s l mkF unF (fw . mkV) (unV . bw) n h
  imap (Iso fw bw) (RawAbsPathOrStdArg s l mkF unF mkV unV n h) = RawAbsPathOrStdArg s l mkF unF (fw . mkV) (unV . bw) n h
  imap (Iso fw bw) (RawOptAbsPathArg s l mkF unF mkV unV d n h) = RawOptAbsPathArg s l mkF unF (fw . mkV) (unV . bw) d n h

-- | Get the long switch names from a raw option. Used to construct error messages.
switchNames :: RawOptSpec f v -> [String]
switchNames (RawNoArg _ l _ _ _)                 = l
switchNames (RawStrArg _ l _ _ _ _ _ _)          = l
switchNames (RawAbsPathArg _ l _ _ _ _ _ _)      = l
switchNames (RawAbsPathOrStdArg _ l _ _ _ _ _ _) = l
switchNames (RawOptAbsPathArg _ l _ _ _ _ _ _ _) = l

-- | Given a list of 'RawOptSpec', find all flags that match a given value.
rawUnparse :: Eq v => [RawOptSpec f v] -> v -> [f]
rawUnparse ropts val =
     [ f | RawNoArg _ _ f v _ <- ropts, v == val ]
  ++ [ mkF s | RawStrArg _ _ mkF _ mkV unV _ _ <- ropts, s <- unV val, mkV s == val ]
  ++ [ mkF p | RawAbsPathArg _ _ mkF _ mkV unV _ _ <- ropts, p <- unV val, mkV p == val ]
  ++ [ mkF p | RawAbsPathOrStdArg _ _ mkF _ mkV unV _ _ <- ropts, p <- unV val, mkV p == val ]
  ++ [ mkF p | RawOptAbsPathArg _ _ mkF _ mkV unV _ _ _ <- ropts, p <- unV val, mkV p == val ]

-- | Given a list of 'RawOptSpec', find all values that match a given flag list
-- in the order in which they appear in the flag list.
rawParse :: Eq f => [RawOptSpec f v] -> [f] -> [(v,RawOptSpec f v)]
rawParse ropts = concatMap rawParseFlag where
  rawParseFlag f = concatMap (go f) ropts
  go f o@(RawNoArg _ _ f' v _)                    = [ (v, o) | f == f' ]
  go f o@(RawStrArg _ _ _ unF mkV _ _ _)          = [ (mkV s, o) | s <- unF f ]
  go f o@(RawAbsPathArg _ _ _ unF mkV _ _ _)      = [ (mkV p, o) | p <- unF f ]
  go f o@(RawAbsPathOrStdArg _ _ _ unF mkV _ _ _) = [ (mkV p, o) | p <- unF f ]
  go f o@(RawOptAbsPathArg _ _ _ unF mkV _ _ _ _) = [ (mkV p, o) | p <- unF f ]

--      [ (v, o)     | f <- fs, o@(RawNoArg _ _ f' v _) <- ropts, f == f' ]
--   ++ [ (mkV s, o) | f <- fs, o@(RawStrArg _ _ _ unF mkV _ _ _) <- ropts, s <- unF f ]
--   ++ [ (mkV p, o) | f <- fs, o@(RawAbsPathArg _ _ _ unF mkV _ _ _) <- ropts, p <- unF f ]
--   ++ [ (mkV p, o) | f <- fs, o@(RawAbsPathOrStdArg _ _ _ unF mkV _ _ _) <- ropts, p <- unF f ]
--   ++ [ (mkV p, o) | f <- fs, o@(RawOptAbsPathArg _ _ _ unF mkV _ _ _ _) <- ropts, p <- unF f ]

-- | The first element of a list, or a default if the list is empty.
defHead :: a -> [a] -> a
defHead def []    = def
defHead _   (x:_) = x

-- | Append \" [DEFAULT\" to the help text of options that match the default value.
addDefaultHelp :: Eq v => v -> RawOptSpec f v -> DarcsOptDescr f
addDefaultHelp dval (RawNoArg s l f v h)
  | dval == v = noArg s l f (h++" [DEFAULT]")
  | otherwise = noArg s l f h
addDefaultHelp dval (RawStrArg s l mkF _ mkV unV a h)
  | [dval] == map mkV (unV dval) = strArg s l mkF a (h++" [DEFAULT]")
  | otherwise = strArg s l mkF a h
addDefaultHelp dval (RawAbsPathArg s l mkF _ mkV unV a h)
  | [dval] == map mkV (unV dval) = absPathArg s l mkF a (h++" [DEFAULT]")
  | otherwise = absPathArg s l mkF a h
addDefaultHelp dval (RawAbsPathOrStdArg s l mkF _ mkV unV a h)
  | [dval] == map mkV (unV dval) = absPathOrStdArg s l mkF a (h++" [DEFAULT]")
  | otherwise = absPathOrStdArg s l mkF a h
addDefaultHelp dval (RawOptAbsPathArg s l mkF _ mkV unV d a h)
  | [dval] == map mkV (unV dval) = optAbsPathArg s l d mkF a (h++" [DEFAULT]")
  | otherwise = optAbsPathArg s l d mkF a h

-- | Construct a 'PrimDarcsOption' from a default value and a list of 'RawOptSpec'.
--
-- Precondition: the list must have an entry for each possible value (type @v@).
withDefault :: Eq v => v -> [RawOptSpec Flag v] -> PrimDarcsOption v
withDefault dval ropts = OptSpec {..} where
  ounparse k = k . rawUnparse ropts
  oparse k = k . defHead dval . map fst . rawParse ropts
  ocheck fs = case map snd (rawParse ropts fs) of
    [] -> [] -- error "this should not happen"
    [_] -> []
    ropts' -> ["conflicting options: " ++ intercalate ", " (map (intercalate "/" . switchNames) ropts')]
  odesc = map (addDefaultHelp dval) ropts

-- * Simple primitive scalar valued options

-- | Construct a 'Bool' valued option with a single flag that takes no arguments
-- and has no default flag.
--
-- The arguments are: short switches, long switches, flag value, help string.
singleNoArg :: [Char] -> [String] -> Flag -> String -> PrimDarcsOption Bool
singleNoArg s l f h = withDefault False [RawNoArg s l f True h]

-- | Construct a @'Maybe' 'String'@ valued option with a single flag that takes a
-- 'String' argument and has no default flag.
--
-- The arguments are: short switches, long switches, flag constructor, single flag
-- parser, help string.
singleStrArg :: [Char] -> [String] -> (String -> Flag) -> (Flag -> Maybe String)
             -> String -> String -> PrimDarcsOption (Maybe String)
singleStrArg s l mkf isf n h =
  withDefault Nothing [ RawStrArg s l mkf (maybeToList . isf) Just maybeToList n h ]

-- | Construct a @'Maybe' 'AbsolutePath'@ valued option with a single flag that
-- takes an 'AbsolutePath' argument and has no default flag.
--
-- The arguments are: short switches, long switches, flag constructor, single flag
-- parser, help string.
singleAbsPathArg :: [Char] -> [String]
             -> (AbsolutePath -> Flag) -> (Flag -> Maybe AbsolutePath)
             -> String -> String -> PrimDarcsOption (Maybe AbsolutePath)
singleAbsPathArg s l mkf isf n h =
  withDefault Nothing [ RawAbsPathArg s l mkf (maybeToList . isf) Just maybeToList n h ]

-- * Simple primitive list valued options

-- | Similar to 'singleStrArg', except that the flag can be given more than once.
-- The flag arguments are collected in a list of 'String's.
multiStrArg :: [Char] -> [String] -> (String -> Flag) -> ([Flag] -> [String])
             -> String -> String -> PrimDarcsOption [String]
multiStrArg = multiArg strArg

-- | Similar to 'multiStrArg', except that the flag arguments are optional.
multiOptStrArg :: [Char] -> [String] -> (Maybe String -> Flag)
               -> ([Flag] -> [Maybe String]) -> String -> String
               -> PrimDarcsOption [Maybe String]
multiOptStrArg = multiArg optStrArg

-- | Similar to 'singleAbsPathArg', except that the flag can be given more than once.
-- The flag arguments are collected in a list of 'AbsolutePath's.
multiAbsPathArg :: [Char] -> [String] -> (AbsolutePath -> Flag) -> ([Flag] -> [AbsolutePath])
             -> String -> String -> PrimDarcsOption [AbsolutePath]
multiAbsPathArg = multiArg absPathArg

-- | A multi-arg option, defined in terms of a single-arg option, returning a
-- list of single args.
--
-- The parameters are: single argument description, short switches, long
-- switches, flag constructor, flag list parser, arg name string, help string.
multiArg :: SingleArgOptDescr a Flag
         -> [Char] -> [String] -> (a -> Flag) -> ([Flag] -> [a])
         -> String -> String -> PrimDarcsOption [a]
multiArg singleArg s l mkf isf n h = OptSpec {..} where
  ounparse k xs = k [ mkf x | x <- xs ]
  oparse k = k . isf
  ocheck _ = []
  odesc = [singleArg s l mkf n h]

-- | A deprecated option. If you want to deprecate only some flags and not the
-- whole option, extract the 'RawOptSpec's out of the original option and create
-- a new deprecated option.
-- The strings in the first argument are appended to the automatically generated
-- error message in case additional hints should be provided.
deprecated :: [String] -> [RawOptSpec Flag v] -> PrimDarcsOption ()
deprecated comments ropts = OptSpec {..} where
  ounparse k _ = k []
  oparse k _ = k ()
  ocheck fs = case map snd (rawParse ropts fs) of
    [] -> []
    ropts' -> ("deprecated option(s): " ++ intercalate ", " (concatMap switchNames ropts')) : comments
  odesc = map noDefaultHelp ropts
  noDefaultHelp (RawNoArg s l f _ h) = noArg s l f h
  noDefaultHelp (RawStrArg s l mkF _ _ _ a h) = strArg s l mkF a h
  noDefaultHelp (RawAbsPathArg s l mkF _ _ _ a h) = absPathArg s l mkF a h
  noDefaultHelp (RawAbsPathOrStdArg s l mkF _ _ _ a h) = absPathOrStdArg s l mkF a h
  noDefaultHelp (RawOptAbsPathArg s l mkF _ _ _ d a h) = optAbsPathArg s l d mkF a h
