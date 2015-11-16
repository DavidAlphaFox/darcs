-- | This module provides a variant of 'System.Console.GetOpt.usageInfo'.
--
--  Unlike the standard @usageInfo@ function, lists of long switches are broken
--  across multiple lines to economise on columns. For example,
--
--  @
--    -r  --recursive           add contents of subdirectories
--        --not-recursive,
--        --no-recursive        don't add contents of subdirectories
--  @

module Darcs.UI.Usage ( usageInfo ) where
import Data.Functor.Compose
import System.Console.GetOpt( OptDescr(..), ArgDescr(..) )
import Darcs.UI.Options ( DarcsOptDescr )

-- | Variant of 'System.Console.GetOpt.usageInfo'.
-- Return a string describing the usage of a command, derived from the header
-- (first argument) and the options described by the second argument.
--
-- Sequences of long switches are presented on separate lines.
usageInfo :: String         -- header
          -> [DarcsOptDescr a]    -- option descriptors
          -> String          -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table)
   where (ss,ls,ds)     = (unzip3 . concatMap fmtOpt) optDescr
         table          = zipWith3 paste
                            shortPadded
                            (zipWith (++) (map (unlines' . init) ls)
                                          (sameLen $ map last ls))
                            ds
         shortPadded    = sameLen ss
         prePad         = replicate (4 + length (head shortPadded)) ' '
         -- Similar to unlines (additional ',' and padding):
         unlines'       = concatMap (\x -> x ++ ",\n" ++ prePad)
         -- Unchanged:
         paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
         sameLen xs     = flushLeft ((maximum . map length) xs) xs
         flushLeft n xs = [ take n (x ++ repeat ' ') | x <- xs ]

-- Mild variant of the standard definition: 'losFmt' is a list rather than a
-- comma separated string.
fmtOpt :: DarcsOptDescr a -> [(String,[String],String)]
fmtOpt (Compose (Option sos los ad descr)) =
   case lines descr of
     []     -> [(sosFmt,losFmt,"")]
     (d:ds) ->  (sosFmt,losFmt,d) : [ ("",[],d') | d' <- ds ]
   where sepBy _  []     = ""
         sepBy _  [x]    = x
         sepBy ch (x:xs) = x ++ ch:' ':sepBy ch xs
         sosFmt = sepBy ',' (map (fmtShort ad) sos)
         losFmt = map (fmtLong ad) los

--------------------------------------------------------------------------------
-- Verbatim copies: these definitions aren't exported by System.Console.GetOpt
--------------------------------------------------------------------------------

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = ['-', so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"
