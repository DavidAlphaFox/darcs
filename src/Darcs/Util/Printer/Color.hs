{-# LANGUAGE CPP #-}
module Darcs.Util.Printer.Color
    ( showDoc, errorDoc, traceDoc, assertDoc, fancyPrinters
    , environmentHelpColor, environmentHelpEscape, environmentHelpEscapeWhite
    ) where

import Darcs.Util.Printer
    ( Printer, Printers, Printers'(..), Printable(..), Color(..), RenderMode(..)
    , invisiblePrinter, (<>), (<?>), Doc(Doc,unDoc), unsafeBothText, simplePrinter, hcat
    , unsafeText, unsafePackedString
    , renderStringWith, prefix
    )

import Prelude hiding ( catch )
import Control.Monad ( liftM )
import Control.Exception ( catch, IOException )
import Debug.Trace ( trace )
import Data.Char ( isAscii, isPrint, isSpace, isControl, ord, chr )
import Data.Bits ( bit, xor )
import System.Environment ( getEnv )
import qualified Data.ByteString.Char8 as BC (unpack, any, last, spanEnd)
import qualified Data.ByteString       as B (null, init)
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ( stderr, hIsTerminalDevice, Handle )
import Text.Printf ( printf )
#ifdef HAVE_TERMINFO
import System.Console.Terminfo( tiGetNum, setupTermFromEnv, getCapability )
import Data.Maybe ( fromMaybe )
#endif

dollar, cr :: Doc
dollar = unsafeBothText "$"
cr     = unsafeBothText "\r"

errorDoc :: Doc -> a
errorDoc = error . showDoc Encode

traceDoc :: Doc -> a -> a
traceDoc d = trace (showDoc Encode d)

assertDoc :: Maybe Doc -> a -> a
assertDoc Nothing x = x
assertDoc (Just e) _ = errorDoc e

showDoc :: RenderMode -> Doc -> String
showDoc = renderStringWith (fancyPrinters stderr)

-- policy
-- | the 'Policy' type is a record containing the variables which control
-- how 'Doc's will be rendered on some output.
data Policy = Policy { poColor :: Bool    -- ^ overall use of color
                     , poEscape :: Bool   -- ^ overall use of escaping
                     , poLineColor :: Bool -- ^ overall use of colored lines (only hunks for now)
                     , poAltColor :: Bool -- ^ alternative to color (bold, inverse)
                     , poIsprint :: Bool  -- ^ don't escape isprints
                     , po8bit  :: Bool    -- ^ don't escape 8-bit chars
                     , poNoEscX :: String   -- ^ extra chars to never escape
                     , poEscX :: String   -- ^ extra chars to always escape
                     , poTrailing :: Bool -- ^ escape trailing spaces
                     , poCR :: Bool       -- ^ ignore \r at end of lines
                     , poSpace :: Bool    -- ^ escape spaces (used with poTrailing)
                     }

{-# NOINLINE getPolicy #-}
-- | 'getPolicy' returns a suitable policy for a given handle.
-- The policy is chosen according to environment variables, and to the
-- type of terminal which the handle represents
getPolicy :: Handle -> Policy
getPolicy handle = unsafePerformIO $
 do isTerminal <- hIsTerminalDevice handle
    nColors <- if isTerminal then getTermNColors else return 0

    envDontEscapeAnything  <- getEnvBool "DARCS_DONT_ESCAPE_ANYTHING"
    envDontEscapeIsprint   <- getEnvBool "DARCS_DONT_ESCAPE_ISPRINT"
    envUseIsprint          <- getEnvBool "DARCS_USE_ISPRINT" -- for backwards-compatibility
    envDontEscape8bit      <- getEnvBool "DARCS_DONT_ESCAPE_8BIT"

    envDontEscapeExtra  <- getEnvString "DARCS_DONT_ESCAPE_EXTRA"
    envEscapeExtra      <- getEnvString "DARCS_ESCAPE_EXTRA"

    envDontEscapeTrailingSpace  <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_SPACES"
    envDontEscapeTrailingCR     <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_CR"

    envDontColor         <- getEnvBool "DARCS_DONT_COLOR"
    envAlwaysColor       <- getEnvBool "DARCS_ALWAYS_COLOR"
    envAlternativeColor  <- getEnvBool "DARCS_ALTERNATIVE_COLOR"
    envDoColorLines    <- getEnvBool "DARCS_DO_COLOR_LINES"

    let haveColor = envAlwaysColor || (isTerminal && (nColors > 4))
        doColor   = not envDontColor && haveColor

    return Policy { poColor    = doColor,
                    poEscape   = not envDontEscapeAnything,
                    poLineColor= doColor && envDoColorLines,
                    poIsprint  = envDontEscapeIsprint || envUseIsprint,
                    po8bit     = envDontEscape8bit,
                    poNoEscX   = envDontEscapeExtra,
                    poEscX     = envEscapeExtra,
                    poTrailing = not envDontEscapeTrailingSpace,
                    poCR       = envDontEscapeTrailingCR,
                    poAltColor = haveColor && envAlternativeColor,

                    poSpace = False
                  }
 where
  getEnvBool s = (/= "0") `liftM` safeGetEnv s
  safeGetEnv s = getEnv s `catch` \(_ :: IOException) -> return "0"
  getEnvString s = getEnv s `catch` \(_ :: IOException) -> return ""


{-
  - This function returns number of colors supported by current terminal
  - or -1 if color output not supported or error occured.
  - Terminal type determined by TERM env. variable.
  -}
getTermNColors :: IO Int
#ifdef HAVE_TERMINFO
getTermNColors = do
  t <- setupTermFromEnv
  return . fromMaybe (-1) . getCapability t . tiGetNum $ "colors"
#else
getTermNColors = return (-1)
#endif



-- printers

-- | @'fancyPrinters' h@ returns a set of printers suitable for outputting
-- to @h@
fancyPrinters :: Printers
fancyPrinters h = let policy = getPolicy h in
                      Printers { colorP = colorPrinter policy,
                             invisibleP = invisiblePrinter,
                             hiddenP = colorPrinter policy Green,
                             userchunkP = userchunkPrinter policy,
                             defP       = escapePrinter policy,
                             lineColorT = lineColorTrans policy,
                             lineColorS = lineColorSuffix policy
                           }

-- | @'lineColorTrans' policy@ tries to color a Doc, according to policy po.
-- That is, if @policy@ has @poLineColor@ set, then colors the line, otherwise
-- does nothing.
lineColorTrans :: Policy -> Color -> Doc -> Doc
lineColorTrans po | poLineColor po = \c d -> prefix (setColor c) d <?> unsafeBothText resetColor
                  | otherwise      = const id

lineColorSuffix :: Policy -> [Printable] -> [Printable]
lineColorSuffix po | poLineColor po = \d -> S resetColor : d
                   | otherwise      = id

colorPrinter :: Policy -> Color -> Printer
colorPrinter po | poColor po = \c -> unDoc . color po c . Doc . escapePrinter po{poColor=False}
                | otherwise  = const $ escapePrinter po

userchunkPrinter :: Policy -> Printer
userchunkPrinter po p
 | not (poEscape po)   = simplePrinter p
 | not (poTrailing po) = escapePrinter po p
 | otherwise           = unDoc $ pr p
 where
  pr (S s)       = prString s
  pr (Both _ ps) = prPS ps
  pr (PS ps)     = prPS ps

  prPS ps = let (leadPS, trailPS) = BC.spanEnd isSpace ps
            in if B.null trailPS
                then Doc $ escapePrinter po p
                else Doc (escapePrinter po (PS leadPS))
                  <> Doc (escapePrinter po{poSpace=True} (PS trailPS))
                  <> markEscape po dollar

  prString s = let (trail',lead') = span isSpace (reverse s)
                   lead = reverse lead'
                   trail = reverse trail'
               in if (not.null) trail
                   then Doc (escapePrinter po (S lead))
                     <> Doc (escapePrinter po{poSpace=True} (S trail))
                     <> markEscape po dollar
                   else Doc (escapePrinter po p)

escapePrinter :: Policy -> Printer
escapePrinter po
 | (not.poEscape) po = simplePrinter
 | otherwise         = unDoc . crepr
 where
  crepr p | poCR po && isEndCR p = epr (initPR p) <> cr
          | otherwise            = epr p

  epr (S s)      = escape po s
  epr (PS ps)    = if BC.any (not.noEscape po) ps
                   then escape po (BC.unpack ps)
                   else unsafePackedString ps
  epr (Both s _) = escape po s

  isEndCR (S s)        = not (null s) && last s == '\r'
  isEndCR (PS ps)      = not (B.null ps) && BC.last ps == '\r'
  isEndCR (Both _ ps)  = not (B.null ps) && BC.last ps == '\r'

  initPR (S s)       = S $ init s
  initPR (PS ps)     = PS $ B.init ps
  initPR (Both s ps) = Both (init s) (B.init ps)


-- | @'escape' policy string@ escapes @string@ according to the rules
-- defined in 'policy', turning it into a 'Doc'.
escape :: Policy -> String -> Doc
escape _ "" = unsafeText ""
escape po s = hcat $ escape' s
 where
   escape' "" = []
   escape' s'@(c:_) | mundane c =
     let (printables, rest) = span mundane s' in
     unsafeText printables:escape' rest
   escape' (c:rest) = (emph . unsafeText $ quoteChar c):escape' rest
   mundane c = noEscape po c || c == ' '
   emph = markEscape po


-- | @'noEscape' policy c@ tells wether @c@ will be left as-is
-- when escaping according to @policy@
noEscape :: Policy -> Char -> Bool
noEscape po c | poSpace po && isSpace c = False
noEscape po c | c `elem` poEscX po = False
noEscape po c | c `elem` poNoEscX po = True
noEscape _ '\t' = True  -- tabs will likely be converted to spaces
noEscape _ '\n' = True
noEscape po c = if poIsprint po then isPrint c
                                   else isPrintableAscii c
                 ||  c >= '\x80' && po8bit po

-- | 'isPrintableAscii' tells wether a character is a printable character
-- of the ascii range.
isPrintableAscii :: Char -> Bool
isPrintableAscii c = isAscii c && isPrint c


-- | 'quoteChar' represents a special character as a string.
--   * @quoteChar '^c'@ (where @^c@ is a control character) is @"^c"@
--   * Otherwise, @quoteChar@ returns "\hex", where 'hex' is the
--     hexadecimal number of the character.
quoteChar :: Char -> String
quoteChar c
 | isControl c && isPrintableAscii cHat = ['^', cHat]
 | otherwise = sHex
 where
  cHat = chr $ (bit 6 `xor`) $ ord c
  sHex = "<U+" ++ printf "%04X" c ++ ">"


-- make colors and highlightings

-- | @'markEscape' policy doc@ marks @doc@ with the appropriate
-- marking for escaped characters according to @policy@
markEscape :: Policy -> Doc -> Doc
markEscape po  | poAltColor po  = makeInvert
               | poColor po     = makeColor Red
               | otherwise      = makeAsciiart

-- | @'color' policy color doc@ colors @doc@ with color @color@ if
-- @policy@ is not set to use an alternative to color. In that case,
-- it makes the text bold instead.
color :: Policy -> Color -> Doc -> Doc
color po | poAltColor po = \_ -> makeBold
         | otherwise     = makeColor

makeColor, makeColor' :: Color -> Doc -> Doc

makeColor' = withColor . setColor

-- memoized version of makeColor'
makeColor Blue    = makeColor' Blue
makeColor Red     = makeColor' Red
makeColor Green   = makeColor' Green
makeColor Cyan    = makeColor' Cyan
makeColor Magenta = makeColor' Magenta

setColor :: Color -> String
setColor Blue    = "\x1B[01;34m" -- bold blue
setColor Red     = "\x1B[01;31m" -- bold red
setColor Green   = "\x1B[01;32m" -- bold green
setColor Cyan    = "\x1B[36m"    -- light cyan
setColor Magenta = "\x1B[35m"    -- light magenta

-- | @'makeAsciiart' doc@ tries to make @doc@ (usually a
-- single escaped char) stand out with the help of only plain
-- ascii, i.e., no color or font style.
makeAsciiart :: Doc -> Doc
makeAsciiart x = unsafeBothText "[_" <> x <> unsafeBothText "_]"

-- | the string to reset the terminal's color.
resetColor :: String
resetColor = "\x1B[00m"

-- | @'withColor' color doc@ returns a colorized version of @doc@.
-- @color@ is a string that represents a color, given by 'setColor'
withColor :: String -> Doc -> Doc
withColor c =
   let c' = unsafeBothText c
       r' = unsafeBothText resetColor
   in \x -> c' <> x <> r'


-- | 'makeBold' boldens a doc.
makeBold :: Doc -> Doc
-- | 'makeInvert' returns an invert video version of a doc.
makeInvert :: Doc -> Doc
makeBold   = withColor "\x1B[01m"
makeInvert = withColor "\x1B[07m"

environmentHelpColor :: ([String], [String])
environmentHelpColor = (["DARCS_DONT_COLOR", "DARCS_ALWAYS_COLOR",
                         "DARCS_ALTERNATIVE_COLOR", "DARCS_DO_COLOR_LINES"],[
  "If the terminal understands ANSI color escape sequences, darcs will",
  "highlight certain keywords and delimiters when printing patches. This",
  "can be turned off by setting the environment variable DARCS_DONT_COLOR",
  "to 1. If you use a pager that happens to understand ANSI colors, like",
  "`less -R`, darcs can be forced always to highlight the output by setting",
  "DARCS_ALWAYS_COLOR to 1. If you can't see colors you can set",
  "DARCS_ALTERNATIVE_COLOR to 1, and darcs will use ANSI codes for bold",
  "and reverse video instead of colors. In addition, there is an",
  "extra-colorful mode, which is not enabled by default, which can be",
  "activated with DARCS_DO_COLOR_LINES"])

environmentHelpEscapeWhite :: ([String], [String])
environmentHelpEscapeWhite = ([ "DARCS_DONT_ESCAPE_TRAILING_SPACES",
                                "DARCS_DONT_ESCAPE_TRAILING_CR"],[
  "By default darcs will escape (by highlighting if possible) any kind",
  "of spaces at the end of lines when showing patch contents.",
  "If you don't want this you can turn it off by setting",
  "DARCS_DONT_ESCAPE_TRAILING_SPACES to 1. A special case exists",
  "for only carriage returns: DARCS_DONT_ESCAPE_TRAILING_CR"])

environmentHelpEscape :: ([String], [String])
environmentHelpEscape = (["DARCS_DONT_ESCAPE_ANYTHING",
                          "DARCS_DONT_ESCAPE_ISPRINT",
                          "DARCS_DONT_ESCAPE_8BIT",
                          "DARCS_DONT_ESCAPE_EXTRA",
                          "DARCS_ESCAPE_EXTRA"],[
  "Darcs needs to escape certain characters when printing patch contents to",
  "a terminal. Characters like backspace can otherwise hide patch content",
  "from the user, and other character sequences can even in some cases",
  "redirect commands to the shell if the terminal allows it.",
  "",
  "By default darcs will only allow printable 7-bit ASCII",
  "characters (including space), and the two control characters tab and",
  "newline. All other octets are printed in quoted form (as `^<control letter>`",
  "or `\\<hex code>`).",
  "",
  "Darcs has some limited support for locales. If the system's locale is a ",
  "single-byte character encoding, like the Latin encodings, you can set the",
  "environment variable DARCS_DONT_ESCAPE_ISPRINT to 1 and darcs will display",
  "all the printables in the current system locale instead of just the ASCII",
  "ones. NOTE: This curently does not work on some architectures if darcs",
  "is compiled with GHC 6.4 or later. Some non-ASCII control characters might",
  "be printed and can possibly spoof the terminal.",
  "",
  "For multi-byte character encodings things are less smooth. UTF-8 will",
  "work if you set DARCS_DONT_ESCAPE_8BIT to 1, but non-printables outside",
  "the 7-bit ASCII range are no longer escaped. E.g., the extra control",
  "characters from Latin-1 might leave your terminal at the mercy of the",
  "patch contents. Space characters outside the 7-bit ASCII range are no",
  "longer recognized and will not be properly escaped at line endings.",
  "",
  "As a last resort you can set DARCS_DONT_ESCAPE_ANYTHING to 1. Then",
  "everything that doesn't flip code sets should work, and so will all the",
  "bells and whistles in your terminal. This environment variable can also",
  "be handy if you pipe the output to a pager or external filter that knows",
  "better than darcs how to handle your encoding. Note that all escaping,",
  "including the special escaping of any line ending spaces, will be turned",
  "off by this setting.",
  "",
  "There are two environment variables you can set to explicitly tell darcs",
  "to not escape or escape octets. They are DARCS_DONT_ESCAPE_EXTRA and",
  "DARCS_ESCAPE_EXTRA. Their values should be strings consisting of the",
  "verbatim octets in question. The do-escapes take precedence over the",
  "dont-escapes. Space characters are still escaped at line endings though.",
  "The special environment variable DARCS_DONT_ESCAPE_TRAILING_CR turns off",
  "escaping of carriage return last on the line (DOS style)."])
