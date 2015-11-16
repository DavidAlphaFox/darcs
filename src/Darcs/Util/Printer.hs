-- | A 'Document' is at heart 'ShowS' from the prelude
--
-- Essentially, if you give a Doc a string it'll print out whatever it
-- wants followed by that string. So @text "foo"@ makes the Doc that
-- prints @"foo"@ followed by its argument. The combinator names are taken
-- from 'Text.PrettyPrint.HughesPJ', although the behaviour of the two libraries is
-- slightly different.
--
-- The advantage of Printer over simple string appending/concatenating is
-- that the appends end up associating to the right, e.g.:
--
-- >   (text "foo" <> text "bar") <> (text "baz" <> text "quux") ""
-- > = \s -> (text "foo" <> text "bar") ((text "baz" <> text "quux") s) ""
-- > = (text "foo" <> text "bar") ((text "baz" <> text "quux") "")
-- > = (\s -> (text "foo") (text "bar" s)) ((text "baz" <> text "quux") "")
-- > = text "foo" (text "bar" ((text "baz" <> text "quux") ""))
-- > = (\s -> "foo" ++ s) (text "bar" ((text "baz" <> text "quux") ""))
-- > = "foo" ++ (text "bar" ((text "baz" <> text "quux") ""))
-- > = "foo" ++ ("bar" ++ ((text "baz" <> text "quux") ""))
-- > = "foo" ++ ("bar" ++ ((\s -> text "baz" (text "quux" s)) ""))
-- > = "foo" ++ ("bar" ++ (text "baz" (text "quux" "")))
-- > = "foo" ++ ("bar" ++ ("baz" ++ (text "quux" "")))
-- > = "foo" ++ ("bar" ++ ("baz" ++ ("quux" ++ "")))
--
-- The Empty alternative comes in because you want
--
-- > text "a" $$ vcat xs $$ text "b"
--
-- '$$' means above, 'vcat' is the list version of '$$'
-- (to be @\"a\\nb\"@ when @xs@  is @[]@), but without the concept of an
-- Empty Document each @$$@ would add a @'\n'@ and you'd end up with
-- @\"a\\n\\nb\"@.
-- Note that @Empty \/= text \"\"@ (the latter would cause two
-- @'\\n'@).
--
-- This code was made generic in the element type by Juliusz Chroboczek.
module Darcs.Util.Printer
    (
      Printable(..), Doc(Doc,unDoc), Printers, Printers'(..), Printer, Color(..)
    , RenderMode(..)
    , hPutDoc,     hPutDocLn,     putDoc,     putDocLn
    , hPutDocWith, hPutDocLnWith, putDocWith, putDocLnWith
    , hPutDocCompr
    , debugDocLn
    , renderString, renderStringWith, renderPS, renderPSWith
    , renderPSs, renderPSsWith, lineColor
    , prefix, insertBeforeLastline, colorText, invisibleText
    , prefixLines
    , hiddenText, hiddenPrefix, userchunk, text
    , printable, wrapText
    , blueText, redText, greenText, magentaText, cyanText
    , unsafeText, unsafeBoth, unsafeBothText, unsafeChar
    , invisiblePS, packedString, unsafePackedString, userchunkPS
    , simplePrinters, invisiblePrinter, simplePrinter
    , doc, empty, (<>), (<?>), (<+>), ($$), vcat, vsep, hcat
    , minus, newline, plus, space, backslash, lparen, rparen
    , parens
    , errorDoc
    ) where

import Control.Exception ( throwIO, ErrorCall(..) )
import Data.String ( IsString(..) )
import Data.List (intersperse)
import GHC.Stack ( currentCallStack )
import System.IO (Handle, stdout, hPutStr)
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B (ByteString, hPut, concat)
import qualified Data.ByteString.Char8 as BC (unpack, pack, singleton)

import Darcs.Util.ByteString ( linesPS, encodeLocale, gzWriteHandle )
import Darcs.Util.Global ( debugMessage )

-- | A 'Printable' is either a String, a packed string, or a chunk of
-- text with both representations.
data Printable = S !String
               | PS !B.ByteString
               | Both !String !B.ByteString

-- | 'spaceP' is the 'Printable' representation of a space.
spaceP :: Printable
spaceP   = Both " "  (BC.singleton ' ')

-- | 'newlineP' is the 'Printable' representation of a newline.
newlineP :: Printable
newlineP = S "\n"

-- | Minimal 'Doc's representing the common characters 'space', 'newline'
-- 'minus', 'plus', and 'backslash'.
space, newline, plus, minus, backslash :: Doc
space     = unsafeBoth " "  (BC.singleton ' ')
newline   = unsafeChar '\n'
minus     = unsafeBoth "-"  (BC.singleton '-')
plus      = unsafeBoth "+"  (BC.singleton '+')
backslash = unsafeBoth "\\" (BC.singleton '\\')

-- | 'lparen' is the 'Doc' that represents @\"(\"@
lparen :: Doc
lparen = unsafeBoth  "(" (BC.singleton '(')

-- | 'rparen' is the 'Doc' that represents @\")\"@
rparen :: Doc
rparen = unsafeBoth ")" (BC.singleton ')')

-- | @'parens' doc@ returns a 'Doc' with the content of @doc@ put within
-- a pair of parenthesis.
parens :: Doc -> Doc
parens d = lparen <> d <> rparen

errorDoc :: Doc -> a
errorDoc x = unsafePerformIO $ do
   stack <- currentCallStack
   throwIO $ ErrorCall $ renderString Encode $ x $$ vcat (map text stack)

-- | 'putDocWith' puts a doc on stdout using the given printer.
putDocWith :: Printers -> Doc -> IO ()
putDocWith prs = hPutDocWith prs Encode stdout

-- | 'putDocLnWith' puts a doc, followed by a newline on stdout using
-- the given printer.
putDocLnWith :: Printers -> Doc -> IO ()
putDocLnWith prs = hPutDocLnWith prs Encode stdout

-- | 'putDoc' puts a doc on stdout using the simple printer 'simplePrinters'.
putDoc :: Doc -> IO ()
putDoc = hPutDoc Encode stdout

-- | 'putDocLn' puts a doc, followed by a newline on stdout using
-- 'simplePrinters'
putDocLn :: Doc -> IO ()
putDocLn = hPutDocLn Encode stdout

-- | 'hputDocWith' puts a doc on the given handle using the given printer.
hPutDocWith :: Printers -> RenderMode -> Handle -> Doc -> IO ()
hPutDocWith prs target h d = hPrintPrintables target h (renderWith (prs h) d)

-- | 'hputDocLnWith' puts a doc, followed by a newline on the given
-- handle using the given printer.
hPutDocLnWith :: Printers -> RenderMode -> Handle -> Doc -> IO ()
hPutDocLnWith prs target h d = hPutDocWith prs target h (d <?> newline)

-- |'hputDoc' puts a doc on the given handle using 'simplePrinters'
hPutDoc :: RenderMode -> Handle -> Doc -> IO ()
hPutDoc = hPutDocWith simplePrinters

-- 'hputDocLn' puts a doc, followed by a newline on the given handle using
-- 'simplePrinters'.
hPutDocLn :: RenderMode -> Handle -> Doc -> IO ()
hPutDocLn = hPutDocLnWith simplePrinters

-- | like 'hPutDoc' but with compress data before writing
hPutDocCompr :: RenderMode -> Handle -> Doc -> IO ()
hPutDocCompr target h = gzWriteHandle h . renderPSs target

-- | Write a 'Doc' to stderr if debugging is turned on.
debugDocLn :: Doc -> IO ()
debugDocLn = debugMessage . renderString Standard

-- | @'hPrintPrintables' h@ prints a list of 'Printable's to the handle h
hPrintPrintables :: RenderMode -> Handle -> [Printable] -> IO ()
hPrintPrintables target h = mapM_ (hPrintPrintable target h)

-- | @hPrintPrintable h@ prints a 'Printable' to the handle h.
hPrintPrintable :: RenderMode -> Handle -> Printable -> IO ()
hPrintPrintable Standard h (S ps) = hPutStr h ps
hPrintPrintable Encode  h (S ps) = B.hPut h (encodeLocale ps)
hPrintPrintable Standard h (PS ps) = B.hPut h ps
hPrintPrintable Encode  h (PS ps) = B.hPut h ps
hPrintPrintable Standard h (Both _ ps) = B.hPut h ps
hPrintPrintable Encode  h (Both _ ps) = B.hPut h ps

-- | a 'Doc' is a bit of enriched text. 'Doc's get concatanated using
-- '<>', which is right-associative.
newtype Doc = Doc { unDoc :: St -> Document }

instance IsString Doc where
   fromString = text

-- TODO this is a rather ad-hoc hack that further complicates
-- some already confusing code. We should find a more general
-- solution. See the discussion on issue1639.
-- | Used when rendering a 'Doc' to indicate if the result
-- should be encoded to the current locale or left alone.
-- In practice this only affects output when a relevant
-- DARCS_DONT_ESCAPE_XXX option is set (see Darcs.Util.Printer.Color)
-- If in doubt, choose 'Standard'.
data RenderMode =
      Encode       -- ^Encode Strings with the current locale.
                   -- At present ByteStrings are assumed to be in
                   -- UTF8 and are left alone, so will be mis-encoded
                   -- in non-UTF8 locales.
    | Standard     -- ^Don't encode.

-- | The State associated with a doc. Contains a set of printers for each
-- hanlde, and the current prefix of the document.
data St = St { printers :: !Printers',
               currentPrefix :: !([Printable] -> [Printable]) }
type Printers = Handle -> Printers'

-- | A set of printers to print different types of text to a handle.
data Printers' = Printers {colorP :: !(Color -> Printer),
                           invisibleP :: !Printer,
                           hiddenP :: !Printer,
                           userchunkP :: !Printer,
                           defP :: !Printer,
                           lineColorT :: !(Color -> Doc -> Doc),
                           lineColorS :: !([Printable] -> [Printable])
                          }
type Printer = Printable -> St -> Document

data Color = Blue | Red | Green | Cyan | Magenta

-- | 'Document' is a wrapper around '[Printable] -> [Printable]' which allows
-- for empty Documents. The simplest 'Documents' are built from 'String's
-- using 'text'.
data Document = Document ([Printable] -> [Printable])
              | Empty

-- | renders a 'Doc' into a 'String' with control codes for the
-- special features of the doc.
renderString :: RenderMode -> Doc -> String
renderString = renderStringWith simplePrinters'

-- | renders a 'Doc' into a 'String' using a given set of printers.
renderStringWith :: Printers' -> RenderMode -> Doc -> String
renderStringWith prs target d = concatMap (toString target) $ renderWith prs d
    where toString Standard (S s) = s
          toString Encode  (S s) = BC.unpack . encodeLocale $ s
          toString Standard (PS ps) = BC.unpack ps
          toString Encode  (PS ps) = BC.unpack ps
          toString Standard (Both s _) = s
          toString Encode  (Both s _) = BC.unpack . encodeLocale $ s

-- | renders a 'Doc' into 'B.ByteString' with control codes for the
-- special features of the Doc. See also 'readerString'.
renderPS :: RenderMode -> Doc -> B.ByteString
renderPS = renderPSWith simplePrinters'

-- | renders a 'Doc' into a list of 'PackedStrings', one for each line.
renderPSs :: RenderMode -> Doc -> [B.ByteString]
renderPSs = renderPSsWith simplePrinters'

-- | renders a doc into a 'B.ByteString' using a given set of printers.
renderPSWith :: Printers' -> RenderMode -> Doc -> B.ByteString
renderPSWith prs target d = B.concat $ renderPSsWith prs target d

-- | renders a 'Doc' into a list of 'PackedStrings', one for each
-- chunk of text that was added to the doc, using the given set of
-- printers.
renderPSsWith :: Printers' -> RenderMode -> Doc -> [B.ByteString]
renderPSsWith prs target d = map (toPS target) $ renderWith prs d
    where toPS Standard (S s)        = BC.pack s
          toPS Encode  (S s)        = encodeLocale s
          toPS Standard (PS ps)      = ps
          toPS Encode  (PS ps)      = ps
          toPS Standard (Both _ ps)  = ps
          toPS Encode  (Both _ ps)  = ps

-- | renders a 'Doc' into a list of 'Printables' using a set of
-- printers. Each item of the list corresponds to a string that was
-- added to the doc.
renderWith :: Printers' -> Doc -> [Printable]
renderWith ps (Doc d) = case d (initState ps) of
                        Empty -> []
                        Document f -> f []

initState :: Printers' -> St
initState prs = St { printers = prs, currentPrefix = id }

prefix :: String -> Doc -> Doc
prefix s (Doc d) = Doc $ \st ->
                   let p = S s
                       st' = st { currentPrefix = currentPrefix st . (p:) } in
                   case d st' of
                     Document d'' -> Document $ (p:) . d''
                     Empty -> Empty

-- TODO try to find another way to do this, it's rather a violation
-- of the Doc abstraction
prefixLines :: Doc -> Doc -> Doc
prefixLines prefixer prefixee =
   vcat $ map (prefixer <+>) $ map packedString $ linesPS $
   -- this will just get round-tripped back into a Doc,
   renderPS Standard prefixee

-- TODO try to find another way to do this, it's rather a violation
-- of the Doc abstraction
insertBeforeLastline :: Doc -> Doc -> Doc
insertBeforeLastline a b =
   -- as this will just get round-tripped back into a Doc,
   -- we use 'Standard' as the Target type so the encoding
   -- is left alone
   case reverse $ map packedString $ linesPS $ renderPS Standard a of
   (ll:ls) -> vcat (reverse ls) $$ b $$ ll
   [] -> error "empty Doc given as first argument of Printer.insert_before_last_line"


lineColor :: Color -> Doc -> Doc
lineColor c d = Doc $ \st -> case lineColorT (printers st) c d of
                             Doc d' -> d' st

hiddenPrefix :: String -> Doc -> Doc
hiddenPrefix s (Doc d) =
    Doc $ \st -> let pr = printers st
                     p = S (renderStringWith pr Standard $ hiddenText s)
                     st' = st { currentPrefix = currentPrefix st . (p:) }
                 in case d st' of
                      Document d'' -> Document $ (p:) . d''
                      Empty -> Empty

-- | 'unsafeBoth' builds a Doc from a 'String' and a 'B.ByteString' representing
-- the same text, but does not check that they do.
unsafeBoth :: String -> B.ByteString -> Doc
unsafeBoth s ps = Doc $ simplePrinter (Both s ps)

-- | 'unsafeBothText' builds a 'Doc' from a 'String'. The string is stored in the
-- Doc as both a String and a 'B.ByteString'.
unsafeBothText :: String -> Doc
unsafeBothText s = Doc $ simplePrinter (Both s (BC.pack s))

-- | 'packedString' builds a 'Doc' from a 'B.ByteString' using 'printable'
packedString :: B.ByteString -> Doc
packedString = printable . PS

-- | 'unsafePackedString' builds a 'Doc' from a 'B.ByteString' using 'simplePrinter'
unsafePackedString :: B.ByteString -> Doc
unsafePackedString = Doc . simplePrinter . PS

-- | 'invisiblePS' creates a 'Doc' with invisible text from a 'B.ByteString'
invisiblePS :: B.ByteString -> Doc
invisiblePS = invisiblePrintable . PS

-- | 'userchunkPS' creates a 'Doc' representing a user chunk from a 'B.ByteString'.
userchunkPS :: B.ByteString -> Doc
userchunkPS = userchunkPrintable . PS

-- | 'unsafeChar' creates a Doc containing just one character.
unsafeChar :: Char -> Doc
unsafeChar = unsafeText . (:"")

-- | 'text' creates a 'Doc' from a @String@, using 'printable'.
text :: String -> Doc
text = printable . S

-- | 'unsafeText' creates a 'Doc' from a 'String', using 'simplePrinter' directly
unsafeText :: String -> Doc
unsafeText = Doc . simplePrinter . S

-- | 'invisibleText' creates a 'Doc' containing invisible text from a @String@
invisibleText :: String -> Doc
invisibleText = invisiblePrintable . S

-- | 'hiddenText' creates a 'Doc' containing hidden text from a @String@
hiddenText :: String -> Doc
hiddenText = hiddenPrintable . S

-- | 'userchunk' creates a 'Doc' containing a user chunk from a @String@
userchunk :: String -> Doc
userchunk = userchunkPrintable . S

-- | 'blueText' creates a 'Doc' containing blue text from a @String@
blueText, redText, greenText, magentaText, cyanText :: String -> Doc
blueText = colorText Blue
redText = colorText Red
greenText = colorText Green
magentaText = colorText Magenta
cyanText = colorText Cyan

-- | 'colorText' creates a 'Doc' containing colored text from a @String@
colorText :: Color -> String -> Doc
colorText c = mkColorPrintable c . S

-- | @'wrapText' n s@ is a 'Doc' representing @s@ line-wrapped at 'n' characters
wrapText :: Int -> String -> Doc
wrapText n s =
    vcat . map text . reverse $ "" : foldl add_to_line [] (words s)
  where add_to_line [] a = [a]
        add_to_line ("":d) a = a:d
        add_to_line (l:ls) new | length l + length new > n = new:l:ls
        add_to_line (l:ls) new = (l ++ " " ++ new):ls

-- | 'printable x' creates a 'Doc' from any 'Printable'.
printable, invisiblePrintable, hiddenPrintable, userchunkPrintable :: Printable -> Doc
printable x = Doc $ \st -> defP (printers st) x st

mkColorPrintable :: Color -> Printable -> Doc
mkColorPrintable c x = Doc $ \st -> colorP (printers st) c x st
invisiblePrintable x = Doc $ \st -> invisibleP (printers st) x st
hiddenPrintable x = Doc $ \st -> hiddenP (printers st) x st
userchunkPrintable x = Doc $ \st -> userchunkP (printers st) x st

-- | 'simplePrinters' is a 'Printers' which uses the set 'simplePriners\'' on any
-- handle.
simplePrinters :: Printers
simplePrinters _ = simplePrinters'

-- | A set of default printers suitable for any handle. Does not use color.
simplePrinters' :: Printers'
simplePrinters'  = Printers { colorP = const simplePrinter,
                              invisibleP = simplePrinter,
                              hiddenP = invisiblePrinter,
                              userchunkP = simplePrinter,
                              defP = simplePrinter,
                              lineColorT = const id,
                              lineColorS = id
                            }

-- | 'simplePrinter' is the simplest 'Printer': it just concatenates together
-- the pieces of the 'Doc'
simplePrinter :: Printer
simplePrinter x = unDoc $ doc (\s -> x:s)

-- | 'invisiblePrinter' is the 'Printer' for hidden text. It just replaces
-- the document with 'empty'.  It's useful to have a printer that doesn't
-- actually do anything because this allows you to have tunable policies,
-- for example, only printing some text if it's to the terminal, but not
-- if it's to a file or vice-versa.
invisiblePrinter :: Printer
invisiblePrinter _ = unDoc empty

infixr 6 <>
infixr 6 <+>
infixr 5 $$

-- | The empty 'Doc'.
empty :: Doc
empty = Doc $ const Empty
doc :: ([Printable] -> [Printable]) -> Doc
doc f = Doc $ const $ Document f

-- | '(<>)' is the concatenation operator for 'Doc's
(<>) :: Doc -> Doc -> Doc
-- | @a '<?>' b@ is @a <> b@ if @a@ is not empty, else empty.
(<?>) :: Doc -> Doc -> Doc
-- | @a '<+>' b@ is @a@ followed by a space, then @b@.
(<+>) :: Doc -> Doc -> Doc
-- | @a '$$' b@ is @a@ above @b@.
($$) :: Doc -> Doc -> Doc
-- a then b
Doc a <> Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> bf s)

-- empty if a empty, else a then b
Doc a <?> Doc b =
    Doc $ \st -> case a st of
                 Empty -> Empty
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf -> bf s)

-- a then space then b
Doc a <+> Doc b =
    Doc $ \st -> case a st of
                 Empty -> b st
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf ->
                                                         spaceP:bf s)

-- a above b
Doc a $$ Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> sf (newlineP:pf (bf s)))
                        where pf = currentPrefix st
                              sf = lineColorS $ printers st

-- | 'vcat' piles vertically a list of 'Doc's.
vcat :: [Doc] -> Doc
vcat [] = empty
vcat ds = foldr1 ($$) ds

-- | 'vsep' piles vertically a list of 'Doc's leaving a blank line between each.
vsep :: [Doc] -> Doc
vsep [] = empty
vsep ds = foldr1 ($$) $ intersperse (text "") ds

-- | 'hcat' concatenates (horizontally) a list of 'Doc's
hcat :: [Doc] -> Doc
hcat [] = empty
hcat ds = foldr1 (<>) ds
