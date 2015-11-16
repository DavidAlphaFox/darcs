-- Copyright (C) 2002-2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Patch.Info ( PatchInfo(..), patchinfo, invertName,
                          rawPatchInfo,
                          addJunk, makePatchname,
                          makeFilename, readPatchInfo,
                          justName, justAuthor, justLog,
                          showPatchInfoUI, toXml, piDate,
                          setPiDate, piDateString, piDateBytestring,
                          piName, piRename, piAuthor, piTag, piLog,
                          showPatchInfo, isTag, readPatchInfos, escapeXML
                        ) where
import System.Random ( randomRIO )
import Numeric ( showHex )
import Control.Monad ( when, unless, void )

import Darcs.Util.ByteString ( unlinesPS, packStringToUTF8, unpackPSFromUTF8, decodeLocale)
import qualified Darcs.Patch.ReadMonads as RM ( take )
import Darcs.Patch.ReadMonads as RM ( skipSpace, char,
                                      takeTill, anyChar, ParserM,
                                      option, parseStrictly,
                                      takeTillChar,
                                      linesStartingWithEndingWith)
import qualified Data.ByteString       as B  (length, splitAt, null
                                             ,isPrefixOf, tail, concat
                                             ,empty, head, cons, append
                                             ,ByteString )
import qualified Data.ByteString.Char8 as BC (index, head, unpack, pack)
import Data.List( isPrefixOf )

import Darcs.Util.Printer ( Doc, packedString,
                 empty, ($$), (<>), (<+>), vcat, text, cyanText, blueText, prefix )
import Darcs.Patch.OldDate ( readUTCDate, showIsoDateTime )
import System.Time ( CalendarTime(ctTZ), calendarTimeToString, toClockTime,
                     toCalendarTime )
import System.IO.Unsafe ( unsafePerformIO )
import Darcs.Util.Crypt.SHA1 ( sha1PS, SHA1 )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Show ( appPrec )
import Prelude hiding (pi, log)

-- | A PatchInfo value contains the metadata of a patch. The date, name, author
-- and log fields are UTF-8 encoded text in darcs 2.4 and later, and just
-- sequences of bytes (decoded with whatever is the locale when displayed) in
-- earlier darcs.
--
-- The members with names that start with '_' are not supposed to be used
-- directly in code that does not care how the patch info is stored.
data PatchInfo = PatchInfo { _piDate    :: !B.ByteString
                           , _piName    :: !B.ByteString
                           , _piAuthor  :: !B.ByteString
                           , _piLog     :: ![B.ByteString]
                           , isInverted :: !Bool
                           }
                 deriving (Eq,Ord)

instance Show PatchInfo where
    showsPrec d (PatchInfo date name author log inverted) =
        showParen (d > appPrec) $
            showString "rawPatchInfo " . showsPrec (appPrec + 1) date .
            showString " " . showsPrec (appPrec + 1) name .
            showString " " . showsPrec (appPrec + 1) author .
            showString " " . showsPrec (appPrec + 1) log .
            showString " " . showsPrec (appPrec + 1) inverted

rawPatchInfo :: String -> String -> String -> [String] -> Bool -> PatchInfo
rawPatchInfo date name author log inverted =
    PatchInfo { _piDate     = BC.pack date
              , _piName     = packStringToUTF8 name
              , _piAuthor   = packStringToUTF8 author
              , _piLog      = map packStringToUTF8 log
              , isInverted  = inverted }

-- | @patchinfo date name author log@ constructs a new 'PatchInfo' value
-- with the given details, automatically assigning an Ignore-this header
-- to guarantee the patch is unique.  The function does not verify
-- the date string's sanity.
patchinfo :: String -> String -> String -> [String] -> IO PatchInfo
patchinfo date name author log =
    addJunk $ rawPatchInfo date name author log False

-- | addJunk adds a line that contains a random number to make the patch
--   unique.
addJunk :: PatchInfo -> IO PatchInfo
addJunk pinf =
    do x <- randomRIO (0,2^(128 ::Integer) :: Integer)
       when (_piLog pinf /= ignoreJunk (_piLog pinf)) $
            do putStrLn "Lines beginning with 'Ignore-this: ' will be ignored."
               confirmed <- promptYorn "Proceed? "
               unless confirmed $ fail "User cancelled because of Ignore-this."
       return $ pinf { _piLog = BC.pack (head ignored++showHex x ""):
                                 _piLog pinf }

ignored :: [String] -- this is a [String] so we can change the junk header.
ignored = ["Ignore-this: "]

ignoreJunk :: [B.ByteString] -> [B.ByteString]
ignoreJunk = filter isnt_ignored
    where isnt_ignored x = doesnt_start_with x (map BC.pack ignored) -- TODO
          doesnt_start_with x ys = not $ any (`B.isPrefixOf` x) ys


-- * Patch info formatting
invertName :: PatchInfo -> PatchInfo
invertName pi = pi { isInverted = not (isInverted pi) }

-- | Get the name, including an "UNDO: " prefix if the patch is inverted.
justName :: PatchInfo -> String
justName pinf = if isInverted pinf then "UNDO: " ++ nameString
                                     else nameString
  where nameString = metadataToString (_piName pinf)

-- | Returns the author of a patch.
justAuthor :: PatchInfo -> String
justAuthor =  metadataToString . _piAuthor

justLog :: PatchInfo -> String
justLog = unlines . map BC.unpack . _piLog

showPatchInfoUI :: PatchInfo -> Doc
showPatchInfoUI pi =
    cyanText "patch " <> cyanText (show $ makePatchname pi)
 $$ text "Author: " <> text (piAuthor pi)
 $$ text "Date:   " <> text (friendlyD $ _piDate pi)
 $$ hfn (piName pi)
 $$ vcat (map ((text "  " <>) . text) (piLog pi))
  where hfn x = case piTag pi of
                Nothing -> inverted <+> text x
                Just t -> text "  tagged" <+> text t
        inverted = if isInverted pi then text "  UNDO:" else text "  *"

-- | Returns the name of the patch. Unlike 'justName', it does not preprend
--   "UNDO: " to the name if the patch is inverted.
piName :: PatchInfo -> String
piName = metadataToString . _piName

piRename :: PatchInfo -> String -> PatchInfo
piRename x n = x { _piName = packStringToUTF8 n }

-- | Returns the author of a patch.
piAuthor :: PatchInfo -> String
piAuthor = metadataToString . _piAuthor

isTag :: PatchInfo -> Bool
isTag pinfo = "TAG " `isPrefixOf` justName pinfo

-- | Note: we ignore timezone information in the date string,
--   systematically treating a time as UTC.  So if the patch
--   tells me it's 17:00 EST, we're actually treating it as
--   17:00 UTC, in other words 11:00 EST.  This is for
--   backwards compatibility to darcs prior to 2003-11, sometime
--   before 1.0.  Fortunately, newer patch dates are written in
--   UTC, so this timezone truncation is harmless for them.
readPatchDate :: B.ByteString -> CalendarTime
readPatchDate = ignoreTz . readUTCDate . BC.unpack
  where ignoreTz ct = ct { ctTZ = 0 }

piDate :: PatchInfo -> CalendarTime
piDate = readPatchDate . _piDate

piDateString :: PatchInfo -> String
piDateString = BC.unpack . _piDate

piDateBytestring :: PatchInfo -> B.ByteString
piDateBytestring = _piDate

setPiDate :: String -> PatchInfo -> PatchInfo
setPiDate date pi = pi { _piDate = BC.pack date }

-- | Get the log message of a patch.
piLog :: PatchInfo -> [String]
piLog = map metadataToString . ignoreJunk . _piLog

-- | Get the tag name, if the patch is a tag patch.
piTag :: PatchInfo -> Maybe String
piTag pinf =
    if l == t
      then Just $ metadataToString r
      else Nothing
    where (l, r) = B.splitAt (B.length t) (_piName pinf)
          t = BC.pack "TAG "

-- | Convert a metadata ByteString to a string. It first tries to convert
--   using UTF-8, and if that fails, tries the locale encoding.
--   We try UTF-8 first because UTF-8 is clearly recognizable, widely used,
--   and people may have UTF-8 patches even when UTF-8 is not their locale.
metadataToString :: B.ByteString -> String
metadataToString bs | '\xfffd' `notElem` bsUtf8 = bsUtf8
                    | otherwise                 = decodeLocale bs
  where bsUtf8 = unpackPSFromUTF8 bs

friendlyD :: B.ByteString -> String
--friendlyD d = calendarTimeToString . readPatchDate . d
friendlyD d = unsafePerformIO $ do
    ct <- toCalendarTime $ toClockTime $ readPatchDate d
    return $ calendarTimeToString ct

toXml :: PatchInfo -> Doc
toXml pi =
        text "<patch"
    <+> text "author='" <> escapeXMLByteString (_piAuthor pi) <> text "'"
    <+> text "date='" <> escapeXMLByteString (_piDate pi) <> text "'"
    <+> text "local_date='" <> escapeXML (friendlyD $ _piDate pi) <> text "'"
    <+> text "inverted='" <> text (show $ isInverted pi) <> text "'"
    <+> text "hash='" <> text (show $ makePatchname pi) <> text "'>"
 $$     prefix "\t" (
            text "<name>" <> escapeXMLByteString (_piName pi) <> text "</name>"
         $$ commentsAsXml (_piLog pi))
 $$     text "</patch>"

commentsAsXml :: [B.ByteString] -> Doc
commentsAsXml comments
  | B.length comments' > 0 = text "<comment>"
                          <> escapeXMLByteString comments'
                          <> text "</comment>"
  | otherwise = empty
    where comments' = unlinesPS comments

-- escapeXML is duplicated in Patch.lhs and Annotate.lhs
-- It should probably be refactored to exist in one place.
escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

-- Escape XML characters in a UTF-8 encoded ByteString, and turn it into a Doc.
-- The data will be in the Doc as a bytestring.
escapeXMLByteString :: B.ByteString -> Doc
escapeXMLByteString = packedString . bstrReplace '\'' "&apos;"
                                   . bstrReplace '"'  "&quot;"
                                   . bstrReplace '>'  "&gt;"
                                   . bstrReplace '<'  "&lt;"
                                   . bstrReplace '&'  "&amp;"

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ strReplace x y zs
  | otherwise = z : strReplace x y zs

bstrReplace :: Char -> String -> B.ByteString -> B.ByteString
bstrReplace c s bs | B.null bs   = B.empty
                   | otherwise   = if BC.head bs == c
                                     then B.append (BC.pack s)
                                                   (bstrReplace c s (B.tail bs))
                                     else B.cons (B.head bs)
                                                 (bstrReplace c s (B.tail bs))

-- | This makes darcs-1 (non-hashed repos) filenames, and is also generally
-- used in both in hashed and non-hashed repo code for making patch "hashes".
--
-- The name consists of three segments:
--
--  * timestamp (ISO8601-compatible yyyymmmddHHMMSS, UTC)
--
--  * SHA1 hash of the author
--
--  * SHA1 hash of the patch name, author, date, log, and \"inverted\"
--    flag.
makeFilename :: PatchInfo -> String
makeFilename pi = showIsoDateTime d++"-"++sha1_a++"-"++ (show $ makePatchname pi) ++ ".gz"
    where d = readPatchDate $ _piDate pi
          sha1_a = take 5 $ show $ sha1PS $ _piAuthor pi

-- | Hash on patch metadata (patch name, author, date, log, and \"inverted\"
-- flag. Robust against context changes but does not garantee patch contents.
-- Usually used as matcher or patch identifier (see Darcs.Patch.Match).
makePatchname :: PatchInfo -> SHA1
makePatchname pi = sha1PS sha1_me
        where b2ps True = BC.pack "t"
              b2ps False = BC.pack "f"
              sha1_me = B.concat [_piName pi,
                                  _piAuthor pi,
                                  _piDate pi,
                                  B.concat $ _piLog pi,
                                  b2ps $ isInverted pi]


-- |Patch is stored between square brackets.
--
-- > [ <patch name>
-- > <patch author>*<patch date>
-- >  <patch log (may be empty)> (indented one)
-- >  <can have multiple lines in patch log,>
-- >  <as long as they're preceded by a space>
-- >  <and don't end with a square bracket.>
-- > ]
--
-- note that below I assume the name has no newline in it.
-- See 'readPatchInfo' for the inverse operation.
showPatchInfo :: PatchInfo -> Doc
showPatchInfo pi =
    blueText "[" <> packedString (_piName pi)
 $$ packedString (_piAuthor pi) <> text inverted <> packedString (_piDate pi)
                                 <> myunlines (_piLog pi) <> blueText "] "
    where inverted = if isInverted pi then "*-" else "**"
          myunlines [] = empty
          myunlines xs =
              foldr (\s -> ((text "\n " <> packedString s) <>)) (text "\n") xs

-- |Parser for 'PatchInfo' as stored in patch bundles and inventory files,
-- for example:
--
-- > [Document the foo interface
-- > John Doe <john.doe@example.com>**20110615084241
-- >  Ignore-this: 85b94f67d377c4ab671101266ef9c229
-- >  Nobody knows what a 'foo' is, so describe it.
-- > ]
--
-- See 'showPatchInfo' for the inverse operation.
readPatchInfo :: ParserM m => m PatchInfo
readPatchInfo = do
  skipSpace
  char '['
  name <- takeTillChar '\n'
  _ <- anyChar
  author <- takeTillChar '*'
  s2 <- RM.take 2
  ct <- takeTill (\c->c==']'||c=='\n')
  option () (void (char '\n')) -- consume newline char, if present
  log <- linesStartingWithEndingWith ' ' ']'
  return PatchInfo { _piDate = ct
                   , _piName = name
                   , _piAuthor = author
                   , _piLog = log
                   , isInverted = BC.index s2 1 /= '*'
                   }

readPatchInfos :: B.ByteString -> [PatchInfo]
readPatchInfos inv | B.null inv = []
readPatchInfos inv = case parseStrictly readPatchInfo inv of
                     Just (pinfo,r) -> pinfo : readPatchInfos r
                     _ -> []

