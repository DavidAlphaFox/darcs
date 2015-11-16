--  Copyright (C) 2007 Kevin Quick
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

{-# LANGUAGE CPP #-}
module Darcs.UI.Commands.ShowRepo ( showRepo ) where

import Prelude hiding ( (^) )
import Data.Char ( toLower, isSpace )
import Data.List ( intercalate )
import Control.Monad ( when, unless, liftM )
import Text.Html ( tag, stringToHtml )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.Flags ( DarcsFlag(XMLOutput, Verbose, NoFiles), useCache )
import Darcs.UI.Options ( DarcsOption, (^), oid, odesc, ocheck, onormalise, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Darcs.Repository ( withRepository, RepoJob(..), readRepo )
import Darcs.Repository.Internal ( Repository(..) )
import Darcs.Repository.InternalTypes ( Pristine(..) )
import Darcs.Repository.Cache ( Cache(..) )
import Darcs.Repository.Format ( RepoFormat(..) )
import Darcs.Repository.Prefs ( getPreflist )
import Darcs.Repository.Motd ( getMotd )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch.Witnesses.Ordered ( lengthRL )
import qualified Data.ByteString.Char8 as BC  (unpack)
import Darcs.Patch.Apply( ApplyState )
import Storage.Hashed.Tree ( Tree )

showRepoHelp :: String
showRepoHelp =
 "The `darcs show repo` command displays statistics about the current\n" ++
 "repository, allowing third-party scripts to access this information\n" ++
 "without inspecting `_darcs` directly (and without breaking when the\n" ++
 "`_darcs` format changes).\n" ++
 "\n" ++
 "By default, the number of patches is shown.  If this data isn't\n" ++
 "needed, use `--no-files` to accelerate this command from O(n) to O(1).\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The `--xml-output`\n" ++
 "option can be used to generate output for machine postprocessing.\n"

showRepoDescription :: String
showRepoDescription = "Show repository summary information"

showRepoBasicOpts :: DarcsOption a (Maybe String -> Bool -> O.XmlOutput -> a)
showRepoBasicOpts = O.workingRepoDir ^ O.files ^ O.xmloutput

showRepoOpts :: DarcsOption a
                (Maybe String
                 -> Bool
                 -> O.XmlOutput
                 -> Maybe O.StdCmdAction
                 -> Bool
                 -> Bool
                 -> O.Verbosity
                 -> Bool
                 -> O.UseCache
                 -> Maybe String
                 -> Bool
                 -> Maybe String
                 -> Bool
                 -> a)
showRepoOpts = showRepoBasicOpts `withStdOpts` oid

showRepo :: DarcsCommand [DarcsFlag]
showRepo = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "repo"
    , commandHelp = showRepoHelp
    , commandDescription = showRepoDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = repoCmd
    , commandPrereq = amInRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showRepoBasicOpts
    , commandDefaults = defaultFlags showRepoOpts
    , commandCheckOptions = ocheck showRepoOpts
    , commandParseOptions = onormalise showRepoOpts
    }

repoCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
repoCmd _ opts _ = let put_mode = if XMLOutput `elem` opts then showInfoXML else showInfoUsr
                    in withRepository (useCache opts) $ RepoJob $ \repository -> actuallyShowRepo (putInfo put_mode) repository opts

-- Some convenience functions to output a labelled text string or an
-- XML tag + value (same API).  If no value, output is suppressed
-- entirely.  Borrow some help from Text.Html to perform XML output.

type ShowInfo = String -> String -> String

showInfoXML :: ShowInfo
showInfoXML t i = show $ tag (safeTag t) $ stringToHtml i

safeTag :: String -> String
safeTag [] = []
safeTag (' ':cs) = safeTag cs
safeTag ('#':cs) = "num_" ++ safeTag cs
safeTag (c:cs) = toLower c : safeTag cs

-- labelled strings: labels are right-aligned at 14 characters;
-- subsequent lines in multi-line output are indented accordingly.
showInfoUsr :: ShowInfo
showInfoUsr t i = replicate (14 - length t) ' ' ++ t ++ ": " ++
                  intercalate ('\n' : replicate 16 ' ') (lines i) ++ "\n"

type PutInfo = String -> String -> IO ()
putInfo :: ShowInfo -> PutInfo
putInfo m t i = unless (null i) (putStr $ m t i)

-- Primary show-repo operation.  Determines ordering of output for
-- sub-displays.  The `out' argument is one of the above operations to
-- output a labelled text string or an XML tag and contained value.

actuallyShowRepo :: (RepoPatch p, ApplyState p ~ Tree)
                 => PutInfo -> Repository p wR wU wR -> [DarcsFlag] -> IO ()
actuallyShowRepo out r@(Repo loc rf pris cs) opts = do
         when (XMLOutput `elem` opts) (putStr "<repository>\n")
         when (Verbose `elem` opts) (out "Show" $ show r)
         showRepoFormat out rf
         out "Root" loc
         showRepoAux out pris cs
         showRepoPrefs out
         unless (NoFiles `elem` opts) (numPatches r >>= (out "Num Patches" . show ))
         showRepoMOTD out r
         when (XMLOutput `elem` opts) (putStr "</repository>\n")

-- Most of the actual elements being displayed are part of the Show
-- class; that's fine for a Haskeller, but not for the common user, so
-- the routines below work to provide more human-readable information
-- regarding the repository elements.

showRepoFormat :: PutInfo -> RepoFormat -> IO ()
showRepoFormat out rf = out "Format" . intercalate ", " . lines . show $ rf

showRepoAux :: PutInfo -> Pristine -> Cache -> IO ()
showRepoAux out pris cs =
    do out "Pristine" $ show pris
       out "Cache" $ intercalate ", " $ lines $ show cs


showRepoPrefs :: PutInfo -> IO ()
showRepoPrefs out = do
    getPreflist "prefs" >>= mapM_ prefOut
    getPreflist "author" >>= out "Author" . unlines
    getPreflist "defaultrepo" >>= out "Default Remote" . unlines
  where prefOut = uncurry out . (\(p,v) -> (p++" Pref", dropWhile isSpace v)) . break isSpace

showRepoMOTD :: RepoPatch p => PutInfo -> Repository p wR wU wR -> IO ()
showRepoMOTD out (Repo loc _ _ _) = getMotd loc >>= out "MOTD" . BC.unpack

-- Support routines to provide information used by the PutInfo operations above.

numPatches :: (RepoPatch p, ApplyState p ~ Tree) => Repository p wR wU wR -> IO Int
numPatches r = (lengthRL . newset2RL) `liftM` readRepo r

