--  Copyright (C) 2002-2004 David Roundy
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


module Darcs.UI.Commands.WhatsNew
    (
      whatsnew
    , status
    ) where

import Prelude hiding ( (^), catch )

import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Control.Monad.Reader ( runReaderT )
import Control.Monad.State ( evalStateT, liftIO )
import Storage.Hashed.Tree ( Tree )
import System.Exit ( ExitCode (..), exitSuccess, exitWith )

import Darcs.Patch
    ( PrimOf, PrimPatch, RepoPatch
    , applyToTree, plainSummaryPrims, primIsHunk
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Choices ( patchChoicesLps, lpPatch )
import Darcs.Patch.FileHunk ( IsHunk (..) )
import Darcs.Patch.Format ( PatchListFormat (..) )
import Darcs.Patch.Inspect ( PatchInspect (..) )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.Patch.Prim.Class ( PrimDetails (..) )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.TouchesFiles ( choosePreTouching )
import Darcs.Patch.Witnesses.Ordered
    ( (:>) (..), FL (..), RL (..)
    , lengthFL, reverseFL, reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed (..), Sealed2 (..)
    , unFreeLeft
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.WZipper ( FZipper (..) )
import Darcs.Repository
    ( RepoJob (..), Repository
    , listRegisteredFiles, readRecorded
    , unrecordedChangesWithPatches, withRepository
    )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.Util ( getMovesPs, getReplaces )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, amInRepository
    , commandAlias, nodefaults
    )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Flags
    ( DarcsFlag (Summary, LookForAdds, LookForMoves), diffAlgorithm, diffingOpts
    , isUnified, useCache, fixSubPaths
    , verbosity, isInteractive, isUnified, lookForAdds, lookForMoves, lookForReplaces, hasSummary
    )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PrintPatch
    ( contextualPrintPatch, printPatch
    , printPatchPager
    )
import Darcs.UI.SelectChanges
    ( InteractiveSelectionContext (..)
    , InteractiveSelectionM, KeyPress (..)
    , WhichChanges (..), backAll
    , backOne, currentFile
    , currentPatch, decide
    , decideWholeFile, helpFor
    , keysFor, prompt
    , selectionContextPrim, skipMundane
    , skipOne, printSummary
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Util.Path ( AbsolutePath, SubPath, toFilePath )
import Darcs.Util.Printer
    ( putDocLn, renderString, RenderMode(..)
    , text, vcat
    )
import Darcs.Util.Prompt ( PromptConfig (..), promptChar )


whatsnewBasicOpts :: DarcsOption a
                     (Maybe O.Summary
                      -> O.WithContext
                      -> O.LookFor
                      -> O.DiffAlgorithm
                      -> Maybe String
                      -> Maybe Bool
                      -> a)
whatsnewBasicOpts
    = O.summary
    ^ O.withContext
    ^ O.lookfor
    ^ O.diffAlgorithm
    ^ O.workingRepoDir
    ^ O.interactive -- False

whatsnewAdvancedOpts :: DarcsOption a (O.UseIndex -> Bool -> a)
whatsnewAdvancedOpts = O.useIndex ^ O.includeBoring

whatsnewOpts :: DarcsOption a
                (Maybe O.Summary
                 -> O.WithContext
                 -> O.LookFor
                 -> O.DiffAlgorithm
                 -> Maybe String
                 -> Maybe Bool
                 -> Maybe O.StdCmdAction
                 -> Bool
                 -> Bool
                 -> O.Verbosity
                 -> Bool
                 -> O.UseIndex
                 -> Bool
                 -> O.UseCache
                 -> Maybe String
                 -> Bool
                 -> Maybe String
                 -> Bool
                 -> a)
whatsnewOpts = whatsnewBasicOpts `withStdOpts` whatsnewAdvancedOpts

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = []
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = hasSummary (defaultSummary flags) flags
    , S.withContext = isUnified flags
    }

defaultSummary :: [DarcsFlag] -> O.Summary
defaultSummary flags = if lookForAdds flags == O.YesLookForAdds then O.YesSummary else O.NoSummary

whatsnew :: DarcsCommand [DarcsFlag]
whatsnew = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "whatsnew"
    , commandHelp = whatsnewHelp
    , commandDescription = whatsnewDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = whatsnewCmd
    , commandPrereq = amInRepository
    , commandGetArgPossibilities = listRegisteredFiles
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc whatsnewAdvancedOpts
    , commandBasicOptions = odesc whatsnewBasicOpts
    , commandDefaults = defaultFlags whatsnewOpts
    , commandCheckOptions = ocheck whatsnewOpts
    , commandParseOptions = onormalise whatsnewOpts
    }

whatsnewDescription :: String
whatsnewDescription = "List unrecorded changes in the working tree."

whatsnewHelp :: String
whatsnewHelp =
 "The `darcs whatsnew` command lists unrecorded changes to the working\n" ++
 "tree.  If you specify a set of files and directories, only unrecorded\n" ++
 "changes to those files and directories are listed.\n" ++
 "\n" ++
 "With the `--summary` option, the changes are condensed to one line per\n" ++
 "file, with mnemonics to indicate the nature and extent of the change.\n" ++
 "The `--look-for-adds` option causes candidates for `darcs add` to be\n" ++
 "included in the summary output.  Summary mnemonics are as follows:\n" ++
 "\n" ++
 "* `A f` and `A d/` respectively mean an added file or directory.\n" ++
 "* `R f` and `R d/` respectively mean a removed file or directory.\n" ++
 "* `M f -N +M rP` means a modified file, with `N` lines deleted, `M`\n" ++
 "  lines added, and `P` lexical replacements.\n" ++
 "* `f -> g` means a moved file or directory.\n" ++
 "* `a f` and `a d/` respectively mean a new, but unadded, file or\n" ++
 "  directory, when using `--look-for-adds`.\n" ++
 "\n" ++
 "  An exclamation mark (!) as in `R! foo.c`, means the hunk is known to\n" ++
 "  conflict with a hunk in another patch.  The phrase `duplicated`\n" ++
 "  means the hunk is known to be identical to a hunk in another patch.\n" ++
 "\n" ++
 "By default, `darcs whatsnew` uses Darcs' internal format for changes.\n" ++
 "To see some context (unchanged lines) around each change, use the\n" ++
 "`--unified` option.  To view changes in conventional `diff` format, use\n" ++
 "the `darcs diff` command; but note that `darcs whatsnew` is faster.\n" ++
 "\n" ++
 "This command exits unsuccessfully (returns a non-zero exit status) if\n" ++
 "there are no unrecorded changes.\n"

whatsnewCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
whatsnewCmd fps opts args =
   withRepository (useCache opts) $ RepoJob $ \(repo :: Repository p wR wU wR) -> do
    files <- if null args
                 then return Nothing
                 else Just <$> fixSubPaths fps args
    let isLookForMoves = lookForMoves opts == O.YesLookForMoves && parseFlags O.summary opts /= Just O.NoSummary
        isLookForAdds = lookForAdds opts == O.YesLookForAdds && parseFlags O.summary opts /= Just O.NoSummary
        isLookForReplaces = lookForReplaces opts == O.YesLookForReplaces
        -- LookForAdds and LookForMoves implies Summary, unless it's explcitly disabled.
        optsModifier = if isLookForAdds
                           then (Summary :) . filter (\o -> LookForAdds /= o &&
                                                            LookForMoves /= o )
                           else id
        opts' = optsModifier opts
    movesPs <- if isLookForMoves
        then getMovesPs repo files
        else return NilFL
    Sealed replacePs <- if isLookForReplaces
        then getReplaces (diffingOpts opts) repo files
        else return (Sealed NilFL)
    Sealed noLookChanges <- filteredUnrecordedChanges opts' repo files movesPs
                                                                       (unsafeCoerceP replacePs :: FL (PrimOf p) wR wR)
    pristine <- readRecorded repo
    -- If we are looking for moves, return the corresponding FL of changes.
    -- If we are looking for adds, return the corresponding FL of changes.
    Sealed unaddedNewPathsPs <- if isLookForAdds
        then do
            -- Use opts not opts', here, since we *do* want to look for adds.
            Sealed lookChanges <- filteredUnrecordedChanges opts repo files movesPs (unsafeCoerceP replacePs :: FL (PrimOf p) wR wR)
            noLookAddsTree <- applyAddPatchesToPristine noLookChanges pristine
            lookAddsTree <- applyAddPatchesToPristine lookChanges pristine
            ftf <- filetypeFunction
            -- Return the patches that create files/dirs that aren't yet added.
            unFreeLeft <$> treeDiff (diffAlgorithm opts) ftf noLookAddsTree lookAddsTree
        else return (Sealed NilFL)
    announceFiles files "What's new in"
    exitOnNoChanges (unaddedNewPathsPs, noLookChanges)
    if maybeIsInteractive opts
      then runInteractive (interactiveHunks pristine) opts' pristine noLookChanges
      else do
        printChanges opts' pristine noLookChanges
        printUnaddedPaths unaddedNewPathsPs
  where
    -- |Filter out hunk patches (leaving add patches) and return the tree
    -- resulting from applying the filtered patches to the pristine tree.
    applyAddPatchesToPristine ps pristine = do
        adds :> _ <- return $ partitionRL primIsHunk $ reverseFL ps
        applyToTree (reverseRL adds) pristine

    exitOnNoChanges :: (FL p wX wY, FL p wU wV) -> IO ()
    exitOnNoChanges (NilFL, NilFL) = do putStrLn "No changes!"
                                        exitWith $ ExitFailure 1
    exitOnNoChanges _ = return ()

    printUnaddedPaths :: PrimPatch p => FL p wX wY -> IO ()
    printUnaddedPaths NilFL = return ()
    printUnaddedPaths ps =
        putDocLn . lowercaseAs . renderString Encode . plainSummaryPrims $ ps

    -- Make any add markers lowercase, to distinguish new-but-unadded files
    -- from those that are unrecorded, but added.
    lowercaseAs x = vcat $ map (text . lowercaseA) $ lines x
    lowercaseA ('A' : x) = 'a' : x
    lowercaseA x = x

    -- |Appropriately print changes, according to the passed flags.
    printChanges :: (PatchListFormat p, IsHunk p, Patchy p, ShowPatch p, PrimDetails p,
                 ApplyState p ~ Tree) => [DarcsFlag] -> Tree IO -> FL p wX wY
                 -> IO ()
    printChanges opts' pristine changes
        | Summary `elem` opts' = putDocLn $ plainSummaryPrims changes
        | isUnified opts' == O.YesContext = contextualPrintPatch pristine changes
        | otherwise = printPatch changes

    -- |return the unrecorded changes that affect an optional list of paths.
    filteredUnrecordedChanges :: forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree,
                              ApplyState (PrimOf p) ~ Tree) => [DarcsFlag]
                              -> Repository p wR wU wT -> Maybe [SubPath]
                              -> FL (PrimOf p) wR wT -- look-for-moves patches
                              -> FL (PrimOf p) wT wT -- look-for-replaces patches
                              -> IO (Sealed (FL (PrimOf p) wT))
    filteredUnrecordedChanges  opts' repo files movesPs replacesPs =
        let filePaths = map toFilePath <$> files in
        choosePreTouching filePaths <$> unrecordedChangesWithPatches (diffingOpts opts') repo files movesPs replacesPs

-- | Runs the 'InteractiveSelectionM' code
runInteractive :: (PatchListFormat p, IsHunk p, Patchy p, ShowPatch p,
                     PrimPatch p, PatchInspect p, PrimDetails p,
                     ApplyState p ~ Tree)
               => InteractiveSelectionM p wX wY () -- Selection to run
               -> [DarcsFlag]     -- Command-line options
               -> Tree IO         -- Pristine
               -> FL p wX wY      -- A list of patches
               -> IO ()
runInteractive i opts pristine ps' = do
    let (choices',lps') = patchChoicesLps ps'
    let ps = evalStateT i $
             ISC { total   = lengthFL lps'
                 , current = 0
                 , lps     = FZipper NilRL lps'
                 , choices = choices'
                 }
    void $ runReaderT ps $
           selectionContextPrim First "view" (patchSelOpts opts) (Just primSplitter)
             Nothing (Just pristine)

-- | The interactive part of @darcs whatsnew@
interactiveHunks :: (PatchListFormat p, IsHunk p, Patchy p, ShowPatch p,
                     PatchInspect p, PrimDetails p, ApplyState p ~ Tree)
                 => Tree IO -> InteractiveSelectionM p wX wY ()
interactiveHunks pristine = do
    c <- currentPatch
    case c of
        Nothing -> liftIO $ putStrLn "No more changes!"
        Just (Sealed2 lp) -> do
            liftIO $ printPatch (lpPatch lp)
            repeatThis lp
  where
    repeatThis lp = do
        thePrompt <- prompt -- "Shall I view this change? (n/m)"
        yorn <- liftIO $ promptChar
                (PromptConfig thePrompt (keysFor basic_options) (keysFor adv_options)
                 (Just 'n') "?h")
        case yorn of
            -- View hunk in context
            'v' -> liftIO (contextualPrintPatch pristine (lpPatch lp))
                   >> repeatThis lp
            -- View summary of the change
            'x' -> liftIO (printSummary (lpPatch lp))
                   >> repeatThis lp
            -- View hunk and move on
            'y' -> liftIO (contextualPrintPatch pristine (lpPatch lp))
                   >> decide True lp >> next_hunk
            -- Go to the next patch
            'n' -> decide False lp >> next_hunk
            -- Skip the whole file
            's' -> do
                currentFile >>= maybe
                    (return ())
                    (\f -> decideWholeFile f False)
                next_hunk
            -- View hunk in a pager
            'p' -> liftIO (printPatchPager $ lpPatch lp)
                   >> repeatThis lp
            -- Next hunk
            'j' -> next_hunk
            -- Previous hunk
            'k' -> prev_hunk
            -- Start from the first change
            'g' -> start_over
            -- Quit whatsnew
            'q' -> liftIO $ exitSuccess
            _ -> do liftIO . putStrLn $
                        helpFor "whatsnew" basic_options adv_options
                    repeatThis lp
    start_over = backAll >> interactiveHunks pristine
    next_hunk  = skipOne >> skipMundane >> interactiveHunks pristine
    prev_hunk  = backOne >> interactiveHunks pristine
    options_yn =
        [ KeyPress 'v' "view this hunk in a context"
        , KeyPress 'y'
          "view this hunk in a context and go to the next one"
        , KeyPress 'n' "go to the next hunk" ]
    optionsView =
        [ KeyPress 'p' "view this hunk in context wih pager "
        , KeyPress 'x' "view a summary of this patch"
        ]
    optionsNav =
        [ KeyPress 'q' "quit whatsnew"
        , KeyPress 's' "skip the rest of the changes to this file"
        , KeyPress 'j' "skip to the next hunk"
        , KeyPress 'k' "back up to previous hunk"
        , KeyPress 'g' "start over from the first hunk"
        ]
    basic_options = [ options_yn ]
    adv_options = [ optionsView, optionsNav ]

-- |status is an alias for whatsnew, with implicit Summary and LookForAdds
-- flags. We override the default description, to include the implicit flags.
status :: DarcsCommand [DarcsFlag]
status = statusAlias { commandCommand = statusCmd
                     , commandDescription = statusDesc
                     }
  where
    statusAlias = commandAlias "status" Nothing whatsnew
    statusCmd fps fs = commandCommand whatsnew fps (Summary : LookForAdds : fs)
    statusDesc = "Alias for `darcs " ++ commandName whatsnew ++ " -ls '."

maybeIsInteractive :: [DarcsFlag] -> Bool
maybeIsInteractive = maybe False id . parseFlags O.interactive
