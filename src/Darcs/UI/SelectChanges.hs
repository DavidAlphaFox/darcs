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

{-# LANGUAGE CPP #-}


module Darcs.UI.SelectChanges
    ( -- * Working with changes
      selectChanges
    , WhichChanges(..)
    , viewChanges
    , withSelectedPatchFromRepo
    , runSelection
    , selectionContextPrim
    , selectionContextGeneric
    , selectionContext
    , PatchSelectionContext(allowSkipAll)
    , printSummary
    -- * Interactive selection utils
    , PatchSelectionOptions(..)
    , InteractiveSelectionM
    , InteractiveSelectionContext(..)
    -- ** Navigating the patchset
    , currentPatch
    , skipMundane
    , skipOne
    , backOne
    , backAll
    , showCur
    -- ** Decisions
    , decide
    , decideWholeFile
    -- ** Prompts and queries
    , isSingleFile
    , currentFile
    , promptUser
    , prompt
    , KeyPress(..)
    , keysFor
    , helpFor
    , askAboutDepends
    ) where

import Prelude hiding ( (^) )
import Control.Monad ( liftM, unless, when, (>=>) )
import Control.Monad.Identity ( Identity (..) )
import Control.Monad.Reader
    ( Reader, ReaderT, asks
    , runReader, runReaderT
    )
import Control.Monad.State
    ( StateT, execStateT, gets
    , modify, runStateT
    )
import Control.Monad.Trans ( liftIO )
import Data.Char ( toUpper )
import Data.List ( intercalate, union )
import Data.Maybe ( isJust, isNothing, catMaybes )
import System.Exit ( exitSuccess )

import Darcs.Patch
    ( Patchy, PrimPatch, RepoPatch, PrimOf
    , commuteFLorComplain, invert
    , listTouchedFiles, anonymous, fromPrims
    )
import qualified Darcs.Patch ( thing, things, summary )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Choices
    ( PatchChoices, Slot (..), LabelledPatch
    , patchChoicesLps, forceFirsts
    , forceFirst, forceLast, forceMatchingFirst
    , forceMatchingLast, getChoices
    , makeEverythingLater, makeEverythingSooner
    , makeUncertain, patchChoices
    , patchChoicesLpsSub, patchSlot'
    , refineChoices, selectAllMiddles
    , separateFirstFromMiddleLast
    , substitute, label, lpPatch
    )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Match ( haveNonrangeMatch, matchAPatch, matchAPatchread )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, n2pia )
import Darcs.Patch.Set ( PatchSet(..), newset2RL )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.Split ( Splitter (..) )
import qualified Darcs.Patch.TouchesFiles as TouchesFiles
import Darcs.Patch.Type ( PatchType (..) )
import Darcs.Patch.Witnesses.Eq ( unsafeCompare )
import Darcs.Patch.Witnesses.Ordered
    ( (:>) (..), (:||:) (..), FL (..)
    , RL (..), filterFL, lengthFL, mapFL
    , mapFL_FL, reverseFL, spanFL, spanFL_M
    , (+<+), (+>+), reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal (..), Sealed2 (..)
    , flipSeal, seal2, unseal2
    )
import Darcs.Patch.Witnesses.WZipper
    ( FZipper (..), left, right
    , rightmost, toEnd, toStart
    )
import Darcs.Repository ( Repository, readRepo, readTentativeRepo )
import Darcs.UI.External ( editText )
import Darcs.UI.Options.All
    ( Verbosity(..), Summary(..), DiffAlgorithm(..)
    , WithContext(..), SelectDeps(..), MatchFlag )
import Darcs.UI.PrintPatch
    ( printFriendly, printPatch
    , printPatchPager, showFriendly )
import Darcs.Util.English ( Noun (..), englishNum )
import Darcs.Util.Printer ( prefix, putDocLn )
import Darcs.Util.Prompt ( PromptConfig (..), askUser, promptChar )
import Storage.Hashed.Tree ( Tree )


-- | When asking about patches, we either ask about them in
-- oldest-first or newest first (with respect to the current ordering
-- of the repository), and we either want an initial segment or a
-- final segment of the poset of patches.
--
-- @First@: ask for an initial
-- segment, first patches first (default for all pull-like commands)
--
-- @FirstReversed@: ask for an initial segment, last patches first
-- (used to ask about dependencies in record, and for pull-like
-- commands with the @--reverse@ flag).
--
-- @LastReversed@: ask for a final segment, last patches first. (default
-- for unpull-like commands, except for selecting *primitive* patches in
-- rollback)
--
-- @Last@: ask for a final segment, first patches first. (used for selecting
-- primitive patches in rollback, and for unpull-like commands with the
-- @--reverse@ flag
data WhichChanges = Last | LastReversed | First | FirstReversed deriving (Eq, Show)

-- | A @WhichChanges@ is backwards if the order in which patches are presented
-- is the opposite of the order of dependencies for that operation.
backward :: WhichChanges -> Bool
backward w = w == Last || w == FirstReversed

-- | The type of the function we use to filter patches when @--match@ is
-- given.
data MatchCriterion p = MatchCriterion
   { mcHasNonrange :: Bool
   , mcFunction :: WhichChanges -> Sealed2 p -> Bool
   }

data PatchSelectionOptions = PatchSelectionOptions
  { verbosity :: Verbosity
  , matchFlags :: [MatchFlag]
  , diffAlgorithm :: DiffAlgorithm
  , interactive :: Bool
  , selectDeps :: SelectDeps
  , summary :: Summary
  , withContext :: WithContext
  }

-- | A @PatchSelectionContext@ contains all the static settings for selecting
-- patches. See "PatchSelectionM"
data PatchSelectionContext p = PSC { opts :: PatchSelectionOptions
                                   , splitter :: Maybe (Splitter p)
                                   , files :: Maybe [FilePath]
                                   , matchCriterion :: MatchCriterion p
                                   , jobname :: String
                                   , allowSkipAll :: Bool
                                   , pristine :: Maybe (Tree IO)
                                   , whichChanges :: WhichChanges
                                   }

-- | A 'PatchSelectionContext' for selecting 'Prim' patches.
selectionContextPrim :: PrimPatch prim
                     => WhichChanges
                     -> String
                     -> PatchSelectionOptions
                     -> Maybe (Splitter prim)
                     -> Maybe [FilePath]
                     -> Maybe (Tree IO)
                     -> PatchSelectionContext prim
selectionContextPrim whch jn o spl fs p =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = triv
     , jobname = jn
     , allowSkipAll = True
     , pristine = p
     , whichChanges = whch
     }

-- | A 'PatchSelectionContext' for selecting full patches ('PatchInfoAnd' patches)
selectionContext :: (RepoPatch p) => WhichChanges -> String -> PatchSelectionOptions -> Maybe (Splitter (PatchInfoAnd p))
                 -> Maybe [FilePath] -> PatchSelectionContext (PatchInfoAnd p)
selectionContext whch jn o spl fs =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = iswanted seal2 (matchFlags o)
     , jobname = jn
     , allowSkipAll = True
     , pristine = Nothing
     , whichChanges = whch
     }

-- | A generic 'PatchSelectionContext'.
selectionContextGeneric :: (RepoPatch p, Invert q)
                        => (forall wX wY . q wX wY -> Sealed2 (PatchInfoAnd p))
                        -> WhichChanges
                        -> String
                        -> PatchSelectionOptions
                        -> Maybe [FilePath]
                        -> PatchSelectionContext q
selectionContextGeneric extract whch jn o fs =
 PSC { opts = o
     , splitter = Nothing
     , files = fs
     , matchCriterion = iswanted extract (matchFlags o)
     , jobname = jn
     , allowSkipAll = True
     , pristine = Nothing
     , whichChanges = whch
     }

-- | The dynamic parameters for interactive selection of patches.
data InteractiveSelectionContext p wX wY = ISC { total :: Int
                                                  -- ^ total number of patches
                                                , current :: Int
                                                  -- ^ number of already-seen patches
                                                , lps :: FZipper (LabelledPatch p) wX wY
                                                  -- ^ the patches we offer
                                                , choices :: PatchChoices p wX wY
                                                  -- ^ the user's choices
                                                }

type PatchSelectionM p a = ReaderT (PatchSelectionContext p) a

type InteractiveSelectionM p wX wY a =
    StateT (InteractiveSelectionContext p wX wY)
           (PatchSelectionM p IO) a

type PatchSelection p wX wY =
        PatchSelectionM p IO ((FL p :> FL p) wX wY)

-- Common match criteria

-- | For commands without @--match@, 'triv' matches all patches
triv :: MatchCriterion p
triv = MatchCriterion { mcHasNonrange = False, mcFunction = \ _ _ -> True }

-- | 'iswanted' selects patches according to the given match flags
iswanted :: forall p q
          . (RepoPatch p, Invert q)
         => (forall wX wY . q wX wY -> Sealed2 (PatchInfoAnd p))
         -> [MatchFlag]
         -> MatchCriterion q
iswanted extract mflags = MatchCriterion
    { mcHasNonrange = haveNonrangeMatch (PatchType :: PatchType p) mflags
    , mcFunction = isWantedMcFunction
    }
  where
    isWantedMcFunction x = unseal2 $ iw x
    iw First = unseal2 (matchAPatch mflags) . extract
    iw Last = unseal2 (matchAPatch mflags) . extract
    iw LastReversed = unseal2 (matchAPatch mflags) . extract . invert
    iw FirstReversed = unseal2 (matchAPatch mflags) . extract . invert

liftR :: Monad m => Reader r a -> ReaderT r m a
liftR = asks . runReader

-- | runs a 'PatchSelection' action in the given 'PatchSelectionContext'.
runSelection :: (Patchy p) => PatchSelection p wX wY -> PatchSelectionContext p
             -> IO ((FL p :> FL p) wX wY)
runSelection = runReaderT

-- | Select patches from a @FL@.
selectChanges :: forall p wX wY . (Patchy p, PatchInspect p, ShowPatch p, ApplyState p ~ Tree) =>
                                FL p wX wY
                             -> PatchSelection p wX wY
selectChanges ps = do
    whch <- asks whichChanges
    case whch of
        First -> normal ps
        Last -> normal ps
        FirstReversed -> reversed ps
        LastReversed -> reversed ps
  where normal = sc1
        reversed = return . invert >=> sc1 >=> return . invertC

sc1 :: forall p wX wY . (Patchy p, PatchInspect p, ShowPatch p, ApplyState p ~ Tree) =>
                                FL p wX wY
                             -> PatchSelection p wX wY
sc1 =
    (liftR . patchesToConsider)
     >=> realSelectChanges
     >=> (\ps -> do whch <- asks whichChanges ; return $ selectedPatches whch ps)
     >=> (liftR . canonizeAfterSplitter)

-- | inverses the choices that have been made
invertC :: (Patchy p) => (FL p :> FL p) wX wY -> (FL p :> FL p) wY wX
invertC (a :> b) = invert b :> invert a

-- | Shows the patch that is actually being selected the way the user
-- should see it.
repr :: (Patchy p) => WhichChanges -> Sealed2 p -> Sealed2 p
repr First (Sealed2 p) = Sealed2 p
repr LastReversed (Sealed2 p) = Sealed2 (invert p)
repr Last (Sealed2 p) = Sealed2 p
repr FirstReversed (Sealed2 p) = Sealed2 (invert p)

-- | The equivalent of 'selectChanges' for the @darcs changes@ command
viewChanges :: (Patchy p, ShowPatch p, ApplyState p ~ Tree) => PatchSelectionOptions -> [Sealed2 p] -> IO ()
viewChanges ps_opts = textView ps_opts Nothing 0 []

-- | The type of the answers to a "shall I [wiggle] that [foo]?" question
-- They are found in a [[KeyPress]] bunch, each list representing a set of
-- answers which belong together
data KeyPress = KeyPress { kp     :: Char
                           , kpHelp :: String }

-- | Generates the help for a set of basic and advanced 'KeyPress' groups.
helpFor :: String -> [[KeyPress]] -> [[KeyPress]] -> String
helpFor jn basicKeypresses advancedKeyPresses =
  unlines $ [ "How to use "++jn++":" ]
            ++ intercalate [""] (map (map help) keypresses)
            ++ [ ""
               , "?: show this help"
               , ""
               , "<Space>: accept the current default (which is capitalized)"
               ]
  where help i = kp i:(": "++kpHelp i)
        keypresses = basicKeypresses ++ advancedKeyPresses

-- | The keys used by a list of 'keyPress' groups.
keysFor :: [[KeyPress]] -> [Char]
keysFor = concatMap (map kp)

-- | The function for selecting a patch to amend record. Read at your own risks.
withSelectedPatchFromRepo ::
       forall p wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
    => String   -- name of calling command (always "amend" as of now)
    -> Repository p wR wU wT
    -> PatchSelectionOptions
    -> (forall wA . (FL (PatchInfoAnd p) :> PatchInfoAnd p) wA wR -> IO ())
    -> IO ()
withSelectedPatchFromRepo jn repository o job = do
    patchSet <- readRepo repository
    sp <- wspfr jn (matchAPatchread $ matchFlags o) (newset2RL patchSet) NilFL
    case sp of
        Just (FlippedSeal (skipped :> selected')) -> job (skipped :> selected')
        Nothing ->
            putStrLn $ "Cancelling " ++ jn ++ " since no patch was selected."

data SkippedReason = SkippedAutomatically | SkippedManually

data WithSkipped p wX wY = WithSkipped { _skippedReason :: SkippedReason, skippedPatch :: p wX wY }

-- | This ensures that the selected patch commutes freely with the skipped
-- patches, including pending and also that the skipped sequences has an
-- ending context that matches the recorded state, z, of the repository.
wspfr :: forall p wX wY wU. (RepoPatch p, ApplyState p ~ Tree)
      => String
      -> (forall wA wB . (PatchInfoAnd p) wA wB -> Bool)
      -> RL (PatchInfoAnd p) wX wY
      -> FL (WithSkipped (PatchInfoAnd p)) wY wU
      -> IO (Maybe (FlippedSeal (FL (PatchInfoAnd p) :> PatchInfoAnd p) wU))
wspfr _ _ NilRL _ = return Nothing
wspfr jn matches remaining@(p:<:pps) skipped
    | not $ matches p = wspfr jn matches pps
                            (WithSkipped SkippedAutomatically p :>: skipped)
    | otherwise =
    case commuteFLorComplain (p :> mapFL_FL skippedPatch skipped) of
    Left _  -> do putStrLn "\nSkipping depended-upon patch:"
                  defaultPrintFriendly p
                  wspfr jn matches pps (WithSkipped SkippedAutomatically p :>: skipped)

    Right (skipped' :> p') -> do
        defaultPrintFriendly p
        yorn <- promptChar
                    PromptConfig { pPrompt = prompt'
                                 , pBasicCharacters = keysFor basicOptions
                                 , pAdvancedCharacters = keysFor advancedOptions
                                 , pDefault = Just 'n'
                                 , pHelp = "?h" }
        case yorn of
            'y' -> return $ Just $ flipSeal $ skipped' :> p'
            'n' -> nextPatch
            'j' -> nextPatch
            'k' -> previousPatch remaining skipped
            'v' -> printPatch p >> repeatThis
            'p' -> printPatchPager p >> repeatThis
            'x' -> do putDocLn $ prefix "    " $ Darcs.Patch.summary p
                      repeatThis
            'q' -> do putStrLn $ jnCapital ++ " cancelled."
                      exitSuccess
            _   -> do putStrLn $ helpFor jn basicOptions advancedOptions
                      repeatThis
  where jnCapital = toUpper (head jn) : tail jn
        repeatThis = wspfr jn matches (p:<:pps) skipped
        prompt' = "Shall I " ++ jn ++ " this patch?"
        nextPatch = wspfr jn matches pps (WithSkipped SkippedManually p:>:skipped)
        previousPatch :: RL (PatchInfoAnd p) wX wQ
                      -> FL (WithSkipped (PatchInfoAnd p)) wQ wU
                      -> IO (Maybe (FlippedSeal (FL (PatchInfoAnd p) :> PatchInfoAnd p) wU))
        previousPatch remaining' NilFL = wspfr jn matches remaining' NilFL
        previousPatch remaining' (WithSkipped sk prev :>: skipped'') =
            case sk of
                SkippedManually -> wspfr jn matches (prev :<: remaining') skipped''
                SkippedAutomatically -> previousPatch (prev :<: remaining') skipped''
        basicOptions =
                    [[ KeyPress 'y' (jn ++ " this patch")
                     , KeyPress 'n' ("don't " ++ jn ++ " it")
                     , KeyPress 'j' "skip to next patch"
                     , KeyPress 'k' "back up to previous patch"
                    ]]
        advancedOptions =
                    [[ KeyPress 'v' "view this patch in full"
                     , KeyPress 'p' "view this patch in full with pager"
                     , KeyPress 'x' "view a summary of this patch"
                     , KeyPress 'q' ("cancel " ++ jn)
                    ]]
        defaultPrintFriendly =
          printFriendly Nothing NormalVerbosity NoSummary NoContext

-- After selecting with a splitter, the results may not be canonical
canonizeAfterSplitter :: (FL p :> FL p) wX wY -> Reader (PatchSelectionContext p) ((FL p :> FL p) wX wY)
canonizeAfterSplitter (x :> y) =
    do o <- asks opts
       mspl <- asks splitter
       let  canonizeIfNeeded = maybe (\_ b -> b) canonizeSplit mspl
            da = diffAlgorithm o
       return $ canonizeIfNeeded da x :> canonizeIfNeeded da y

realSelectChanges :: forall p wX wY.
                     (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree)
                  => PatchChoices p wX wY
                  -> PatchSelectionM p IO (PatchChoices p wX wY)
realSelectChanges autoChoices =
    do
      o <- asks opts
      whch <- asks whichChanges
      if not $ interactive o
       then return $ promote whch autoChoices
       else refineChoices textSelect autoChoices
    where forward whch = not $ backward whch
          promote whch =
              if forward whch
                  then makeEverythingSooner
                  else makeEverythingLater

-- | When using @--match@, remove unmatched patches not depended upon by matched
-- patches.
deselectUnwanted :: forall p wX wY . Patchy p =>
                     PatchChoices p wX wY ->
                     Reader (PatchSelectionContext p) (PatchChoices p wX wY)
deselectUnwanted pc =
    do
      o <- asks opts
      mc <- asks matchCriterion
      whichch <- asks whichChanges
      let iswanted_ = mcFunction mc whichch . seal2 . lpPatch
          select = if forward whichch
                   then forceMatchingFirst iswanted_
                   else forceMatchingLast iswanted_
          deselect = if forward whichch
                     then forceMatchingLast (not . iswanted_)
                     else forceMatchingFirst (not . iswanted_)
      return $
        if mcHasNonrange mc
            then if selectDeps o == NoDeps
                 then deselect pc
                 else demote whichch $ select pc
            else pc
    where
      forward whichch = not $ backward whichch
      demote whichch =
          if forward whichch
            then makeEverythingLater
            else makeEverythingSooner

-- | Selects the patches matching the match criterion, and puts them first or last
-- according to whch, while respecting any dependencies.
patchesToConsider :: forall p wX wY
                      . (Patchy p, PatchInspect p, ApplyState p ~ Tree)
                     => FL p wX wY
                     -> Reader (PatchSelectionContext p) (PatchChoices p wX wY)
patchesToConsider ps =
    do
      fs <- asks files
      crit <- asks matchCriterion
      whch <- asks whichChanges
      let deselectNotTouching =
              case whch of
                First -> TouchesFiles.deselectNotTouching
                Last -> TouchesFiles.selectNotTouching
                FirstReversed -> TouchesFiles.selectNotTouching
                LastReversed -> TouchesFiles.deselectNotTouching
          everything = patchChoices ps
      if isNothing fs && not (mcHasNonrange crit)
         then return everything
         else do notUnwanted <- deselectUnwanted everything
                 return $ deselectNotTouching fs notUnwanted

-- | Returns the results of a patch selection user interaction
selectedPatches :: Patchy p => WhichChanges -> PatchChoices p wY wZ
                      -> (FL p :> FL p) wY wZ
selectedPatches Last pc =
  case getChoices pc of
   fc :> mc :> lc -> mapFL_FL lpPatch (fc +>+ mc) :> mapFL_FL lpPatch lc

selectedPatches First pc =
  case separateFirstFromMiddleLast pc of
  xs :> ys -> mapFL_FL lpPatch xs :> mapFL_FL lpPatch ys

selectedPatches LastReversed pc =
  case separateFirstFromMiddleLast pc of
  xs :> ys -> mapFL_FL lpPatch xs :> mapFL_FL lpPatch ys

selectedPatches FirstReversed pc =
  case getChoices pc of
  fc :> mc :> lc -> mapFL_FL lpPatch (fc +>+ mc) :> mapFL_FL lpPatch lc

-- | Runs a function on the underlying @PatchChoices@ object
liftChoices :: forall p a wX wY . Patchy p =>
               StateT (PatchChoices p wX wY) Identity a
                   -> InteractiveSelectionM p wX wY a
liftChoices act = do
  ch <- gets choices
  let (result, _) = runIdentity $ runStateT act ch
  modify $ \isc -> isc {choices = ch} -- Should this be ch or the result of runState?
  return result

-- | @justDone n@ notes that @n@ patches have just been processed
justDone :: Patchy p => Int -> InteractiveSelectionM p wX wY ()
justDone n = modify $ \isc -> isc{ current = current isc + n}

-- | The actual interactive selection process.
textSelect :: forall p wX wY . (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree)
           => FL (LabelledPatch p) wX wY -> PatchChoices p wX wY
           -> PatchSelectionM p IO (PatchChoices p wX wY)
textSelect lps' pcs = do
    userSelection <- execStateT (skipMundane >>
                                 showCur >>
                                 textSelectIfAny)
                     ISC { total = lengthFL lps'
                         , current = 0
                         , lps = FZipper NilRL lps'
                         , choices = pcs }
    return $ choices userSelection
    where textSelectIfAny = do
            z <- gets lps
            unless (rightmost z) $
              textSelect'

textSelect' :: (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree)
            => InteractiveSelectionM p wX wY ()
textSelect' = do
  z <- gets lps
  done <- if not $ rightmost z
           then textSelectOne
           else lastQuestion
  unless done $ textSelect'

optionsBasic :: String -> String -> [KeyPress]
optionsBasic jn aThing =
    [ KeyPress 'y' (jn++" this "++aThing)
    , KeyPress 'n' ("don't "++jn++" it")
    , KeyPress 'w' "wait and decide later, defaulting to no" ]

optionsFile :: String -> [KeyPress]
optionsFile jn =
    [ KeyPress 's' ("don't "++jn++" the rest of the changes to this file")
    , KeyPress 'f' (jn++" the rest of the changes to this file") ]

optionsView :: String -> String -> [KeyPress]
optionsView aThing someThings =
    [ KeyPress 'v' ("view this "++aThing++" in full")
    , KeyPress 'p' ("view this "++aThing++" in full with pager")
    , KeyPress 'l' ("list all selected "++someThings) ]

optionsSummary :: String -> [KeyPress]
optionsSummary aThing =
    [ KeyPress 'x' ("view a summary of this "++aThing) ]

optionsQuit :: String -> Bool -> String -> [KeyPress]
optionsQuit jn allowsa someThings =
    [ KeyPress 'd' (jn++" selected "++someThings++", skipping all the remaining "++someThings)
            | allowsa ]
    ++
    [ KeyPress 'a' (jn++" all the remaining "++someThings)
    , KeyPress 'q' ("cancel "++jn) ]

optionsNav :: String -> Bool -> [KeyPress]
optionsNav aThing isLast=
    [ KeyPress 'j' ("skip to next "++ aThing) | not isLast ]
    ++
    [ KeyPress 'k' ("back up to previous "++ aThing)
    , KeyPress 'g' ("start over from the first "++aThing)]

optionsSplit :: Maybe (Splitter a) -> String -> [KeyPress]
optionsSplit split aThing
    | Just _ <- split
             = [ KeyPress 'e' ("interactively edit this "++ aThing) ]
    | otherwise = []

optionsLast :: String -> String -> ([[KeyPress]], [[KeyPress]])
optionsLast jn aThing =
  (optionsNav aThing True:
   [[ KeyPress 'y' "confirm this operation"
    , KeyPress 'q' ("cancel " ++ jn) ]
    , [ KeyPress 'l' "list all selected" ]
   ]
  ,[[KeyPress 'a' "confirm this operation"
    , KeyPress 'd' "confirm this operation"
    , KeyPress 'n' ("cancel " ++ jn) ]])

options :: forall p wX wY . (Patchy p, ShowPatch p) => Bool ->
           InteractiveSelectionM p wX wY ([[KeyPress]],[[KeyPress]])
options single = do
  split <- asks splitter
  jn <- asks jobname
  allowsa <- asks allowSkipAll
  aThing <- thing
  someThings <- things
  o <- asks opts
  return ([optionsBasic jn aThing]
         ,[optionsSplit split aThing]
         ++ [optionsFile jn | single]
         ++ [optionsView aThing someThings ++
                if summary o == YesSummary
                    then []
                    else optionsSummary aThing]
         ++ [optionsQuit jn allowsa someThings]
         ++ [optionsNav aThing False]
         )

-- | Returns a @Sealed2@ version of the patch we are asking the user
-- about.
currentPatch :: forall p wX wY . Patchy p =>
               InteractiveSelectionM p wX wY
                    (Maybe (Sealed2 (LabelledPatch p)))
currentPatch = do
  (FZipper _ lps_todo) :: FZipper (LabelledPatch p) wX wY <- gets lps
  case lps_todo of
    NilFL -> return Nothing
    (lp:>:_) -> return $ Just (Sealed2 lp)

-- | Returns the patches we have yet to ask the user about.
todo :: forall p wX wY . Patchy p
        => InteractiveSelectionM p wX wY
              (FlippedSeal (FL (LabelledPatch p)) wY)
todo = do
    (FZipper _ lps_todo) <- gets lps
    return (FlippedSeal lps_todo)

-- | Modify the underlying @PatchChoices@ by some function
modChoices :: forall p wX wY . Patchy p =>
              (PatchChoices p wX wY -> PatchChoices p wX wY)
              -> InteractiveSelectionM p wX wY ()
modChoices f = modify $ \isc -> isc{choices = f $ choices isc}

-- | returns @Just f@ if the 'currentPatch' only modifies @f@,
-- @Nothing@ otherwise.
currentFile :: forall p wX wY . (Patchy p, PatchInspect p)
               => InteractiveSelectionM p wX wY (Maybe FilePath)
currentFile = do
  c <- currentPatch
  return $ case c of
             Nothing -> Nothing
             Just (Sealed2 lp) ->
                 case listTouchedFiles lp of
                   [f] -> Just f
                   _ -> Nothing

-- | @decide True@ selects the current patch, and @decide False@ deselects
-- it.
decide :: forall p wX wY wT wU . Patchy p => Bool
         -> LabelledPatch p wT wU
         -> InteractiveSelectionM p wX wY ()
decide takeOrDrop lp = do
    whch <- asks whichChanges
    if backward whch == takeOrDrop -- we go backward xor we are dropping
    then modChoices $ forceLast (label lp)
    else modChoices $ forceFirst (label lp)

-- | like 'decide', but for all patches touching @file@
decideWholeFile :: forall p wX wY. (Patchy p, PatchInspect p) =>
                  FilePath -> Bool -> InteractiveSelectionM p wX wY ()
decideWholeFile file takeOrDrop =
    do
      FlippedSeal lps_todo <- todo
      let patches_to_skip =
              filterFL (\lp' -> listTouchedFiles lp' == [file]) lps_todo
      mapM_ (unseal2 $ decide takeOrDrop) patches_to_skip

-- | Undecide the current patch.
postponeNext :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
postponeNext =
    do
      Just (Sealed2 lp) <- currentPatch
      modChoices $ makeUncertain (label lp)

-- | Focus the next patch.
skipOne :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
skipOne = modify so
    where so x = x{lps = right (lps x), current = current x +1}

-- | Focus the previous patch.
backOne :: forall p wX wY . Patchy p => InteractiveSelectionM p wX wY ()
backOne = modify so
    where so isc = isc{lps = left (lps isc), current = max (current isc-1) 0}

-- | Split the current patch (presumably a hunk), and add the replace it
-- with its parts.
splitCurrent :: forall p wX wY . Patchy p
             => Splitter p
             -> InteractiveSelectionM p wX wY ()
splitCurrent s = do
    FZipper lps_done (lp:>:lps_todo) <- gets lps
    o <- asks opts
    case applySplitter s (diffAlgorithm o) (lpPatch lp) of
      Nothing -> return ()
      Just (text, parse) ->
          do
            newText <- liftIO $ editText "darcs-patch-edit" text
            case parse newText of
               Nothing -> return ()
               Just ps -> do
                 lps_new <- liftIO . return . snd
                             $ patchChoicesLpsSub (Just (label lp)) ps
                 modify $ \isc -> isc { total = total isc + lengthFL lps_new - 1
                                      , lps = FZipper lps_done
                                               (lps_new +>+ lps_todo)
                                      , choices = substitute
                                                   (seal2 (lp :||: lps_new))
                                                   (choices isc)
                                      }

-- | Returns a list of the currently selected patches, in
-- their original context, i.e., not commuted past unselected
-- patches.
selected :: forall p wX wY. Patchy p =>
           InteractiveSelectionM p wX wY [Sealed2 p]
selected = do
  whichch <- asks whichChanges
  c <- gets choices
  (first_chs :> _ :> last_chs) <- return $ getChoices c
  return $ if backward whichch
           then
               mapFL (repr whichch . Sealed2 . lpPatch) last_chs
           else
               mapFL (repr whichch . Sealed2 . lpPatch) first_chs

-- | Prints the list of the selected patches. See 'selected'.
printSelected :: (Patchy p, ShowPatch p) =>
                InteractiveSelectionM p wX wY ()
printSelected = do
  someThings <- things
  o <- asks opts
  s <- selected
  liftIO $ do
    putStrLn $ "---- Already selected "++someThings++" ----"
    mapM_ (putDocLn . unseal2 (showFriendly (verbosity o) (summary o))) s
    putStrLn $ "---- end of already selected "++someThings++" ----"

printSummary :: forall p wX wY . ShowPatch p => p wX wY -> IO ()
printSummary = putDocLn . prefix "    " . Darcs.Patch.summary

-- | Skips all remaining patches.
skipAll :: forall p wX wY . Patchy p =>
          InteractiveSelectionM p wX wY ()
skipAll = modify $ \isc -> isc {lps = toEnd $ lps isc}

backAll :: forall p wX wY . Patchy p =>
          InteractiveSelectionM p wX wY ()
backAll = modify $ \isc -> isc {lps = toStart $ lps isc
                               ,current = 0}

isSingleFile :: PatchInspect p => p wX wY -> Bool
isSingleFile p = length (listTouchedFiles p) == 1

askConfirmation :: forall p wX wY . Patchy p =>
                   InteractiveSelectionM p wX wY ()
askConfirmation = do
    jn <- asks jobname
    liftIO $ when (jn `elem` ["unpull", "unrecord", "obliterate"]) $ do
               yorn <- askUser $ "Really " ++ jn ++ " all undecided patches? "
               case yorn of
                 ('y':_) -> return ()
                 _ -> exitSuccess

-- | The singular form of the noun for items of type @p@.
thing :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
thing = (Darcs.Patch.thing . helper) `liftM` gets choices
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The plural form of the noun for items of type @p@.
things :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
things = (Darcs.Patch.things . helper) `liftM` gets choices
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The question to ask about one patch.
prompt :: (Patchy p, ShowPatch p) => InteractiveSelectionM p wX wY String
prompt = do
  jn <- asks jobname
  aThing <- thing
  n <- gets current
  n_max <- gets total
  return $ "Shall I "++jn++" this "++aThing++"? "
             ++ "(" ++ show (n+1) ++ "/" ++ show n_max ++ ") "

-- | Asks the user about one patch, returns their answer.
promptUser :: forall p wX wY . (Patchy p, ShowPatch p) => Bool -> Char
              -> InteractiveSelectionM p wX wY Char
promptUser single def = do
  thePrompt <- prompt
  (basicOptions,advancedOptions) <- options single
  liftIO $ promptChar PromptConfig { pPrompt = thePrompt
                                   , pBasicCharacters = keysFor basicOptions
                                   , pAdvancedCharacters = keysFor advancedOptions
                                   , pDefault = Just def
                                   , pHelp = "?h"
                                   }

-- | Ask the user what to do with the next patch.
textSelectOne :: forall p wX wY. (Patchy p, ShowPatch p, PatchInspect p, ApplyState p ~ Tree)
              => InteractiveSelectionM p wX wY Bool
textSelectOne = do
 c <- currentPatch
 case c of
   Nothing -> return False
   Just (Sealed2 lp) ->
       do
         jn <- asks jobname
         spl <- asks splitter
         whichch <- asks whichChanges
         let singleFile = isSingleFile (lpPatch lp)
             reprCur = repr whichch (Sealed2 (lpPatch lp))
         (basicOptions,advancedOptions) <- options singleFile
         theSlot <- liftChoices $ patchSlot' lp
         let
             the_default = getDefault (whichch == Last || whichch == FirstReversed) theSlot
             jnCapital = toUpper (head jn) : tail jn
         yorn <- promptUser singleFile the_default
         let nextPatch = skipMundane >> showCur
         case yorn of
               'y' -> decide True lp >> skipOne >> nextPatch
                      >> return False
               'n' -> decide False lp >> skipOne >> nextPatch
                      >> return False
               'w' -> postponeNext >> skipOne >> nextPatch
                      >> return False
               'e' | (Just s) <- spl -> splitCurrent s >> showCur
                                        >> return False
               's' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile f False) >> nextPatch
                       >> return False
               'f' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile f True) >> nextPatch
                       >> return False
               'v' -> liftIO $ unseal2 printPatch reprCur >> return False
               'p' -> liftIO $ unseal2 printPatchPager reprCur >> return False
               'l' -> printSelected >> showCur >> return False
               'x' -> liftIO $ unseal2 printSummary reprCur >> return False
               'd' -> skipAll >> return True
               'g' -> backAll >> showCur >> return False
               'a' ->
                   do
                     askConfirmation
                     modChoices $ selectAllMiddles (whichch == Last || whichch == FirstReversed)
                     skipAll
                     return True
               'q' -> liftIO $
                      do putStrLn $ jnCapital++" cancelled."
                         exitSuccess
               'j' -> skipOne >> showCur >> return False
               'k' -> backOne >> showCur >> return False
               _   -> do
                 liftIO . putStrLn $ helpFor jn basicOptions advancedOptions
                 return False

lastQuestion :: forall p wX wY . (Patchy p, ShowPatch p, ApplyState p ~ Tree) =>
                InteractiveSelectionM p wX wY Bool
lastQuestion = do
  jn <- asks jobname
  theThings <-things
  aThing <- thing
  let (basicOptions, advancedOptions) = optionsLast jn aThing
  yorn <- liftIO . promptChar $
            PromptConfig { pPrompt = "Do you want to "++jn++" these "++ theThings++"?"
                         , pBasicCharacters = "yglqk"
                         , pAdvancedCharacters = "dan"
                         , pDefault = Just 'y'
                         , pHelp = "?h"}
  case yorn of c | c `elem` "yda" -> return True
                 | c `elem` "qn" -> liftIO $
                                    do putStrLn $ jn ++" cancelled."
                                       exitSuccess
               'g' -> backAll >> showCur >> return False
               'l' -> printSelected >> return False
               'k' -> backOne >> showCur >> return False
               _ -> do
                 liftIO . putStrLn $ helpFor "this confirmation prompt" basicOptions advancedOptions
                 return False

-- | Shows the current patch as it should be seen by the user.
showCur :: forall p wX wY . (Patchy p, ShowPatch p, ApplyState p ~ Tree)
        => InteractiveSelectionM p wX wY ()
showCur = do
  o <- asks opts
  p <- asks pristine
  c <- currentPatch
  whichch <- asks whichChanges
  case c of
      Nothing -> return ()
      Just (Sealed2 lp) -> do
             let reprCur = repr whichch (Sealed2 (lpPatch lp))
             liftIO . unseal2 (printFriendly p (verbosity o) (summary o) (withContext o)) $ reprCur

-- | The interactive part of @darcs changes@
textView :: forall p . (Patchy p, ShowPatch p, ApplyState p ~ Tree)
         => PatchSelectionOptions -> Maybe Int -> Int
         -> [Sealed2 p] -> [Sealed2 p]
         -> IO ()
textView _ _ _ _ [] = return ()
textView o n_max n
            ps_done ps_todo@(p:ps_todo') = do
      unseal2 (printFriendly Nothing (verbosity o) (summary o) (withContext o)) p
      repeatThis -- prompt the user
    where
        prev_patch :: IO ()
        prev_patch = case ps_done of
                       [] -> repeatThis
                       (p':ps_done') ->
                         textView o
                            n_max (n-1) ps_done' (p':ps_todo)
        next_patch :: IO ()
        next_patch = case ps_todo' of
                         [] -> -- May as well work out the length now we have all
                                  -- the patches in memory
                               textView o n_max
                                   n ps_done []
                         _ -> textView o n_max
                                  (n+1) (p:ps_done) ps_todo'
        first_patch = textView o n_max 0 [] (ps_done++ps_todo)
        options_yn =
          [ KeyPress 'y' "view this patch and go to the next"
          , KeyPress 'n' "skip to the next patch" ]
        optionsView' =
          [ KeyPress 'v' "view this patch in full"
          , KeyPress 'p' "view this patch in full with pager" ]
        optionsSummary' =
          [ KeyPress 'x' "view a summary of this patch" ]
        optionsNav' =
          [ KeyPress 'q' "quit view changes"
          , KeyPress 'k' "back up to previous patch"
          , KeyPress 'j' "skip to next patch"
          , KeyPress 'g' "start over from the first patch"
          , KeyPress 'c' "count total patch number" ]
        basicOptions = [ options_yn ]
        advancedOptions =
                     (optionsView' ++
                        if summary o == YesSummary then [] else optionsSummary')
                  : [ optionsNav' ]
        prompt' = "Shall I view this patch? "
               ++ "(" ++ show (n+1) ++ "/" ++ maybe "?" show n_max ++ ")"
        repeatThis :: IO ()
        repeatThis = do
          yorn <- promptChar (PromptConfig prompt' (keysFor basicOptions) (keysFor advancedOptions) (Just 'n') "?h")
          case yorn of
            'y' -> unseal2 printPatch p >> next_patch
            'n' -> next_patch
            'v' -> unseal2 printPatch p >> repeatThis
            'p' -> unseal2 printPatchPager p >> repeatThis
            'x' -> do putDocLn $ prefix "    " $ unseal2 Darcs.Patch.summary p
                      repeatThis
            'q' -> exitSuccess
            'k' -> prev_patch
            'j' -> next_patch
            'g' -> first_patch
            'c' -> textView o
                       count_n_max n ps_done ps_todo
            _   -> do putStrLn $ helpFor "view changes" basicOptions advancedOptions
                      repeatThis
        count_n_max | isJust n_max = n_max
                    | otherwise    = Just $ length ps_done + length ps_todo

-- | Skips patches we should not ask the user about
skipMundane :: (Patchy p, ShowPatch p)
            => InteractiveSelectionM p wX wY ()
skipMundane = do
  (FZipper lps_done lps_todo) <- gets lps
  o <- asks opts
  crit <- asks matchCriterion
  jn <- asks jobname
  whichch <- asks whichChanges
  (skipped :> unskipped) <- liftChoices $ spanFL_M
                                 (patchSlot' >=> return . decided)
                                 lps_todo
  let numSkipped = lengthFL skipped
  when (numSkipped > 0) . liftIO $ show_skipped o jn numSkipped skipped
  let boringThenInteresting =
          if selectDeps o == AutoDeps
          then spanFL (not. mcFunction crit whichch . seal2 . lpPatch)
                                 unskipped
          else NilFL :> unskipped
  case boringThenInteresting of
    boring :> interesting ->
        do
          justDone $ lengthFL boring + numSkipped
          modify $ \isc -> isc {lps = FZipper (reverseFL boring +<+ reverseFL skipped +<+ lps_done) interesting}
    where
      show_skipped o jn n ps = do putStrLn $ _nevermind_ jn ++ _these_ n ++ "."
                                  when (verbosity o == Verbose) $
                                       showskippedpatch ps
      _nevermind_ jn = "Will not ask whether to " ++ jn ++ " "
      _these_ n  = show n ++ " already decided " ++ _elem_ n ""
      _elem_ n = englishNum n (Noun "patch")
      showskippedpatch :: (Patchy p, ShowPatch p) => FL (LabelledPatch p) wY wT -> IO ()
      showskippedpatch =
                    sequence_ . mapFL (printSummary . lpPatch)

decided :: Slot -> Bool
decided InMiddle = False
decided _ = True

-- | The action bound to space, depending on the current status of the
-- patch.
getDefault :: Bool -> Slot -> Char
getDefault _ InMiddle = 'w'
getDefault True InFirst  = 'n'
getDefault True InLast   = 'y'
getDefault False InFirst = 'y'
getDefault False InLast  = 'n'

askAboutDepends :: forall p wR wU wT wY . (RepoPatch p, ApplyState p ~ Tree)
                => Repository p wR wU wT -> FL (PrimOf p) wT wY
                -> PatchSelectionOptions
                -> [PatchInfo] -> IO [PatchInfo]
askAboutDepends repository pa' ps_opts olddeps = do
  -- ideally we'd just default the olddeps to yes but still ask about them.
  -- SelectChanges doesn't currently (17/12/09) offer a way to do this so would
  -- have to have this support added first.
  pps <- readTentativeRepo repository
  pa <- n2pia `fmap` anonymous (fromPrims pa')
  -- FIXME: this code is completely unreadable
  FlippedSeal ps <- return
                      ((case pps of
                          PatchSet x _ -> FlippedSeal (reverseRL x+>+(pa:>:NilFL)))
                         :: FlippedSeal (FL (PatchInfoAnd p)) wY)
  let (pc, my_lps) = patchChoicesLps ps
      tas = case catMaybes (mapFL (\lp -> if pa `unsafeCompare` lpPatch lp || info (lpPatch lp) `elem` olddeps
                                          then Just (label lp) else Nothing) my_lps) of

                [] -> error "askAboutDepends: []"
                tgs -> tgs
  Sealed2 ps' <- return $ case getChoices (forceFirsts tas pc) of _ :> mc :> _ -> Sealed2 $ mapFL_FL lpPatch mc
  (deps:>_) <- runSelection (selectChanges ps') $
                 selectionContext FirstReversed "depend on" ps_opts { matchFlags = [], interactive = True } Nothing Nothing
  return $ olddeps `union` mapFL info deps
{-
  where
    askdep_allowed = not . patchSelectFlag
    opts' = filter askdep_allowed cfg
    psOpts = (recordPatchSelOpts cfg)
-}

{-
-- | @'patchSelectFlag' f@ holds whenever @f@ is a way of selecting
-- patches such as @PatchName n@. <- ???
patchSelectFlag :: F.DarcsFlag -> Bool
patchSelectFlag F.All = True
patchSelectFlag (F.PatchName _) = True      -- ???
patchSelectFlag (F.OnePatch _) = True
patchSelectFlag (F.OneHash _) = True
patchSelectFlag (F.SeveralPatch _) = True
patchSelectFlag (F.AfterPatch _) = True
patchSelectFlag (F.UpToPatch _) = True
patchSelectFlag (F.TagName _) = True
patchSelectFlag (F.LastN _) = True
patchSelectFlag (F.OneTag _) = True
patchSelectFlag (F.AfterTag _) = True
patchSelectFlag (F.UpToTag _) = True
patchSelectFlag (F.OnePattern _) = True
patchSelectFlag (F.SeveralPattern _) = True
patchSelectFlag (F.AfterPattern _) = True
patchSelectFlag (F.UpToPattern _) = True
patchSelectFlag _ = False
-}
