--  Copyright (C) 2004,2007 David Roundy
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

-- |
-- Copyright   : 2004, 2007 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Amend
    (
      amend
    , amendrecord
    , updatePatchHeader
    ) where

import Prelude hiding ( (^) )

import Data.Maybe ( isNothing, isJust )
import Control.Applicative ( (<$>) )
import Control.Monad ( unless, when )
import System.Exit ( exitSuccess )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , commandAlias
    , nodefaults
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Record ( getLog )
import Darcs.UI.Commands.Util ( announceFiles, testTentativeAndMaybeExit )
import Darcs.UI.Flags
    ( DarcsFlag
    , diffOpts, fixSubPaths, getEasyAuthor, promptAuthor, getDate )
import Darcs.UI.Options ( DarcsOption, (^), oparse, odesc, ocheck, defaultFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdateWorking(..), DryRun(NoDryRun) )
import Darcs.Patch ( RepoPatch, description, PrimOf, fromPrims,
                     infopatch, getdeps, adddeps, effect, invert, invertFL
                   )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( piAuthor, piName, piLog, piDateString,
                          patchinfo, isInverted, isTag, invertName,
                        )
import Darcs.Patch.Prim ( canonizeFL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully, info, patchDesc )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Util.Path ( toFilePath, SubPath(), AbsolutePath )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , tentativelyRemovePatches
    , tentativelyAddPatch
    , withManualRebaseUpdate
    , finalizeRepositoryChanges
    , invalidateIndex
    , unrecordedChangesWithPatches
    , readRecorded
    , listRegisteredFiles
    )
import Darcs.Repository.Prefs ( globalPrefsDirDoc )
import Darcs.Repository.Util ( getMovesPs, getReplaces )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContextPrim
    , runSelection
    , withSelectedPatchFromRepo
    , askAboutDepends
    )
import qualified Darcs.UI.SelectChanges as S
    ( PatchSelectionOptions(..)
    )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Prompt ( askUser )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), (+>+), nullFL, reverseRL, mapFL_FL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.Printer ( putDocLn )
import Storage.Hashed.Tree( Tree )
import Darcs.Repository.Internal ( tentativelyRemoveFromPending )


amendDescription :: String
amendDescription = "Improve a patch before it leaves your repository."


amendHelp :: String
amendHelp =
    "Amend updates a \"draft\" patch with additions or improvements,\n" ++
    "resulting in a single \"finished\" patch.\n" ++
    "\n" ++
    "By default `amend` proposes you to record additional changes.\n" ++
    "If instead you want to remove changes, use the flag `--unrecord`.\n" ++
    "\n" ++
    "When recording a draft patch, it is a good idea to start the name with\n" ++
    "`DRAFT:`. When done, remove it with `darcs amend --edit-long-comment`.\n" ++
    "Alternatively, to change the patch name without starting an editor, \n" ++
    "use the `--name`/`-m` flag:\n" ++
    "\n" ++
    "    darcs amend --match 'name \"DRAFT: foo\"' --name 'foo2'\n" ++
    "\n" ++
    "Like `darcs record`, if you call amend with files as arguments,\n" ++
    "you will only be asked about changes to those files.  So to amend a\n" ++
    "patch to foo.c with improvements in bar.c, you would run:\n" ++
    "\n" ++
    "    darcs amend --match 'touch foo.c' bar.c\n" ++
    "\n" ++
    "It is usually a bad idea to amend another developer's patch.  To make\n" ++
    "amend only ask about your own patches by default, you can add\n" ++
    "something like `amend match David Roundy` to `" ++ globalPrefsDirDoc ++
    "defaults`, \n" ++
    "where `David Roundy` is your name.\n"

amendBasicOpts :: DarcsOption a
                  (Bool
                   -> [O.MatchFlag]
                   -> O.TestChanges
                   -> Maybe Bool
                   -> Maybe String
                   -> Bool
                   -> Maybe String
                   -> Bool
                   -> Maybe O.AskLongComment
                   -> Bool
                   -> O.LookFor
                   -> Maybe String
                   -> O.WithContext
                   -> O.DiffAlgorithm
                   -> a)
amendBasicOpts
    = O.amendUnrecord
    ^ O.matchOneNontag
    ^ O.testChanges
    ^ O.interactive --True
    ^ O.author
    ^ O.selectAuthor
    ^ O.patchname
    ^ O.askdeps
    ^ O.askLongComment
    ^ O.keepDate
    ^ O.lookfor
    ^ O.workingRepoDir
    ^ O.withContext
    ^ O.diffAlgorithm

amendAdvancedOpts :: DarcsOption a
                     (O.Compression
                      -> O.UseIndex
                      -> O.UMask
                      -> O.SetScriptsExecutable
                      -> a)
amendAdvancedOpts = O.compress ^ O.useIndex ^ O.umask ^ O.setScriptsExecutable

amendOpts :: DarcsOption a
             (Bool
              -> [O.MatchFlag]
              -> O.TestChanges
              -> Maybe Bool
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> Maybe O.AskLongComment
              -> Bool
              -> O.LookFor
              -> Maybe String
              -> O.WithContext
              -> O.DiffAlgorithm
              -> Maybe O.StdCmdAction
              -> Bool
              -> Bool
              -> O.Verbosity
              -> Bool
              -> O.Compression
              -> O.UseIndex
              -> O.UMask
              -> O.SetScriptsExecutable
              -> O.UseCache
              -> Maybe String
              -> Bool
              -> Maybe String
              -> Bool
              -> a)
amendOpts = withStdOpts amendBasicOpts amendAdvancedOpts

data AmendConfig = AmendConfig
    { amendUnrecord :: Bool
    , matchFlags :: [O.MatchFlag]
    , testChanges :: O.TestChanges
    , interactive :: Maybe Bool
    , author :: Maybe String
    , selectAuthor :: Bool
    , patchname :: Maybe String
    , askDeps :: Bool
    , askLongComment :: Maybe O.AskLongComment
    , keepDate :: Bool
    , lookfor :: O.LookFor
    , _workingRepoDir :: Maybe String
    , withContext :: O.WithContext
    , diffAlgorithm :: O.DiffAlgorithm
    , verbosity :: O.Verbosity
    , compress :: O.Compression
    , useIndex :: O.UseIndex
    , umask :: O.UMask
    , sse :: O.SetScriptsExecutable
    , useCache :: O.UseCache
    }

amendConfig :: [DarcsFlag] -> AmendConfig
amendConfig =
  oparse (amendBasicOpts ^ O.verbosity ^ amendAdvancedOpts ^ O.useCache) AmendConfig

amend :: DarcsCommand AmendConfig
amend = DarcsCommand
    {
      commandProgramName          = "darcs"
    , commandName                 = "amend"
    , commandHelp                 = amendHelp
    , commandDescription          = amendDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = ["[FILE or DIRECTORY]..."]
    , commandCommand              = amendCmd
    , commandPrereq               = amInHashedRepository
    , commandGetArgPossibilities  = listRegisteredFiles
    , commandArgdefaults          = nodefaults
    , commandAdvancedOptions      = odesc amendAdvancedOpts
    , commandBasicOptions         = odesc amendBasicOpts
    , commandDefaults             = defaultFlags amendOpts
    , commandCheckOptions         = ocheck amendOpts
    , commandParseOptions         = amendConfig
    }


amendrecord :: DarcsCommand AmendConfig
amendrecord = commandAlias "amend-record" Nothing amend

amendCmd :: (AbsolutePath, AbsolutePath)
         -> AmendConfig
         -> [String]
         -> IO ()
amendCmd _   cfg [] = doAmend cfg Nothing
amendCmd fps cfg args = do
    files <- fixSubPaths fps args
    if null files
      then fail "No valid arguments were given, nothing to do."
      else doAmend cfg $ Just files

doAmend :: AmendConfig -> Maybe [SubPath] -> IO ()
doAmend cfg files =
    withRepoLock NoDryRun (useCache cfg) YesUpdateWorking (umask cfg) $
      RebaseAwareJob (compress cfg) (verbosity cfg) YesUpdateWorking $ \(repository :: Repository p wR wU wR) ->
    withSelectedPatchFromRepo "amend" repository (patchSelOpts cfg) $ \ (_ :> oldp) -> do
        announceFiles files "Amending changes in"
            -- auxiliary function needed because the witness types differ for the isTag case
        pristine <- readRecorded repository
        let go :: forall wU1 . FL (PrimOf p) wR wU1 -> IO ()
            go NilFL | not (hasEditMetadata cfg) = putStrLn "No changes!"
            go ch =
              do let context = selectionContextPrim First "record"
                                      (patchSelOpts cfg)
                                      --([All,Unified] `intersect` opts)
                                      (Just primSplitter)
                                      (map toFilePath <$> files)
                                      (Just pristine)
                 (chosenPatches :> _) <- runSelection (selectChanges ch) context
                 addChangesToPatch cfg repository oldp chosenPatches
        if not (isTag (info oldp))
              -- amending a normal patch
           then if amendUnrecord cfg
                   then do let sel = selectChanges (effect oldp)
                               context = selectionContextPrim Last "unrecord"
                                             (patchSelOpts cfg)
                                             -- ([All,Unified] `intersect` opts)
                                             (Just primSplitter)
                                             (map toFilePath <$> files)
                                             (Just pristine)
                           (_ :> chosenPrims) <- runSelection sel context
                           let invPrims = reverseRL (invertFL chosenPrims)
                           addChangesToPatch cfg repository oldp invPrims
                   else do Sealed replacePs <- if O.replaces (lookfor cfg) == O.YesLookForReplaces
                             then getReplaces (diffingOpts cfg) repository files
                             else return (Sealed NilFL)
                           movesPs <- if O.moves (lookfor cfg) == O.YesLookForMoves
                              then getMovesPs repository files
                              else return NilFL
                           go =<< unrecordedChangesWithPatches (diffingOpts cfg) repository files
                                                               movesPs (unsafeCoerceP replacePs :: FL (PrimOf p) wR wR)
              -- amending a tag
           else if hasEditMetadata cfg && isNothing files
                        -- the user is not trying to add new changes to the tag so there is
                        -- no reason to warn.
                   then go NilFL
                        -- the user is trying to add new changes to a tag.
                   else do if hasEditMetadata cfg
                                -- the user already knows that it is possible to edit tag metadata,
                                -- note that s/he is providing editing options!
                             then putStrLn "You cannot add new changes to a tag."
                                -- the user may not be aware that s/he can edit tag metadata.
                             else putStrLn "You cannot add new changes to a tag, but you are allowed to edit tag's metadata (see darcs help amend)."
                           go NilFL


addChangesToPatch :: forall p wR wU wT wX wY . (RepoPatch p, ApplyState p ~ Tree)
                  => AmendConfig
                  -> Repository p wR wU wT
                  -> PatchInfoAnd p wX wT
                  -> FL (PrimOf p) wT wY
                  -> IO ()
addChangesToPatch cfg repository oldp chs =
    if nullFL chs && not (hasEditMetadata cfg)
    then putStrLn "You don't want to record anything!"
    else do
         invalidateIndex repository
         -- If a rebase is in progress, we want to manually update the rebase state, using
         -- the amendments directly as rebase fixups. This is necessary because otherwise
         -- the normal commute rules for the rebase state will first remove the original
         -- patch then add the amended patch, and this can lead to more conflicts than using
         -- the amendment as a fixup directly. For example, if a rename operation is amended in,
         -- the rename can be propagated to any edits to the file in the rebase state, whereas
         -- a delete then add would just cause a conflict.
         --
         -- We can also signal that any explicit dependencies of the old patch should be rewritten
         -- for the new patch using a 'NameFixup'.
         (repository''', (mlogf, newp)) <- withManualRebaseUpdate (compress cfg) (verbosity cfg) YesUpdateWorking repository $ \repository' -> do

             repository'' <- tentativelyRemovePatches repository' (compress cfg) YesUpdateWorking (oldp :>: NilFL)
             (mlogf, newp) <- updatePatchHeader
                  (askDeps cfg)
                  (patchSelOpts cfg)
                  (keepDate cfg)
                  (selectAuthor cfg)
                  (author cfg)
                  (patchname cfg)
                  (askLongComment cfg)
                  repository'' oldp chs
             let fixups =
                   mapFL_FL PrimFixup (invert chs) +>+
                   NameFixup (Rename (info newp) (info oldp)) :>:
                   NilFL
             setEnvDarcsFiles newp
             repository''' <- tentativelyAddPatch repository'' (compress cfg) (verbosity cfg) YesUpdateWorking newp
             return (repository''', fixups, (mlogf, newp))
         let failmsg = maybe "" (\lf -> "\nLogfile left in "++lf++".") mlogf
         testTentativeAndMaybeExit repository''' (verbosity cfg) (testChanges cfg) (sse cfg) (isInteractive True cfg)
              ("you have a bad patch: '" ++ patchDesc newp ++ "'") "amend it"
              (Just failmsg)
         when (O.moves (lookfor cfg) == O.YesLookForMoves || O.replaces (lookfor cfg) == O.YesLookForReplaces)
           $ tentativelyRemoveFromPending repository''' YesUpdateWorking oldp
         finalizeRepositoryChanges repository''' YesUpdateWorking (compress cfg) `clarifyErrors` failmsg
         putStrLn "Finished amending patch:"
         putDocLn $ description newp
         setEnvDarcsPatches (newp :>: NilFL)


updatePatchHeader :: forall p wX wY wR wU wT . (RepoPatch p, ApplyState p ~ Tree)
                  => Bool -- askDeps
                  -> S.PatchSelectionOptions
                  -> Bool -- keepDate
                  -> Bool -- selectAuthor
                  -> Maybe String -- author
                  -> Maybe String -- patchname
                  -> Maybe O.AskLongComment
                  -> Repository p wR wU wT
                  -> PatchInfoAnd p wT wX
                  -> FL (PrimOf p) wX wY
                  -> IO (Maybe String, PatchInfoAnd p wT wY)
updatePatchHeader ask_deps pSelOpts nKeepDate nSelectAuthor nAuthor nPatchname nAskLongComment repository oldp chs = do

    let newchs = canonizeFL (S.diffAlgorithm pSelOpts) (effect oldp +>+ chs)

    let old_pdeps = getdeps $ hopefully oldp
    newdeps <- if ask_deps
               then askAboutDepends repository newchs pSelOpts old_pdeps
               else return old_pdeps

    let old_pinf = info oldp
        prior    = (piName old_pinf, piLog old_pinf)
        old_author = piAuthor old_pinf
    date <- if nKeepDate then return (piDateString old_pinf) else getDate False
    (new_author,edit_author) <- getAuthor nSelectAuthor nAuthor old_author
    warnIfHijacking old_author edit_author
    (new_name, new_log, mlogf) <- getLog 
        nPatchname False (O.Logfile Nothing False) nAskLongComment (Just prior) chs
    let maybe_invert = if isInverted old_pinf then invertName else id
    new_pinf <- maybe_invert `fmap` patchinfo date new_name
                                              new_author new_log

    let newp = n2pia (adddeps (infopatch new_pinf (fromPrims newchs)) newdeps)

    return (mlogf, newp)


warnIfHijacking :: String -- Original author
                -> Bool   -- Author change requested by options
                -> IO ()
warnIfHijacking old_author edit_author = do
    authors_here <- getEasyAuthor
    unless (edit_author || old_author `elem` authors_here) $ do
      yorn <- askUser $
          "You're not " ++ old_author ++"! Amend anyway? "
      case yorn of ('y':_) -> return ()
                   _       -> exitSuccess


hasEditMetadata :: AmendConfig -> Bool
hasEditMetadata cfg = isJust (author cfg)
                    || selectAuthor cfg
                    || isJust (patchname cfg)
                    || askLongComment cfg == Just O.YesEditLongComment
                    || askLongComment cfg == Just O.PromptLongComment
                    || askDeps cfg

-- hasEditMetadata []                    = False
-- hasEditMetadata (Author _:_)          = True
-- hasEditMetadata (SelectAuthor:_)      = True
-- hasEditMetadata (LogFile _:_)         = True -- ??? not listed as an option for amend
-- hasEditMetadata (PatchName _:_)       = True
-- hasEditMetadata (EditLongComment:_)   = True
-- hasEditMetadata (PromptLongComment:_) = True
-- hasEditMetadata (AskDeps:_)           = True
-- hasEditMetadata (_:fs)                = hasEditMetadata fs


getAuthor :: Bool -> Maybe String -> String -> IO (String,Bool)
getAuthor True _ _ = do
  a <- promptAuthor False True
  return (a,True)
getAuthor False (Just a) _ = return (a,True)
getAuthor False Nothing old = return (old,False)

-- getAuthor (SelectAuthor:_) _  = do
--   a <- promptAuthor False True
--   return (a,True)
-- getAuthor (Author a:_) _  = return (a,True)
-- getAuthor (_:as) old      = getAuthor as old
-- getAuthor [] old          = return (old,False)

patchSelOpts :: AmendConfig -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = verbosity cfg
    , S.matchFlags = matchFlags cfg
    , S.diffAlgorithm = diffAlgorithm cfg
    , S.interactive = isInteractive True cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext cfg
    }

diffingOpts :: AmendConfig -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts cfg = diffOpts (useIndex cfg) (O.adds (lookfor cfg)) False (diffAlgorithm cfg)

isInteractive :: Bool -> AmendConfig -> Bool
isInteractive def = maybe def id . interactive
