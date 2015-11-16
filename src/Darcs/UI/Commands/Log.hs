--  Copyright (C) 2003-2004 David Roundy
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

{-# LANGUAGE CPP, PatternGuards #-}


module Darcs.UI.Commands.Log
    ( changes, log
    , changelog, getLogInfo
    ) where

import Prelude hiding ( (^), log, catch )

import Unsafe.Coerce (unsafeCoerce)
import Data.List ( intersect, sort, nub, find )
import Data.Maybe ( fromMaybe, fromJust, isJust )
import Control.Arrow ( second )
import Control.Exception ( catch, IOException )
import Control.Monad.State.Strict
import Control.Applicative ((<$>))

import Darcs.UI.PrintPatch ( showFriendly )
import Darcs.Patch.PatchInfoAnd ( fmapFLPIAP, hopefullyM, info )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, commandAlias, findRepository )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.Flags
    ( DarcsFlag(GenContext, HumanReadable,
                MachineReadable, Count, Interactive,
                NumberPatches, XMLOutput, Summary,
                Verbose, Debug, NoPatchIndexFlag)
    , doReverse, showChangesOnlyToFiles
    , useCache, maxCount, umask
    , verbosity, isUnified, isInteractive, diffAlgorithm, hasSummary
    , fixSubPaths, getRepourl )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( SubPath(), toFilePath,
                    fp2fn, fn2fp, normPath, AbsolutePath, simpleSubPath )
import Darcs.Repository ( PatchSet, PatchInfoAnd,
                          withRepositoryDirectory, RepoJob(..),
                          readRepo, unrecordedChanges,
                          withRepoLockCanFail )
import Darcs.Repository.Flags ( UseIndex(..), ScanKnown(..), DiffAlgorithm(MyersDiff), UpdateWorking(..) )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Patch.Set ( PatchSet(..), newset2RL )
import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Info ( toXml, showPatchInfo, escapeXML, PatchInfo )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Patch.Bundle( contextPatches )
import Darcs.Patch.Prim ( PrimPatchBase )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Patch.TouchesFiles ( lookTouch )
import Darcs.Patch.Type ( PatchType(PatchType) )
import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch ( invert, xmlSummary, description,
                     effectOnFilePaths, listTouchedFiles )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(NilFL), RL(..), filterOutFLFL, filterRL,
    reverseFL, (:>)(..), mapRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), unseal2, Sealed(..), seal2 )
import Darcs.Patch.Match
    ( MatchFlag
    , firstMatch
    , secondMatch
    , matchAPatchread
    , haveNonrangeMatch
    , matchFirstPatchset
    , matchSecondPatchset
    )
import Darcs.Patch.Matchable ( Matchable )
import Darcs.Util.Printer ( Doc, simplePrinters, (<+>), prefix, text, vcat,
                 vsep, (<>), ($$), errorDoc, insertBeforeLastline, empty, RenderMode(..) )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Progress ( setProgressMode, debugMessage )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.UI.SelectChanges ( viewChanges )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Repository.PatchIndex ( PatchFilter, maybeFilterPatches, attemptCreatePatchIndex )
import Storage.Hashed.Tree( Tree )

logHelp :: String
logHelp =
 "The `darcs log` command lists the patches that constitute the\n" ++
 "current repository or, with `--repo`, a remote repository.  Without\n" ++
 "options or arguments, ALL patches will be listed.\n" ++
 "\n" ++ logHelp' ++
 "\n" ++ logHelp''

logBasicOpts :: DarcsOption a
                ([O.MatchFlag]
                 -> Maybe Int
                 -> Bool
                 -> Maybe O.ChangesFormat
                 -> Maybe O.Summary
                 -> Bool
                 -> Maybe String
                 -> Maybe String
                 -> Maybe Bool
                 -> a)
logBasicOpts
    = O.matchSeveralOrRange
    ^ O.matchMaxcount
    ^ O.onlyToFiles
    ^ O.changesFormat
    ^ O.summary
    ^ O.changesReverse
    ^ O.possiblyRemoteRepo
    ^ O.workingRepoDir
    ^ O.interactive -- False

logAdvancedOpts :: DarcsOption a (O.NetworkOptions -> O.WithPatchIndex -> a)
logAdvancedOpts = O.network ^ O.patchIndexYes

logOpts :: DarcsOption a
           ([O.MatchFlag]
            -> Maybe Int
            -> Bool
            -> Maybe O.ChangesFormat
            -> Maybe O.Summary
            -> Bool
            -> Maybe String
            -> Maybe String
            -> Maybe Bool
            -> Maybe O.StdCmdAction
            -> Bool
            -> Bool
            -> O.Verbosity
            -> Bool
            -> O.NetworkOptions
            -> O.WithPatchIndex
            -> O.UseCache
            -> Maybe String
            -> Bool
            -> Maybe String
            -> Bool
            -> a)
logOpts = logBasicOpts `withStdOpts` logAdvancedOpts

log :: DarcsCommand [DarcsFlag]
log = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "log"
    , commandHelp = logHelp
    , commandDescription = "List patches in the repository."
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandGetArgPossibilities = return []
    , commandCommand = logCmd
    , commandPrereq = findRepository
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc logAdvancedOpts
    , commandBasicOptions = odesc logBasicOpts
    , commandDefaults = defaultFlags logOpts
    , commandCheckOptions = ocheck logOpts
    , commandParseOptions = onormalise logOpts
    }

logCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
logCmd fps opts args
  | GenContext `elem` opts = if not . null $ args
      then fail "log --context cannot accept other arguments"
      else logContext opts
  | hasRemoteRepo opts = do
      (fs, es) <- remoteSubPaths args []
      if null es then
        withTempDir "darcs.log" (\_ -> showLog opts $ maybeNotNull $ nub $ sort fs)
      else
        fail $ "For a remote repo I can only handle relative paths.\n"
            ++ "Invalid arguments: "++unwords es
  | null args = showLog opts Nothing
  | otherwise = do
      fs <- fixSubPaths fps args
      case fs of
        [] -> putStrLn "No valid arguments were given, nothing to do."
        _ -> do unless (Interactive `elem` opts)
                 $ unless (NoPatchIndexFlag `elem` opts)
                  $ withRepoLockCanFail (useCache opts) YesUpdateWorking (umask opts)
                    $ RepoJob attemptCreatePatchIndex
                showLog opts $ Just $ nub $ sort fs

maybeNotNull :: [a] -> Maybe [a]
maybeNotNull [] = Nothing
maybeNotNull xs = Just xs

hasRemoteRepo :: [DarcsFlag] -> Bool
hasRemoteRepo = maybe False (not . isValidLocalPath) . parseFlags O.possiblyRemoteRepo

remoteSubPaths :: [String] -> [String] -> IO ([SubPath],[String])
remoteSubPaths [] es = return ([], es)
remoteSubPaths (arg:args) es = case simpleSubPath arg of
  Nothing -> remoteSubPaths args (arg:es)
  Just sp -> do
    (sps, es') <- remoteSubPaths args es
    return (sp:sps, es')

showLog :: [DarcsFlag] -> Maybe [SubPath] -> IO ()
showLog opts files =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryDirectory (useCache opts) repodir $ RepoJob $ \repository -> do
  unless (Debug `elem` opts) $ setProgressMode False
  Sealed unrec <- case files of
    Nothing -> return $ Sealed NilFL
    Just _ -> Sealed `fmap` unrecordedChanges (UseIndex, ScanKnown, MyersDiff) repository files
                  `catch` \(_ :: IOException) -> return (Sealed NilFL) -- this is triggered when repository is remote
  let normfp = fn2fp . normPath . fp2fn
      undoUnrecordedOnFPs = effectOnFilePaths (invert unrec)
      recFiles = map normfp . undoUnrecordedOnFPs . map toFilePath <$> files
      filtered_changes p =
          maybe_reverse <$>
          getLogInfo
              (maxCount opts)
              (parseFlags O.matchSeveralOrRange opts)
              (showChangesOnlyToFiles opts)
              recFiles
              (maybeFilterPatches repository)
              p
  debugMessage "About to read the repository..."
  patches <- readRepo repository
  debugMessage "Done reading the repository."
  if Interactive `elem` opts
    then do (fp_and_fs, _, _) <- filtered_changes patches
            let fp = map fst fp_and_fs
            viewChanges (logPatchSelOpts opts) fp
    else do let header = if isJust files && XMLOutput `notElem` opts
                          then text $ "Changes to "++unwords (fromJust recFiles)++":\n"
                          else empty
            debugMessage "About to print the patches..."
            let printers = if XMLOutput `elem` opts then simplePrinters else fancyPrinters
            ps <- readRepo repository -- read repo again to prevent holding onto
                                       -- values forced by filtered_changes
            logOutput <- changelog opts ps `fmap` filtered_changes patches
            viewDocWith printers Encode $ header $$ logOutput
  where maybe_reverse (xs,b,c) = if doReverse opts
                                 then (reverse xs, b, c)
                                 else (xs, b, c)


-- FIXME: this prose is unreadable. --twb, 2009-08
logHelp' :: String
logHelp' =
 "When given one or more files or directories as arguments, only\n" ++
 "patches which affect those files or directories are listed. This\n" ++
 "includes patches that happened to files before they were moved or\n" ++
 "renamed.\n" ++
 "\n" ++
 "When given a `--from-tag`, `--from-patch` or `--from-match`, only patches\n" ++
 "since that tag or patch are listed.  Similarly, the `--to-tag`,\n" ++
 "`--to-patch` and `--to-match` options restrict the list to older patches.\n" ++
 "\n" ++
 "The `--last` and `--max-count` options both limit the number of patches\n" ++
 "listed.  The former applies BEFORE other filters, whereas the latter\n" ++
 "applies AFTER other filters.  For example `darcs log foo.c\n" ++
 "--max-count 3` will print the last three patches that affect foo.c,\n" ++
 "whereas `darcs log --last 3 foo.c` will, of the last three\n" ++
 "patches, print only those that affect foo.c.\n"

getLogInfo :: forall p wX wY
                . (Matchable p, ApplyState p ~ Tree)
               => Maybe Int -> [MatchFlag] -> Bool
               -> Maybe [FilePath]
               -> PatchFilter p
               -> PatchSet p wX wY
               -> IO ( [(Sealed2 (PatchInfoAnd p), [FilePath])]
                     , [(FilePath, FilePath)]
                     , Maybe Doc )
getLogInfo maxCountFlag matchFlags onlyToFilesFlag plain_fs patchFilter ps =
    case (sp1s, sp2s) of
      (Sealed p1s, Sealed p2s) ->
          case findCommonWithThem p2s p1s of
            _ :> us ->
              let ps' = filterRL pf (reverseFL us) in
                case plain_fs of
                  Nothing -> return $ foldr (\x xs -> (x, []) -:- xs) ([], [], Nothing) $
                    maybe id take maxCountFlag ps'
                  Just fs -> let fs' = map (\x -> "./" ++ x) fs in do
                    filterOutUnrelatedChanges <$> do
                       ps'' <- patchFilter fs' ps'
                       return $ filterPatchesByNames maxCountFlag fs' ps''
  where
        sp1s = if firstMatch matchFlags
               then matchFirstPatchset matchFlags ps
               else Sealed $ PatchSet NilRL NilRL
        sp2s = if secondMatch matchFlags
               then matchSecondPatchset matchFlags ps
               else Sealed ps
        pf = if haveNonrangeMatch (PatchType :: PatchType p) matchFlags
             then matchAPatchread matchFlags
             else \_ -> True

        filterOutUnrelatedChanges (pfs, renames, doc)
          | onlyToFilesFlag = (map onlyRelated pfs, renames, doc)
          | otherwise       = (pfs, renames, doc)

        onlyRelated (Sealed2 p, fs) =
          (Sealed2 $ fmapFLPIAP (filterOutFLFL (unrelated fs)) p, fs)

        unrelated fs p
          -- If the change does not affect the patches we are looking at,
          -- we ignore the difference between the two states.
          | null $ fs `intersect` listTouchedFiles p = unsafeCoerce IsEq
          | otherwise                                = NotEq

-- | Take a list of filenames and patches and produce a list of patches that
-- actually touch the given files with a list of touched file names, a list of
-- original-to-current filepath mappings, indicating the original names of the
-- affected files and possibly an error. Additionaly, the function takes a
-- "depth limit" -- maxcount, that could be Nothing (return everything) or
-- "Just n" -- returns at most n patches touching the file (starting from the
-- beginning of the patch list).
filterPatchesByNames
    :: forall p
     . (Matchable p, ApplyState p ~ Tree)
    => Maybe Int -- ^ maxcount
    -> [FilePath] -- ^ filenames
    -> [Sealed2 (PatchInfoAnd p)] -- ^ patchlist
    -> ([(Sealed2 (PatchInfoAnd p),[FilePath])], [(FilePath, FilePath)], Maybe Doc)
filterPatchesByNames maxcount fns patches = removeNonRenames $
    evalState (filterPatchesByNames' fns patches) (maxcount, initRenames) where
        removeNonRenames (ps, renames, doc) = (ps, removeIds renames, doc)
        removeIds = filter $ uncurry (/=)
        initRenames = map (\x -> (x, x)) fns
        returnFinal = (\renames -> ([], renames, Nothing)) <$> gets snd
        filterPatchesByNames' [] _ = returnFinal
        filterPatchesByNames' _ [] = returnFinal
        filterPatchesByNames' fs (s2hp@(Sealed2 hp) : ps) = do
            (count, renames) <- get
            let stopNow = case count of
                                Nothing -> False
                                Just c -> c <= 0
            if stopNow
                then returnFinal
                else case hopefullyM hp of
                    Nothing -> do
                        let err = text "Can't find patches prior to:"
                                  $$ showPatchInfo (info hp)
                        return ([], renames, Just err)
                    Just p ->
                        case lookTouch (Just renames) fs (invert p) of
                            (True, affected, [], renames') ->
                                return ([(s2hp, affected)], renames', Nothing)
                            (True, affected, fs', renames') -> do
                                let sub1Mb c = subtract 1 <$> c
                                modify $ \(c, _) -> (sub1Mb c, renames')
                                rest <- filterPatchesByNames' fs' ps
                                return $ (s2hp, affected) -:- rest
                            (False, _, fs', renames') -> do
                                modify $ second (const renames')
                                filterPatchesByNames' fs' ps

-- | Note, lazy pattern matching is required to make functions like
-- filterPatchesByNames lazy in case you are only not interested in
-- the first element. E.g.:
--
--   let (fs, _, _) = filterPatchesByNames ...
(-:-) :: a -> ([a],b,c) -> ([a],b,c)
x -:- ~(xs,y,z) = (x:xs,y,z)

changelog :: forall p wStart wX
           . ( Apply p, ApplyState p ~ Tree, ShowPatch p, IsHunk p
             , PrimPatchBase p, PatchListFormat p
             , Conflict p, CommuteNoConflicts p
             )
          => [DarcsFlag] -> PatchSet p wStart wX
          -> ([(Sealed2 (PatchInfoAnd p), [FilePath])], [(FilePath, FilePath)], Maybe Doc)
          -> Doc
changelog opts patchset (pis_and_fs, createdAsFs, mbErr)
    | Count `elem` opts = text $ show $ length pis_and_fs
    | MachineReadable `elem` opts =
        maybe (vsep $ map (unseal2 (showPatchInfo.info)) pis) errorDoc mbErr
    | XMLOutput `elem` opts =
         text "<changelog>"
      $$ vcat created_as_xml
      $$ vcat actual_xml_changes
      $$ text "</changelog>"
    | Summary `elem` opts || Verbose `elem`  opts =
        mbAppendErr $ vsep (map (number_patch change_with_summary) pis)
    | otherwise = mbAppendErr $ vsep (map (number_patch description') pis)
    where mbAppendErr = maybe id (\err -> ($$ err)) mbErr
          change_with_summary :: Sealed2 (PatchInfoAnd p) -> Doc
          change_with_summary (Sealed2 hp)
              | Just p <- hopefullyM hp = showFriendly (verbosity opts) (hasSummary O.NoSummary opts) p
              | otherwise = description hp
                            $$ indent (text "[this patch is unavailable]")

          xml_with_summary (Sealed2 hp)
              | Just p <- hopefullyM hp = insertBeforeLastline
                                           (toXml $ info hp) (indent $ xmlSummary p)
          xml_with_summary (Sealed2 hp) = toXml (info hp)
          indent = prefix "    "
          actual_xml_changes = if Summary `elem` opts
                               then map xml_with_summary pis
                               else map (toXml . unseal2 info) pis

          created_as_xml = map create createdAsFs where
            create rename@(_, as) = createdAsXml (first_change_of as) rename
            -- We need to reorder the patches when they haven't been reversed
            -- already, so that we find the *first* patch that modifies a given
            -- file, not the last (by default, the list is oldest->newest).
            reorderer = if not (doReverse opts) then reverse else id
            oldest_first_pis_and_fs = reorderer pis_and_fs
            couldnt_find fn = error $ "Couldn't find first patch affecting " ++
                                      fn ++ " in pis_and_fs"
            mb_first_change_of fn = find ((fn `elem`) . snd) oldest_first_pis_and_fs
            find_first_change_of fn = fromMaybe (couldnt_find fn)
              (mb_first_change_of fn)
            first_change_of = unseal2 info . fst . find_first_change_of
          number_patch f x = if NumberPatches `elem` opts
                             then case get_number x of
                                  Just n -> text (show n++":") <+> f x
                                  Nothing -> f x
                             else f x
          get_number :: Sealed2 (PatchInfoAnd p) -> Maybe Int
          get_number (Sealed2 y) = gn 1 (newset2RL patchset)
              where iy = info y
                    gn :: Int -> RL (PatchInfoAnd p) wStart wY -> Maybe Int
                    gn n (b:<:bs) | seq n (info b) == iy = Just n
                                  | otherwise = gn (n+1) bs
                    gn _ NilRL = Nothing
          pis = map fst pis_and_fs
          description' = unseal2 description

-- FIXME: this prose is unreadable. --twb, 2009-08
logHelp'' :: String
logHelp'' =
 "Three output formats exist.  The default is `--human-readable`.  You can\n" ++
 "also select `--context`, which is the internal format (as seen in patch\n" ++
 "bundles) that can be re-read by Darcs (e.g. `darcs clone --context`).\n" ++
 "\n" ++
 "Finally, there is `--xml-output`, which emits valid XML... unless a the\n" ++
 "patch metadata (author, name or description) contains a non-ASCII\n" ++
 "character and was recorded in a non-UTF8 locale.\n" ++
 "\n" ++
 -- FIXME: can't we just disallow the following usage?
 "Note that while the `--context` flag may be used in conjunction with\n" ++
 "`--xml-output` or `--human-readable`, in neither case will darcs clone be\n" ++
 "able to read the output.  On the other hand, sufficient information\n" ++
 "WILL be output for a knowledgeable human to recreate the current state\n" ++
 "of the repository.\n"

logContext :: [DarcsFlag] -> IO ()
logContext opts = do
  let repodir = fromMaybe "." $ getRepourl opts
  withRepositoryDirectory (useCache opts) repodir $ RepoJob $ \repository -> do
      (_ :> ps') <- contextPatches `fmap` readRepo repository
      let ps = mapRL (\p -> (seal2 p, [])) ps'
      let header = if fancy then empty else text "\nContext:\n"
      let logOutput = changelog opts' emptyset (ps, [], Nothing)
      viewDocWith simplePrinters Encode $ header $$ logOutput
        where opts' = if fancy then opts else MachineReadable : opts
              fancy = HumanReadable `elem` opts || XMLOutput `elem` opts
              emptyset = PatchSet NilRL NilRL

-- | changes is an alias for log
changes :: DarcsCommand [DarcsFlag]
changes = commandAlias "changes" Nothing log

createdAsXml :: PatchInfo -> (String, String) -> Doc
createdAsXml pinfo (current, createdAs) =
    text "<created_as current_name='"
       <> escapeXML current
       <> text "' original_name='"
       <> escapeXML createdAs
       <> text "'>"
    $$    toXml pinfo
    $$    text "</created_as>"

logPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
logPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity flags
    , S.matchFlags = parseFlags O.matchSeveralOrRange flags
    , S.diffAlgorithm = diffAlgorithm flags
    , S.interactive = isInteractive False flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.summary = hasSummary O.NoSummary flags
    , S.withContext = isUnified flags
    }
