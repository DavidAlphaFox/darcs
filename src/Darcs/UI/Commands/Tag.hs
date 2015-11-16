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

module Darcs.UI.Commands.Tag ( tag, getTags ) where

import Prelude hiding ( (^) )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Maybe ( catMaybes )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Commands.Record ( getLog )
import Darcs.UI.Flags
    ( DarcsFlag(AskDeps), getDate, compression, verbosity, useCache, umask, getAuthor
    , hasAuthor, diffAlgorithm )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, onormalise, defaultFlags, parseFlags )
import qualified Darcs.UI.Options.All as O
import Darcs.Patch.PatchInfoAnd ( n2pia, hopefully )
import Darcs.Repository ( withRepoLock, Repository, RepoJob(..), readRepo,
                    tentativelyAddPatch, finalizeRepositoryChanges,
                  )
import Darcs.Patch
    ( infopatch, adddeps, Patchy, PrimPatch, PrimOf
    , RepoPatch
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( patchinfo, piTag )
import Darcs.Patch.Depends ( getUncovered )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.MaybeInternal ( MaybeInternal(patchInternalChecker), InternalChecker(..) )
import Darcs.Patch.Named ( patchcontents )
import Darcs.Patch.Set ( PatchSet(..), emptyPatchSet, appendPSFL, newset2FL )
import Darcs.Patch.Witnesses.Ordered ( FL(..), filterOutRLRL, (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )
import Darcs.UI.SelectChanges
    ( selectChanges
    , WhichChanges(..)
    , selectionContext
    , runSelection
    , PatchSelectionContext(allowSkipAll)
    )
import qualified Darcs.UI.SelectChanges as S
import Darcs.Repository.Util ( patchSetfMap )
import Darcs.Repository.Flags ( UpdateWorking(..), DryRun(NoDryRun) )
import Darcs.Util.Path ( AbsolutePath )

import Storage.Hashed.Tree( Tree )

import System.IO ( hPutStr, stderr )

tagDescription :: String
tagDescription = "Name the current repository state for future reference."

tagHelp :: String
tagHelp =
 "The `darcs tag` command names the current repository state, so that it\n" ++
 "can easily be referred to later.  Every *important* state should be\n" ++
 "tagged; in particular it is good practice to tag each stable release\n" ++
 "with a number or codename.  Advice on release numbering can be found\n" ++
 "at <http://producingoss.com/en/development-cycle.html>.\n" ++
 "\n" ++
 "To reproduce the state of a repository `R` as at tag `t`, use the\n" ++
 "command `darcs clone --tag t R`.  The command `darcs show tags` lists\n" ++
 "all tags in the current repository.\n" ++
 "\n" ++
 "Tagging also provides significant performance benefits: when Darcs\n" ++
 "reaches a shared tag that depends on all antecedent patches, it can\n" ++
 "simply stop processing.\n" ++
 "\n" ++
 "Like normal patches, a tag has a name, an author, a timestamp and an\n" ++
 "optional long description, but it does not change the working tree.\n" ++
 "A tag can have any name, but it is generally best to pick a naming\n" ++
 "scheme and stick to it.\n" ++
 "\n" ++
 "By default a tag names the entire repository state at the time the tag\n" ++
 "is created. If the --ask-deps option is used, the patches to include\n" ++
 "as part of the tag can be explicitly selected.\n" ++
 "\n" ++
 "The `darcs tag` command accepts the `--pipe` option, which behaves as\n" ++
 "described in `darcs record`.\n"

tagBasicOpts :: DarcsOption a
                (Maybe String
                 -> Maybe String
                 -> Bool
                 -> Maybe O.AskLongComment
                 -> Bool
                 -> Maybe String
                 -> a)
tagBasicOpts
    = O.patchname
    ^ O.author
    ^ O.pipe
    ^ O.askLongComment
    ^ O.askdeps
    ^ O.workingRepoDir

tagAdvancedOpts :: DarcsOption a (O.Compression -> O.UMask -> a)
tagAdvancedOpts = O.compress ^ O.umask

tagOpts :: DarcsOption a
           (Maybe String
            -> Maybe String
            -> Bool
            -> Maybe O.AskLongComment
            -> Bool
            -> Maybe String
            -> Maybe O.StdCmdAction
            -> Bool
            -> Bool
            -> O.Verbosity
            -> Bool
            -> O.Compression
            -> O.UMask
            -> O.UseCache
            -> Maybe String
            -> Bool
            -> Maybe String
            -> Bool
            -> a)
tagOpts = tagBasicOpts `withStdOpts` tagAdvancedOpts

tag :: DarcsCommand [DarcsFlag]
tag = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tag"
    , commandHelp = tagHelp
    , commandDescription = tagDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[TAGNAME]"]
    , commandCommand = tagCmd
    , commandPrereq = amInHashedRepository
    , commandGetArgPossibilities = return []
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc tagAdvancedOpts
    , commandBasicOptions = odesc tagBasicOpts
    , commandDefaults = defaultFlags tagOpts
    , commandCheckOptions = ocheck tagOpts
    , commandParseOptions = onormalise tagOpts
    }

filterNonInternal :: MaybeInternal p => PatchSet p wX wY -> PatchSet p wX wY
filterNonInternal =
  case patchInternalChecker of
    Nothing -> id
    Just f -> \(PatchSet ps ts) -> PatchSet (filterOutRLRL (isInternal f . patchcontents . hopefully) ps) ts

tagCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagCmd _ opts args =
  withRepoLock NoDryRun (useCache opts) YesUpdateWorking (umask opts) $ RepoJob $ \(repository :: Repository p wR wU wR) -> do
    date <- getDate (hasPipe opts)
    the_author <- getAuthor (hasAuthor opts) (hasPipe opts)
    patches <- readRepo repository
    tags <- getTags patches
    let nonInternalPatches = filterNonInternal patches
    Sealed chosenPatches <-
        if AskDeps `elem` opts
            then mapSeal (appendPSFL emptyPatchSet) <$> askAboutTagDepends opts (newset2FL nonInternalPatches)
            else return $ Sealed nonInternalPatches
    let deps = getUncovered chosenPatches
    (name, long_comment)  <- get_name_log (NilFL :: FL (PrimOf p) wA wA) args tags
    myinfo <- patchinfo date name the_author long_comment
    let mypatch = infopatch myinfo NilFL
    _ <- tentativelyAddPatch repository (compression opts) (verbosity opts) YesUpdateWorking
             $ n2pia $ adddeps mypatch deps
    finalizeRepositoryChanges repository YesUpdateWorking (compression opts)
    putStrLn $ "Finished tagging patch '"++name++"'"
  where  get_name_log ::(Patchy prim, PrimPatch prim) => FL prim wA wA -> [String] -> [String] -> IO (String, [String])
         get_name_log nilFL a tags
                          = do (name, comment, _) <- getLog
                                  (case parseFlags O.patchname opts of
                                    Nothing -> Just (unwords a)
                                    Just s -> Just s)
                                  (hasPipe opts)
                                  (parseFlags O.logfile opts)
                                  (parseFlags O.askLongComment opts)
                                  Nothing nilFL
                               when (length name < 2) $ hPutStr stderr $
                                 "Do you really want to tag '"
                                 ++name++"'? If not type: darcs obliterate --last=1\n"
                               when (name `elem` tags) $
                                  putStrLn $ "WARNING: The tag "  ++ 
                                             "\"" ++ name ++ "\"" ++
                                             " already exists."
                               return ("TAG " ++ name, comment)

getTags :: MaybeInternal p => PatchSet p wW wR -> IO [String]
getTags ps = catMaybes `fmap` patchSetfMap (return . piTag . info) ps

-- This may be useful for developers, but users don't care about
-- internals:
--
-- A tagged version automatically depends on all patches in the
-- repository.  This allows you to later reproduce precisely that
-- version.  The tag does this by depending on all patches in the
-- repository, except for those which are depended upon by other tags
-- already in the repository.  In the common case of a sequential
-- series of tags, this means that the tag depends on all patches
-- since the last tag, plus that tag itself.

askAboutTagDepends
     :: forall p wX wY . (RepoPatch p, ApplyState p ~ Tree)
     => [DarcsFlag]
     -> FL (PatchInfoAnd p) wX wY
     -> IO (Sealed (FL (PatchInfoAnd p) wX))
askAboutTagDepends flags ps = do
  let opts = S.PatchSelectionOptions
             { S.verbosity = verbosity flags
             , S.matchFlags = []
             , S.diffAlgorithm = diffAlgorithm flags
             , S.interactive = True
             , S.selectDeps = O.PromptDeps
             , S.summary = O.NoSummary
             , S.withContext = O.NoContext
             }
  (deps:>_) <- runSelection (selectChanges ps) $
                     ((selectionContext FirstReversed "depend on" opts Nothing Nothing)
                          { allowSkipAll = False })
  return $ Sealed deps

hasPipe :: [DarcsFlag] -> Bool
hasPipe = parseFlags O.pipe
