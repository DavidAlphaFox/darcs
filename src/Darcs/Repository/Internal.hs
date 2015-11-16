-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# LANGUAGE CPP, ScopedTypeVariables, Rank2Types, RankNTypes, PatternGuards #-}

module Darcs.Repository.Internal
    ( Repository(..)
    , maybeIdentifyRepository
    , identifyRepository
    , identifyRepositoryFor
    , IdentifyRepo(..)
    , findRepository
    , amInRepository
    , amNotInRepository
    , amInHashedRepository
    , revertRepositoryChanges
    , announceMergeConflicts
    , setTentativePending
    , checkUnrecordedConflicts
    , readRepo
    , readTentativeRepo
    , readRepoUsingSpecificInventory
    , prefsUrl
    , withRecorded
    , withTentative
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyRemovePatches_
    , tentativelyRemoveFromPending
    , tentativelyAddToPending
    , tentativelyAddPatch_
    , tentativelyAddPatches_
    , tentativelyReplacePatches
    , finalizeRepositoryChanges
    , unrevertUrl
    , applyToWorking
    , patchSetToPatches
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , reorderInventory
    , cleanRepository
    , setScriptsExecutable
    , setScriptsExecutablePatches
    , UpdatePristine(..)
    , MakeChanges(..)
    , applyToTentativePristine
    , makeNewPending
    , seekRepo
    ) where

import Prelude hiding ( catch )

import Darcs.Util.Printer ( putDocLn
               , (<+>)
               , text
               , ($$)
               , redText
               , putDocLnWith
               , ($$)
               )
import Darcs.Util.Printer.Color (fancyPrinters)

import Darcs.Repository.State ( readRecorded
                              , readWorking
                              , updateIndex
                              )
import Darcs.Repository.LowLevel
    ( readPending
    , readTentativePending
    , writeTentativePending
    , readNewPending
    , writeNewPending
    , pendingName
    )
import System.Exit ( exitSuccess )
import Darcs.Repository.ApplyPatches
    ( runTolerantly
    , runSilently
    , runDefault
    )

import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Repository.Format ( RepoFormat
                               , RepoProperty( HashedInventory
                                             , NoWorkingDir
                                             )
                               , tryIdentifyRepoFormat
                               , formatHas
                               , readProblem
                               , transferProblem
                               )
import System.Directory ( doesDirectoryExist
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import Control.Monad ( when
                     , unless
                     , filterM
                     , void
                     )

import Control.Applicative ( (<$>) )
import Control.Exception ( catch, IOException )

import qualified Data.ByteString as B ( readFile
                                      , isPrefixOf
                                      )
import qualified Data.ByteString.Char8 as BC (pack)
import Data.List.Ordered ( nubSort )
import Data.Maybe ( fromMaybe )
import Darcs.Patch ( Effect
                   , primIsHunk
                   , primIsBinary
                   , description
                   , tryToShrink
                   , commuteFLorComplain
                   , commute
                   , fromPrim
                   , RepoPatch
                   , Patchy
                   , merge
                   , listConflictedFiles
                   , listTouchedFiles
                   , Named
                   , commuteRL
                   , fromPrims
                   , readPatch
                   , effect
                   , invert
                   , primIsAddfile
                   , primIsAdddir
                   , primIsSetpref
                   , apply
                   , applyToTree
                   )

import Darcs.Patch.Dummy ( DummyPatch )

import Darcs.Patch.Apply ( ApplyState )

import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Prim ( PrimPatchBase
                        , PrimOf
                        , tryShrinkingInverse
                        , PrimPatch
                        )
import Darcs.Patch.Bundle ( scanBundle
                          , makeBundleN
                          )
import Darcs.Patch.Info ( isTag )
import Darcs.Patch.MaybeInternal ( flIsInternal )
import Darcs.Patch.Named ( patchcontents )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd
                                , hopefully
                                , info
                                )
import qualified Darcs.Repository.HashedRepo as HashedRepo
                            ( revertTentativeChanges
                            , finalizeTentativeChanges
                            , removeFromTentativeInventory
                            , copyPristine
                            , copyPartialsPristine
                            , applyToTentativePristine
                            , addToTentativeInventory
                            , readTentativeRepo
                            , readRepoUsingSpecificInventory
                            , cleanPristine
                            , cleanInventories
                            , cleanPatches
                            )
import qualified Darcs.Repository.Old as Old
                            ( revertTentativeChanges
                            , oldRepoFailMsg
                            )
import Darcs.Repository.Flags
    ( Compression, Verbosity(..), UseCache(..), UpdateWorking (..), AllowConflicts (..), ExternalMerge (..)
    , WorkRepo (..), WithWorkingDir (WithWorkingDir) )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , (:>)(..)
    , (+>+)
    , (+<+)
    , lengthFL
    , allFL
    , filterOutFLFL
    , reverseFL
    , mapFL_FL
    , concatFL
    , reverseRL
    , mapRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(Sealed)
    , seal
    , FlippedSeal(FlippedSeal)
    , flipSeal
    , mapSeal
    )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL
                                , removeFL
                                )
import Darcs.Patch.Set ( PatchSet(..)
                       , SealedPatchSet
                       , newset2FL
                       , newset2RL
                       , Origin
                       )
import Darcs.Patch.Depends ( removeFromPatchSet
                           , mergeThem
                           , splitOnTag
                           )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Util.Path
    ( FilePathLike
    , AbsolutePath
    , toFilePath
    , ioAbsoluteOrRemote
    , toPath
    , anchorPath
    )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Patch.Progress (progressFL)
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.Workaround
    ( getCurrentDirectory
    , renameFile
    , setExecutable
    )
import Darcs.Repository.Prefs ( getCaches )
import Darcs.Repository.Lock
    ( writeDocBinFile
    , removeFileMayNotExist
    )
import Darcs.Repository.InternalTypes( Repository(..)
                                     , Pristine(..)
                                     )
import Darcs.Util.Global ( darcsdir )

import System.Mem( performGC )

import qualified Storage.Hashed.Tree as Tree
import Storage.Hashed.Tree ( Tree )
import Darcs.Repository.PatchIndex ( createOrUpdatePatchIndexDisk, doesPatchIndexExist )
import Darcs.Repository.Read ( readRepo )
#include "impossible.h"

-- | The status of a given directory: is it a darcs repository?
data IdentifyRepo p wR wU wT = BadRepository String -- ^ looks like a repository with some error
                             | NonRepository String -- ^ safest guess
                             | GoodRepository (Repository p wR wU wT)

-- | Tries to identify the repository in a given directory
maybeIdentifyRepository :: UseCache -> String -> IO (IdentifyRepo p wR wU wT)
maybeIdentifyRepository useCache "." =
    do darcs <- doesDirectoryExist darcsdir
       if not darcs
        then return (NonRepository $ "Missing " ++ darcsdir ++ " directory")
        else do
        repoFormatOrError <- tryIdentifyRepoFormat "."
        here <- toPath `fmap` ioAbsoluteOrRemote "."
        case repoFormatOrError of
          Left err -> return $ NonRepository err
          Right rf ->
              case readProblem rf of
              Just err -> return $ BadRepository err
              Nothing -> do pris <- identifyPristine
                            cs <- getCaches useCache here
                            return $ GoodRepository $ Repo here rf pris cs
maybeIdentifyRepository useCache url' =
 do url <- toPath `fmap` ioAbsoluteOrRemote url'
    repoFormatOrError <- tryIdentifyRepoFormat url
    case repoFormatOrError of
      Left e -> return $ NonRepository e
      Right rf -> case readProblem rf of
                  Just err -> return $ BadRepository err
                  Nothing ->  do cs <- getCaches useCache url
                                 return $ GoodRepository $ Repo url rf NoPristine cs

identifyPristine :: IO Pristine
identifyPristine =
    do pristine <- doesDirectoryExist $ darcsdir++"/pristine"
       current  <- doesDirectoryExist $ darcsdir++"/current"
       hashinv  <- doesFileExist      $ darcsdir++"/hashed_inventory"
       case (pristine || current, hashinv) of
           (False, False) -> return NoPristine
           (True,  False) -> return PlainPristine
           (False, True ) -> return HashedPristine
           _ -> fail "Multiple pristine trees."

-- | identifyRepository identifies the repo at 'url'. Warning:
-- you have to know what kind of patches are found in that repo.
identifyRepository :: forall p wR wU wT. UseCache -> String
                           -> IO (Repository p wR wU wT)
identifyRepository useCache url =
    do er <- maybeIdentifyRepository useCache url
       case er of
         BadRepository s -> fail s
         NonRepository s -> fail s
         GoodRepository r -> return r

-- | @identifyRepositoryFor repo url@ identifies (and returns) the repo at 'url',
-- but fails if it is not compatible for reading from and writing to.
identifyRepositoryFor :: forall p wR wU wT vR vU vT. RepoPatch p
                      => Repository p wR wU wT
                      -> UseCache
                      -> String
                      -> IO (Repository p vR vU vT)
identifyRepositoryFor (Repo _ source _ _) useCache url =
    do Repo absurl target x c <- identifyRepository useCache url
       case transferProblem target source of
         Just e -> fail $ "Incompatibility with repository " ++ url ++ ":\n" ++ e
         Nothing -> return $ Repo absurl target x c

amInRepository :: WorkRepo -> IO (Either String ())
amInRepository (WorkRepoDir d) = do
       setCurrentDirectory d `catchall` fail ("can't set directory to "++d)
       status <- maybeIdentifyRepository YesUseCache "."
       case status of
         GoodRepository _ -> return (Right ())
         BadRepository  e -> return (Left $ "While " ++ d ++ " looks like a repository directory, we have a problem with it:\n" ++ e)
         NonRepository  _ -> return (Left "You need to be in a repository directory to run this command.")
amInRepository _ =
       fromMaybe (Left "You need to be in a repository directory to run this command.") <$> seekRepo

amInHashedRepository :: WorkRepo -> IO (Either String ())
amInHashedRepository wd
 = do inrepo <- amInRepository wd
      case inrepo of
       Right _ -> do pristine <- identifyPristine
                     case pristine of
                       HashedPristine -> return (Right ())
                       _ -> return (Left Old.oldRepoFailMsg)
       left    -> return left

-- | hunt upwards for the darcs repository
-- This keeps changing up one parent directory, testing at each
-- step if the current directory is a repository or not.  $
-- The result is:
--   Nothing, if no repository found
--   Just (Left errorMessage), if bad repository found
--   Just (Right ()), if good repository found.
-- WARNING this changes the current directory for good if matchFn succeeds
seekRepo :: IO (Maybe (Either String ()))
seekRepo = getCurrentDirectory >>= helper where
   helper startpwd = do
    status <- maybeIdentifyRepository YesUseCache "."
    case status of
      GoodRepository _ -> return . Just $ Right ()
      BadRepository e  -> return . Just $ Left e
      NonRepository _ ->
            do cd <- toFilePath `fmap` getCurrentDirectory
               setCurrentDirectory ".."
               cd' <- toFilePath `fmap` getCurrentDirectory
               if cd' /= cd
                  then helper startpwd
                  else do setCurrentDirectory startpwd
                          return Nothing

-- The performGC in this function is a workaround for a library/GHC bug,
-- http://hackage.haskell.org/trac/ghc/ticket/2924 -- (doesn't seem to be a
-- problem on fast machines, but virtual ones trip this from time to time)
amNotInRepository :: WorkRepo -> IO (Either String ())
amNotInRepository (WorkRepoDir d) = do
    createDirectoryIfMissing False d
       `catchall` (performGC >> createDirectoryIfMissing False d)
    -- note that the above could always fail
    setCurrentDirectory d
    amNotInRepository WorkRepoCurrentDir
amNotInRepository _ = do
       status <- maybeIdentifyRepository YesUseCache "."
       case status of
         GoodRepository _ -> return (Left "You may not run this command in a repository.")
         BadRepository e  -> return (Left $ "You may not run this command in a repository.\nBy the way, we have a problem with it:\n" ++ e)
         NonRepository _  -> return (Right ())

findRepository :: WorkRepo -> IO (Either String ())
findRepository (WorkRepoPossibleURL d) | isValidLocalPath d =
    do setCurrentDirectory d `catchall` fail ("can't set directory to "++d)
       findRepository WorkRepoCurrentDir
findRepository (WorkRepoDir d) =
    do setCurrentDirectory d `catchall` fail ("can't set directory to "++d)
       findRepository WorkRepoCurrentDir
findRepository _ = fromMaybe (Right ()) <$> seekRepo

-- TODO: see also Repository.State.readPendingLL ... to be removed after GHC 7.2
readNewPendingLL :: (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT -> IO (Sealed ((FL p) wT))
readNewPendingLL repo = mapSeal (mapFL_FL fromPrim) `fmap` readNewPending repo


-- | @makeNewPending repo YesUpdateWorking pendPs@ verifies that the
--   @pendPs@ could be applied to pristine if we wanted to, and if so
--   writes it to disk.  If it can't be applied, @pendPs@ must
--   be somehow buggy, so we save it for forensics and crash.
makeNewPending :: forall p wR wU wT wY. (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wT
                 -> UpdateWorking
                 -> FL (PrimOf p) wT wY
                 -> IO ()
makeNewPending _                  NoUpdateWorking _ = return ()
makeNewPending repo@(Repo r _ _ _) YesUpdateWorking origp =
    withCurrentDirectory r $
    do let newname = pendingName ++ ".new"
       debugMessage $ "Writing new pending:  " ++ newname
       Sealed sfp <- return $ siftForPending origp
       writeNewPending repo sfp
       cur <- readRecorded repo
       Sealed p <- readNewPendingLL repo -- :: IO (Sealed (FL (PrimOf p) wT))
       -- We don't ever use the resulting tree.
       _ <- catch (applyToTree p cur) $ \(err :: IOException) -> do
         let buggyname = pendingName ++ "_buggy"
         renameFile newname buggyname
         bugDoc $ text ("There was an attempt to write an invalid pending! " ++ show err)
                    $$ text "If possible, please send the contents of"
                    <+> text buggyname
                    $$ text "along with a bug report."
       renameFile newname pendingName
       debugMessage $ "Finished writing new pending:  " ++ newname

-- | @siftForPending ps@ simplifies the candidate pending patch @ps@
--   through a combination of looking for self-cancellations
--   (sequences of patches followed by their inverses), coalescing,
--   and getting rid of any hunk/binary patches we can commute out
--   the back
--
--   The visual image of sifting can be quite helpful here.  We are
--   repeatedly tapping (shrinking) the patch sequence and
--   shaking it (sift). Whatever falls out is the pending we want
--   to keep. We do this until the sequence looks about as clean as
--   we can get it
siftForPending :: forall prim wX wY . PrimPatch prim => FL prim wX wY -> Sealed (FL prim wX)
siftForPending simple_ps =
    if allFL (\p -> primIsAddfile p || primIsAdddir p) oldps
       then seal oldps
       else fromJust $ do
           Sealed x <- return $ sift NilFL $ reverseFL oldps
           return $ case tryToShrink x of
               ps | lengthFL ps < lengthFL oldps -> siftForPending ps
                  | otherwise -> seal ps
  where
    oldps = fromMaybe simple_ps $ tryShrinkingInverse $ crudeSift simple_ps
    -- get rid of any hunk/binary patches that we can commute out the
    -- back (ie. we work our way backwards, pushing the patches down
    -- to the very end and popping them off; so in (addfile f :> hunk)
    -- we can nuke the hunk, but not so in (hunk :> replace)
    sift :: FL prim wA wB -> RL prim wC wA -> Sealed (FL prim wC)
    sift sofar NilRL = seal sofar
    sift sofar (p:<:ps) | primIsHunk p || primIsBinary p =
        case commuteFLorComplain (p :> sofar) of
            Right (sofar' :> _) -> sift sofar'      ps
            Left _              -> sift (p:>:sofar) ps
    sift sofar (p:<:ps) = sift (p:>:sofar) ps

readTentativeRepo :: (RepoPatch p, ApplyState p ~ Tree)
                  => Repository p wR wU wT
                  -> IO (PatchSet p Origin wT)
readTentativeRepo repo@(Repo r rf _ _)
    | formatHas HashedInventory rf = HashedRepo.readTentativeRepo repo r
    | otherwise = fail Old.oldRepoFailMsg

readRepoUsingSpecificInventory :: (RepoPatch p, ApplyState p ~ Tree)
                               => String
                               -> Repository p wR wU wT
                               -> IO (PatchSet p Origin wT)
readRepoUsingSpecificInventory invPath repo@(Repo r rf _ _)
    | formatHas HashedInventory rf =
        HashedRepo.readRepoUsingSpecificInventory invPath repo r
    | otherwise = fail Old.oldRepoFailMsg

prefsUrl :: Repository p wR wU wT -> String
prefsUrl (Repo r _ _ _) = r ++ "/"++darcsdir++"/prefs"

unrevertUrl :: Repository p wR wU wT -> String
unrevertUrl (Repo r _ _ _) = r ++ "/"++darcsdir++"/patches/unrevert"

applyToWorking :: (ApplyState (PrimOf p) ~ Tree, RepoPatch p)
               => Repository p wR wU wT -> Verbosity -> FL (PrimOf p) wU wY
               -> IO (Repository p wR wY wT)
applyToWorking (Repo r rf t c) verb patch =
  do
    unless (formatHas NoWorkingDir rf) $
      withCurrentDirectory r $ if verb == Quiet
                               then runSilently $ apply patch
                               else runTolerantly $ apply patch
    return (Repo r rf t c)

-- | @tentativelyRemoveFromPending p@ is used by Darcs whenever it
--   adds a patch to the repository (eg. with apply or record).
--   Think of it as one part of transferring patches from pending to
--   somewhere else.
--
--   Question (Eric Kow): how do we detect patch equivalence?
tentativelyRemoveFromPending :: forall p wR wU wT wX wY. (RepoPatch p)
                 => Repository p wR wU wT
                 -> UpdateWorking
                 -> PatchInfoAnd p wX wY
                 -> IO ()
tentativelyRemoveFromPending _    NoUpdateWorking  _ = return ()
tentativelyRemoveFromPending repo YesUpdateWorking p = do
    Sealed pend <- readTentativePending repo
    -- Question (Eric Kow): why does pending being all simple matter for
    -- changepref patches in p? isSimple includes changepref, so what do
    -- adddir/etc have to do with it?  Why don't we we systematically
    -- crudeSift/not?
    let effectp = if allFL isSimple pend
                     then crudeSift $ effect p
                     else effect p
    Sealed newpend <- return $ rmpend (progressFL "Removing from pending:" effectp)
                               (unsafeCoercePStart pend)
    writeTentativePending repo (unsafeCoercePStart newpend)
  where
    -- @rmpend effect pending@ removes as much of @effect@ from @pending@
    -- as possible
    --
    -- Note that @effect@ and @pending@ must start from the same context
    -- This is not a bad thing to assume because @effect@ is a patch we want to
    -- add to the repository anyway so it'd kind of have to start from wR anyway
    --
    -- Question (Eric Kow), ok then why not
    -- @PatchInfoAnd p wR wY@ in the type signature above?
    rmpend :: FL (PrimOf p) wA wB -> FL (PrimOf p) wA wC -> Sealed (FL (PrimOf p) wB)
    rmpend NilFL x = Sealed x
    rmpend _ NilFL = Sealed NilFL
    rmpend (x:>:xs) xys | Just ys <- removeFL x xys = rmpend xs ys
    rmpend (x:>:xs) ys =
        case commuteWhatWeCanFL (x:>xs) of
            a:>x':>b -> case rmpend a ys of
                Sealed ys' -> case commute (invert (x':>:b) :> ys') of
                    Just (ys'' :> _) -> seal ys''
                    Nothing          -> seal $ invert (x':>:b)+>+ys'
                    -- DJR: I don't think this last case should be
                    -- reached, but it also shouldn't lead to corruption.

isSimple :: PrimPatch prim => prim wX wY -> Bool
isSimple x = primIsHunk x || primIsBinary x || primIsSetpref x

-- This seems to do the opposite of sifting, ie. we retain hunk/binary patches
-- but delete changepref patches.
--
-- Why not just filterOutFLFL (not . primIsSetpref)?  Is it important to only
-- have this behaviour when all other patches are either hunk or binary?
crudeSift :: forall prim wX wY . PrimPatch prim => FL prim wX wY -> FL prim wX wY
crudeSift xs = if allFL isSimple xs then filterOutFLFL ishunkbinary xs else xs
    where ishunkbinary :: prim wA wB -> EqCheck wA wB
          ishunkbinary x | primIsHunk x || primIsBinary x = unsafeCoerceP IsEq
                         | otherwise = NotEq

data HashedVsOld a = HvsO { old, hashed :: a }

decideHashedOrNormal :: Monad m => RepoFormat -> HashedVsOld (m a) -> m a
decideHashedOrNormal rf (HvsO { hashed = h, old = o })
    | formatHas HashedInventory rf = h
    | otherwise = o

data MakeChanges = MakeChanges | DontMakeChanges deriving ( Eq )

announceMergeConflicts :: (PrimPatch p, PatchInspect p)
                       => String
                       -> AllowConflicts
                       -> ExternalMerge
                       -> FL p wX wY
                       -> IO Bool
announceMergeConflicts cmd allowConflicts externalMerge resolved_pw =
  case nubSort $ listTouchedFiles resolved_pw of
    [] -> return False
    cfs -> if allowConflicts `elem` [YesAllowConflicts,YesAllowConflictsAndMark]
              || externalMerge /= NoExternalMerge
           then do putDocLnWith fancyPrinters $
                     redText "We have conflicts in the following files:" $$ text (unlines cfs)
                   return True
           else do putDocLnWith fancyPrinters $
                     redText "There are conflicts in the following files:" $$ text (unlines cfs)
                   fail $ "Refusing to "++cmd++" patches leading to conflicts.\n"++
                          "If you would rather apply the patch and mark the conflicts,\n"++
                          "use the --mark-conflicts or --allow-conflicts options to "++cmd++"\n"++
                          "These can set as defaults by adding\n"++
                          " "++cmd++" mark-conflicts\n"++
                          "to "++darcsdir++"/prefs/defaults in the target repo. "

checkUnrecordedConflicts :: forall p wT wY. RepoPatch p
                         => UpdateWorking
                         -> FL (Named p) wT wY
                         -> IO Bool
checkUnrecordedConflicts NoUpdateWorking _
 = return False -- because we are called by `darcs convert` hence we don't care
checkUnrecordedConflicts _ pc =
    do repository <- identifyRepository NoUseCache "."
       cuc repository
    where cuc :: Repository p wR wU wT -> IO Bool
          cuc r = do Sealed (mpend :: FL (PrimOf p) wT wX) <- readPending r :: IO (Sealed (FL (PrimOf p) wT))
                     case mpend of
                       NilFL -> return False
                       pend ->
                           case merge (fromPrims_ pend :\/: fromPrims_ (concatFL $ mapFL_FL effect pc)) of
                           _ :/\: pend' ->
                               case listConflictedFiles pend' of
                               [] -> return False
                               fs -> do putStrLn ("You have conflicting local changes to:\n"
                                                 ++ unwords fs)
                                        confirmed <- promptYorn "Proceed?"
                                        unless confirmed $
                                             do putStrLn "Cancelled."
                                                exitSuccess
                                        return True
          fromPrims_ :: FL (PrimOf p) wA wB -> FL p wA wB
          fromPrims_ = fromPrims

tentativelyAddPatch :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository p wR wU wT
                    -> Compression
                    -> Verbosity
                    -> UpdateWorking
                    -> PatchInfoAnd p wT wY
                    -> IO (Repository p wR wU wY)
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

data UpdatePristine = UpdatePristine 
                    | DontUpdatePristine
                    | DontUpdatePristineNorRevert deriving Eq

tentativelyAddPatches_
                     :: forall p wR wU wT wY
                      . (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository p wR wU wT
                     -> Compression
                     -> Verbosity
                     -> UpdateWorking
                     -> FL (PatchInfoAnd p) wT wY
                     -> IO (Repository p wR wU wY)
tentativelyAddPatches_ _up r _compr _verb _uw NilFL = return r
tentativelyAddPatches_ up r compr verb uw (p:>:ps) = do
    r' <- tentativelyAddPatch_ up r compr verb uw p
    tentativelyAddPatches_ up r' compr verb uw ps

-- TODO re-add a safety catch for --dry-run? Maybe using a global, like dryRun
-- :: Bool, with dryRun = unsafePerformIO $ readIORef ...
tentativelyAddPatch_ :: forall p wR wU wT wY
                      . (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository p wR wU wT
                     -> Compression
                     -> Verbosity
                     -> UpdateWorking
                     -> PatchInfoAnd p wT wY
                     -> IO (Repository p wR wU wY)

tentativelyAddPatch_ up r@(Repo dir rf t c) compr verb uw p =
    withCurrentDirectory dir $ do
       decideHashedOrNormal rf HvsO {
          hashed = void $ HashedRepo.addToTentativeInventory c compr p,
          old = fail Old.oldRepoFailMsg}
       when (up == UpdatePristine) $ do debugMessage "Applying to pristine cache..."
                                        applyToTentativePristine r verb p
                                        debugMessage "Updating pending..."
                                        tentativelyRemoveFromPending r uw p
       return (Repo dir rf t c)

applyToTentativePristine :: (ApplyState q ~ Tree, Effect q, Patchy q, ShowPatch q, PrimPatchBase q)
                         => Repository p wR wU wT
                         -> Verbosity
                         -> q wT wY
                         -> IO ()
applyToTentativePristine (Repo dir rf _ _) verb p =
    withCurrentDirectory dir $
    do when (verb == Verbose) $ putDocLn $ text "Applying to pristine..." <+> description p
       decideHashedOrNormal rf HvsO {hashed = HashedRepo.applyToTentativePristine p,
                                     old = fail Old.oldRepoFailMsg}

-- | @tentativelyAddToPending repo NoDryRun YesUpdateWorking pend ps@
--   appends @ps@ to the pending patch.
--
--   It has no effect with @NoUpdateWorking@.
--
--   This fuction is unsafe because it accepts a patch that works on the
--   tentative pending and we don't currently track the state of the
--   tentative pending.
tentativelyAddToPending :: forall p wR wU wT wX wY. RepoPatch p
                        => Repository p wR wU wT
                        -> UpdateWorking
                        -> FL (PrimOf p) wX wY
                        -> IO ()
tentativelyAddToPending _                   NoUpdateWorking  _     = return ()
tentativelyAddToPending repo@(Repo dir _ _ _) YesUpdateWorking patch =
    withCurrentDirectory dir $ do
        Sealed pend <- readTentativePending repo
        FlippedSeal newpend_ <- return $
            newpend (unsafeCoerceP pend :: FL (PrimOf p) wA wX) patch
        writeTentativePending repo (unsafeCoercePStart newpend_)
  where
    newpend :: FL prim wA wB -> FL prim wB wC -> FlippedSeal (FL prim) wC
    newpend NilFL patch_ = flipSeal patch_
    newpend p     patch_ = flipSeal $ p +>+ patch_

-- | setTentativePending is basically unsafe.  It overwrites the pending
--   state with a new one, not related to the repository state.
setTentativePending :: forall p wR wU wT wX wY. RepoPatch p
                    => Repository p wR wU wT
                    -> UpdateWorking
                    -> FL (PrimOf p) wX wY
                    -> IO ()
setTentativePending _                   NoUpdateWorking  _ = return ()
setTentativePending repo@(Repo dir _ _ _) YesUpdateWorking patch = do
    Sealed prims <- return $ siftForPending patch
    withCurrentDirectory dir $ writeTentativePending repo (unsafeCoercePStart prims)

-- | @prepend repo YesUpdateWorking ps@ prepends @ps@ to the pending patch
--   It's used right before removing @ps@ from the repo.  This ensures that
--   the pending patch can still be applied on top of the recorded state.
--
--   This function is basically unsafe.  It overwrites the pending state
--   with a new one, not related to the repository state.
prepend :: forall p wR wU wT wX wY. RepoPatch p
        => Repository p wR wU wT
        -> UpdateWorking
        -> FL (PrimOf p) wX wY
        -> IO ()
prepend _    NoUpdateWorking  _     = return ()
prepend repo YesUpdateWorking patch = do
    Sealed pend <- readTentativePending repo
    Sealed newpend_ <- return $ newpend (unsafeCoerceP pend) patch
    writeTentativePending repo (unsafeCoercePStart $ crudeSift newpend_)
  where
    newpend :: FL prim wB wC -> FL prim wA wB -> Sealed (FL prim wA)
    newpend NilFL patch_ = seal patch_
    newpend p     patch_ = seal $ patch_ +>+ p

tentativelyRemovePatches :: (RepoPatch p, ApplyState p ~ Tree)
                         => Repository p wR wU wT
                         -> Compression
                         -> UpdateWorking
                         -> FL (PatchInfoAnd p) wX wT
                         -> IO (Repository p wR wU wX)
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

tentativelyRemovePatches_ :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => UpdatePristine
                          -> Repository p wR wU wT
                          -> Compression
                          -> UpdateWorking
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO (Repository p wR wU wX)
tentativelyRemovePatches_ up repository@(Repo dir rf t c) compr uw ps =
    withCurrentDirectory dir $ do
      when (up == UpdatePristine) $ do debugMessage "Adding changes to pending..."
                                       prepend repository uw $ effect ps
      unless (up == DontUpdatePristineNorRevert) $ removeFromUnrevertContext repository ps
      debugMessage "Removing changes from tentative inventory..."
      if formatHas HashedInventory rf
        then do HashedRepo.removeFromTentativeInventory repository compr ps
                when (up == UpdatePristine) $
                     HashedRepo.applyToTentativePristine $
                     progressFL "Applying inverse to pristine" $ invert ps
        else fail Old.oldRepoFailMsg
      return (Repo dir rf t c)

-- FIXME this is a rather weird API. If called with a patch that isn't already
-- in the repo, it fails with an obscure error from 'commuteToEnd'. It also
-- ends up redoing the work that the caller has already done - if it has
-- already commuted these patches to the end, it must also know the commuted
-- versions of the other patches in the repo.
-- |Given a sequence of patches anchored at the end of the current repository,
-- actually pull them to the end of the repository by removing any patches
-- with the same name and then adding the passed in sequence.
-- Typically callers will have obtained the passed in sequence using
-- 'findCommon' and friends.
tentativelyReplacePatches :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> Compression
                          -> UpdateWorking
                          -> Verbosity
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO ()
tentativelyReplacePatches repository compr uw verb ps =
    do let ps' = filterOutFLFL (flIsInternal . patchcontents . hopefully) ps
       repository' <- tentativelyRemovePatches_ DontUpdatePristineNorRevert repository compr uw ps'
       mapAdd repository' ps'
  where mapAdd :: Repository p wM wL wI
               -> FL (PatchInfoAnd p) wI wJ
               -> IO ()
        mapAdd _ NilFL = return ()
        mapAdd r (a:>:as) =
               do r' <- tentativelyAddPatch_ DontUpdatePristine r compr verb uw a
                  mapAdd r' as

-- | Replace the pending patch with the tentative pending.
--   If @NoUpdateWorking@, this merely deletes the tentative pending
--   without replacing the current one.
--
--   Question (Eric Kow): shouldn't this also delete the tentative
--   pending if @YesUpdateWorking@?  I'm just puzzled by the seeming
--   inconsistency of the @NoUpdateWorking@ doing deletion, but
--   @YesUpdateWorking@ not bothering.
finalizePending :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository p wR wU wT
                -> UpdateWorking
                -> IO ()
finalizePending (Repo dir _ _ _) NoUpdateWorking =
        withCurrentDirectory dir $ removeFileMayNotExist pendingName
finalizePending repository@(Repo dir _ _ _) updateWorking@YesUpdateWorking =
  withCurrentDirectory dir $ do
      Sealed tpend <- readTentativePending repository
      Sealed new_pending <- return $ siftForPending tpend
      makeNewPending repository updateWorking new_pending

finalizeRepositoryChanges :: (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> UpdateWorking
                          -> Compression
                          -> IO ()
finalizeRepositoryChanges repository@(Repo dir rf _ _) updateWorking compr
    | formatHas HashedInventory rf =
        withCurrentDirectory dir $ do
            debugMessage "Finalizing changes..."
            withSignalsBlocked $ do
                 HashedRepo.finalizeTentativeChanges repository compr
                 finalizePending repository updateWorking
            debugMessage "Done finalizing changes..."
            doesPatchIndexExist dir >>= (`when` createOrUpdatePatchIndexDisk repository)
            updateIndex repository
    | otherwise = fail Old.oldRepoFailMsg

-- TODO: rename this and document the transaction protocol (revert/finalize)
-- clearly.
-- |Slightly confusingly named: as well as throwing away any tentative
-- changes, revertRepositoryChanges also re-initialises the tentative state.
-- It's therefore used before makign any changes to the repo.
revertRepositoryChanges :: RepoPatch p
                        => Repository p wR wU wT
                        -> UpdateWorking
                        -> IO ()
revertRepositoryChanges r@(Repo dir rf _ _) uw =
    withCurrentDirectory dir $
    do removeFileMayNotExist (pendingName ++ ".tentative")
       Sealed x <- readPending r
       setTentativePending r uw x
       when (uw == NoUpdateWorking) $ removeFileMayNotExist pendingName
       decideHashedOrNormal rf HvsO { hashed = HashedRepo.revertTentativeChanges,
                                      old = Old.revertTentativeChanges }

patchSetToPatches :: RepoPatch p => PatchSet p wX wY -> FL (Named p) wX wY
patchSetToPatches patchSet = mapFL_FL hopefully $ newset2FL patchSet

removeFromUnrevertContext :: forall p wR wU wT wX. (RepoPatch p, ApplyState p ~ Tree)
                          => Repository p wR wU wT
                          -> FL (PatchInfoAnd p) wX wT
                          -> IO ()
removeFromUnrevertContext repository ps = do
  Sealed bundle <- unrevert_patch_bundle `catchall` return (seal (PatchSet NilRL NilRL))
  remove_from_unrevert_context_ bundle
  where unrevert_impossible =
            do confirmed <- promptYorn "This operation will make unrevert impossible!\nProceed?"
               if confirmed then removeFileMayNotExist (unrevertUrl repository)
                            else fail "Cancelled."
        unrevert_patch_bundle :: IO (SealedPatchSet p Origin)
        unrevert_patch_bundle = do pf <- B.readFile (unrevertUrl repository)
                                   case scanBundle pf of
                                     Right foo -> return foo
                                     Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
        remove_from_unrevert_context_ :: PatchSet p Origin wZ -> IO ()
        remove_from_unrevert_context_ (PatchSet NilRL NilRL) = return ()
        remove_from_unrevert_context_ bundle =
         do debugMessage "Adjusting the context of the unrevert changes..."
            debugMessage $ "Removing "++ show (lengthFL ps) ++
                                  " patches in removeFromUnrevertContext!"
            ref <- readTentativeRepo repository
            let withSinglet :: Sealed (FL ppp wXxx)
                            -> (forall wYyy . ppp wXxx wYyy -> IO ()) -> IO ()
                withSinglet (Sealed (x :>: NilFL)) j = j x
                withSinglet _ _ = return ()
            withSinglet (mergeThem ref bundle) $ \h_us ->
                  case commuteRL (reverseFL ps :> h_us) of
                    Nothing -> unrevert_impossible
                    Just (us' :> _) ->
                      case removeFromPatchSet ps ref of
                      Nothing -> unrevert_impossible
                      Just common ->
                          do debugMessage "Have now found the new context..."
                             bundle' <- makeBundleN Nothing common (hopefully us':>:NilFL)
                             writeDocBinFile (unrevertUrl repository) bundle'
            debugMessage "Done adjusting the context of the unrevert changes!"

cleanRepository :: RepoPatch p => Repository p wR wU wT -> IO ()
cleanRepository repository@(Repo _ rf _ _) =
    decideHashedOrNormal rf
    HvsO { hashed = cleanHashedRepo repository,
           old = fail Old.oldRepoFailMsg}
           where
            cleanHashedRepo r = do
              HashedRepo.cleanPristine r
              HashedRepo.cleanInventories r
              HashedRepo.cleanPatches r


-- | grab the pristine hash of _darcs/hash_inventory, and retrieve whole pristine tree,
--   possibly writing a clean working copy in the process.
createPristineDirectoryTree :: RepoPatch p => Repository p wR wU wT -> FilePath -> WithWorkingDir -> IO ()
createPristineDirectoryTree (Repo r rf _ c) reldir wwd
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True reldir
           withCurrentDirectory reldir $ HashedRepo.copyPristine c r (darcsdir++"/hashed_inventory") wwd
    | otherwise = fail Old.oldRepoFailMsg

-- fp below really should be FileName
-- | Used by the commands dist and diff
createPartialsPristineDirectoryTree :: (FilePathLike fp, RepoPatch p)
                                    => Repository p wR wU wT
                                    -> [fp]
                                    -> FilePath
                                    -> IO ()
createPartialsPristineDirectoryTree (Repo r rf _ c) prefs dir
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True dir
           withCurrentDirectory dir $
               HashedRepo.copyPartialsPristine c r (darcsdir++"/hashed_inventory") prefs
    | otherwise = fail Old.oldRepoFailMsg

withRecorded :: RepoPatch p
             => Repository p wR wU wT
             -> ((AbsolutePath -> IO a) -> IO a)
             -> (AbsolutePath -> IO a)
             -> IO a
withRecorded repository mk_dir f
    = mk_dir $ \d -> do createPristineDirectoryTree repository (toFilePath d) WithWorkingDir
                        f d

withTentative :: forall p a wR wU wT. (RepoPatch p, ApplyState p ~ Tree)
              => Repository p wR wU wT
              -> ((AbsolutePath -> IO a) -> IO a)
              -> (AbsolutePath -> IO a)
              -> IO a
withTentative (Repo dir rf _ c) mk_dir f
    | formatHas HashedInventory rf =
        mk_dir $ \d -> do HashedRepo.copyPristine
                              c
                              dir
                              (darcsdir++"/tentative_pristine")
                              WithWorkingDir
                          f d
withTentative repository@(Repo dir _ _ _) mk_dir f =
    withRecorded repository mk_dir $ \d ->
    do Sealed ps <- read_patches (dir ++ "/"++darcsdir++"/tentative_pristine")
       runDefault $ apply ps
       f d
    where read_patches :: FilePath -> IO (Sealed (FL p wX))
          read_patches fil = do ps <- B.readFile fil
                                return $ fromMaybe (seal NilFL) $ readPatch ps

-- | Sets scripts in or below the current directory executable.
--   A script is any file that starts with the bytes '#!'.
--   This is used for --set-scripts-executable.
setScriptsExecutable_ :: PatchInspect p => Maybe (p wX wY) -> IO ()
setScriptsExecutable_ pw = do
    debugMessage "Making scripts executable"
    tree <- readWorking
    paths <- case pw of
          Just ps -> filterM doesFileExist $ listTouchedFiles ps
          Nothing -> return [ anchorPath "." p | (p, Tree.File _) <- Tree.list tree ]
    let setExecutableIfScript f =
              do contents <- B.readFile f
                 when (BC.pack "#!" `B.isPrefixOf` contents) $ do
                   debugMessage ("Making executable: " ++ f)
                   setExecutable f True
    mapM_ setExecutableIfScript paths

setScriptsExecutable :: IO ()
setScriptsExecutable = setScriptsExecutable_ (Nothing :: Maybe (FL DummyPatch wX wY))

setScriptsExecutablePatches :: PatchInspect p => p wX wY -> IO ()
setScriptsExecutablePatches = setScriptsExecutable_ . Just


-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository.
--
-- Specifically, it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
reorderInventory :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository p wR wU wR
                 -> Compression
                 -> UpdateWorking
                 -> Verbosity
                 -> IO ()
reorderInventory repository@(Repo _ rf _ _) compr uw verb =
    decideHashedOrNormal rf HvsO {
    hashed = do
        debugMessage "Reordering the inventory."
        PatchSet ps _ <- misplacedPatches `fmap` readRepo repository
        tentativelyReplacePatches repository compr uw verb $ reverseRL ps
        HashedRepo.finalizeTentativeChanges repository compr
        debugMessage "Done reordering the inventory.",
    old = fail Old.oldRepoFailMsg }

-- | Returns the patches that make the most recent tag dirty.
misplacedPatches :: forall p wS wX . RepoPatch p
                 => PatchSet p wS wX
                 -> PatchSet p wS wX
misplacedPatches ps = 
        -- Filter the repository keeping only with the tags, ordered from the
        -- most recent.
        case filter isTag $ mapRL info $ newset2RL ps of
                [] -> ps
                (lt:_) -> 
                    -- Take the most recent tag, and split the repository in,
                    -- the clean PatchSet "up to" the tag (ts), and a RL of
                    -- patches after the tag (r).
                    case splitOnTag lt ps of
                        Just (PatchSet xs ts :> r) -> PatchSet (r+<+xs) ts
                        _ -> impossible -- Because the tag is in ps.


