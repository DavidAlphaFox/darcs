-- Copyright (C) 2005 David Roundy
--
-- This file is licensed under the GPL, version two or later.

{-# LANGUAGE CPP #-}

module Darcs.Repository.Format
    ( RepoFormat(..)
    , RepoProperty(..)
    , identifyRepoFormat
    , tryIdentifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , writeProblem
    , readProblem
    , transferProblem
    , formatHas
    , addToFormat
    , removeFromFormat
    ) where

#include "impossible.h"

import Control.Monad ( mplus, (<=<) )
import qualified Data.ByteString.Char8 as BC ( split, unpack, elemIndex )
import qualified Data.ByteString  as B ( null, empty )
import Data.List ( partition, intercalate, (\\) )
import Data.Maybe ( isJust, mapMaybe )

import Darcs.Repository.External
    ( fetchFilePS
    , Cachable( Cachable )
    )
import Darcs.Util.Global ( darcsdir )
import Darcs.Repository.Lock ( writeBinFile )
import qualified Darcs.Repository.Flags as F ( WithWorkingDir (..) )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.Exception ( catchall, prettyException )

import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Progress ( beginTedious, endTedious, finishedOneIO )

data RepoProperty = Darcs1
                  | Darcs2
                  | HashedInventory
                  | NoWorkingDir
                  | RebaseInProgress
                  | UnknownFormat String
                  deriving ( Eq )

-- | Define string constants in one place, for reuse in show/parse functions.
darcs1Format, darcs2Format, hashedInventoryFormat :: String
noWorkingDirFormat, rebaseInProgressFormat :: String

darcs1Format = "darcs-1.0"
darcs2Format = "darcs-2"
hashedInventoryFormat = "hashed"
noWorkingDirFormat = "no-working-dir"
rebaseInProgressFormat = "rebase-in-progress"

instance Show RepoProperty where
    show Darcs1 = darcs1Format
    show Darcs2 = darcs2Format
    show HashedInventory = hashedInventoryFormat
    show NoWorkingDir = noWorkingDirFormat
    show RebaseInProgress = rebaseInProgressFormat
    show (UnknownFormat f) = "Unknown format: " ++ f

readRepoProperty :: String -> RepoProperty
readRepoProperty input
    | input == darcs1Format = Darcs1
    | input == darcs2Format = Darcs2
    | input == hashedInventoryFormat = HashedInventory
    | input == noWorkingDirFormat = NoWorkingDir
    | input == rebaseInProgressFormat = RebaseInProgress
    | otherwise = UnknownFormat input

-- | Representation of the format of a repository. Each
-- sublist corresponds to a line in the format file.
-- Currently all lines are expected to be singleton words.
newtype RepoFormat = RF [[RepoProperty]]

-- | Is a given property contained within a given format?
formatHas :: RepoProperty -> RepoFormat -> Bool
formatHas f (RF rps) = f `elem` concat rps

-- | Add a single property to an existing format.
addToFormat :: RepoProperty -> RepoFormat -> RepoFormat
addToFormat f (RF rps) = RF (rps ++ [[f]])

-- | Remove a single property from an existing format.
removeFromFormat :: RepoProperty -> RepoFormat -> RepoFormat
removeFromFormat f (RF rps) = RF (rps \\ [[f]])

instance Show RepoFormat where
    show (RF rf) = unlines $ map (intercalate "|" . map show) rf

-- | Identify the format of the repository at the
-- given location (directory, URL, or SSH path).
-- Fails if we weren't able to identify the format.
identifyRepoFormat :: String -> IO RepoFormat
identifyRepoFormat = either fail return <=< tryIdentifyRepoFormat

-- | Identify the format of the repository at the
-- given location (directory, URL, or SSH path).
-- Return @'Left' reason@ if it fails, where @reason@ explains why
-- we weren't able to identify the format. Note that we do no verification of
-- the format, which is handled by 'readProblem' or 'writeProblem' on the
-- resulting 'RepoFormat'.
tryIdentifyRepoFormat :: String -> IO (Either String RepoFormat)
tryIdentifyRepoFormat repo = do
    let k = "Identifying repository " ++ repo
    beginTedious k
    finishedOneIO k "format"
    formatInfo <- (fetchFilePS (repoPath "format") Cachable)
                  `catchall` (return B.empty)
    -- We use a workaround for servers that don't return a 404 on nonexistent
    -- files (we trivially check for something that looks like a HTML/XML tag).
    format <-
      if (B.null formatInfo || isJust (BC.elemIndex '<' formatInfo)) then do
        finishedOneIO k "inventory"
        missingInvErr <- checkFile (repoPath "inventory")
        case missingInvErr of
          Nothing -> return . Right $ RF [[Darcs1]]
          Just e -> return . Left $ makeErrorMsg e
      else return . Right $ readFormat formatInfo
    endTedious k
    return format
  where
    repoPath fileName = repo ++ "/" ++ darcsdir ++ "/" ++ fileName

    readFormat = RF . map (map (readRepoProperty . BC.unpack)) . splitFormat

    -- split into lines, then split each non-empty line on '|'
    splitFormat = map (BC.split '|') . filter (not . B.null) . linesPS

    checkFile path = (fetchFilePS path Cachable >> return Nothing)
                     `catchNonSignal`
                     (return . Just . prettyException)

    makeErrorMsg e = unlines
        [ "Not a repository: " ++ repo ++ " (" ++ e ++ ")"
        , ""
        , "HINT: Do you have the right URI for the repository?"
        ]

-- | Write the repo format to the given file.
writeRepoFormat :: RepoFormat -> FilePath -> IO ()
writeRepoFormat rf loc = writeBinFile loc $ show rf

-- | @createRepoFormat useFormat1 useNoWorkingDir@ create a repo format
createRepoFormat :: Bool -> F.WithWorkingDir -> RepoFormat
createRepoFormat useFormat1 wwd = RF $ (HashedInventory : flags2wd wwd) : flags2format
  where
    flags2format = if useFormat1 then [] else [[Darcs2]]
    flags2wd F.NoWorkingDir = [NoWorkingDir]
    flags2wd _              = []

-- | @writeProblem form@ tells if we can write to a repo in format @form@,
-- first checking if we can read that repo It returns @Nothing@ if there's no
-- problem writing to such a repository.
writeProblem :: RepoFormat -> Maybe String
writeProblem target = readProblem target `mplus` findProblems target wp
  where
    wp [] = impossible
    wp x = case partition isKnown x of
               (_, []) -> Nothing
               (_, unknowns) -> Just . unwords $
                    "Can't write repository format: " : map show unknowns

-- | @'transferProblem' source target@ returns 'Just' an error message if we
-- cannot transfer patches from a repo in format @source@ to a repo in format
-- @target@, or 'Nothing' if there are no such problem.
transferProblem :: RepoFormat -> RepoFormat -> Maybe String
transferProblem source target
    | formatHas Darcs2 source /= formatHas Darcs2 target =
        Just "Cannot mix darcs-2 repositories with older formats"
    | formatHas RebaseInProgress source =
        -- we could support this, by applying an appropriate filter to the patches
        -- as we pull them.
        Just "Cannot transfer patches from a repository where a rebase is in progress" 
    | otherwise = readProblem source `mplus` writeProblem target

-- | @'readProblem' source@ returns 'Just' an error message if we cannot read
-- from a repo in format @source@, or 'Nothing' if there's no such problem.
readProblem :: RepoFormat -> Maybe String
readProblem source
    | formatHas Darcs1 source && formatHas Darcs2 source =
        Just "Invalid repositoryformat: format 2 is incompatible with format 1"
readProblem source = findProblems source rp
  where
    rp x | any isKnown x = Nothing
    rp [] = impossible
    rp x = Just . unwords $ "Can't understand repository format:" : map show x

-- |'findProblems' applies a function that maps format-entries to an optional
-- error message, to each repoformat entry. Returning any errors.
findProblems :: RepoFormat -> ([RepoProperty] -> Maybe String) -> Maybe String
findProblems (RF ks) formatHasProblem = case mapMaybe formatHasProblem ks of
                                            [] -> Nothing
                                            xs -> Just $ unlines xs

-- | Does this version of darcs know how to handle this property?
isKnown :: RepoProperty -> Bool
isKnown p = p `elem` knownProperties
  where
    knownProperties :: [RepoProperty]
    knownProperties = [ Darcs1
                      , Darcs2
                      , HashedInventory
                      , NoWorkingDir
                      , RebaseInProgress
                      ]
