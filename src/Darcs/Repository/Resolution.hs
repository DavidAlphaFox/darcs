--  Copyright (C) 2003,2005 David Roundy
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


module Darcs.Repository.Resolution
    ( standardResolution
    , externalResolution
    , patchsetConflictResolutions
    ) where

import System.FilePath.Posix ( (</>) )
import System.Exit ( ExitCode( ExitSuccess ) )
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Data.List ( zip4 )
import Control.Monad ( when )

import Darcs.Repository.Diff( treeDiff )
import Darcs.Patch ( PrimOf, PrimPatch, RepoPatch, resolveConflicts,
                     effectOnFilePaths, patchcontents,
                     invert, listConflictedFiles, commute, applyToTree, fromPrim )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Prim ( PrimPatchBase )
import Darcs.Util.Path ( toFilePath, filterFilePaths )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..), (+>+),
    mapFL_FL, concatFL, reverseRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft )

import Darcs.Util.CommandLine ( parseCmd )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Util.Prompt ( askEnter )
import Darcs.Patch.Set ( PatchSet(..), Origin )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Util.Exec ( exec, Redirect(..) )
import Darcs.Repository.Lock ( withTempDir )
import Darcs.Repository.External ( cloneTree )
import Darcs.Repository.Flags ( WantGuiPause(..), DiffAlgorithm(..) )

import qualified Storage.Hashed.Tree as Tree
import Storage.Hashed ( writePlainTree, readPlainTree )

--import Darcs.Util.Printer.Color ( traceDoc )
--import Darcs.Util.Printer ( greenText, ($$), Doc )
--import Darcs.Patch ( showPatch )

standardResolution :: (PrimPatchBase p, Conflict p, CommuteNoConflicts p)
                   => FL p wX wY -> Sealed (FL (PrimOf p) wY)
standardResolution p = mergeList $ map head $ resolveConflicts p

mergeList :: forall prim wX . PrimPatch prim => [Sealed (FL prim wX)] -> Sealed (FL prim wX)
mergeList = doml NilFL
    where doml :: FL prim wX wY -> [Sealed (FL prim wX)] -> Sealed (FL prim wX)
          doml mp (Sealed p:ps) =
              case commute (invert p :> mp) of
              Just (mp' :> _) -> doml (p +>+ mp') ps
              Nothing -> doml mp ps -- This shouldn't happen for "good" resolutions.
          doml mp [] = Sealed mp

externalResolution :: forall p wX wY wZ wA. (RepoPatch p, ApplyState p ~ Tree.Tree)
                   => DiffAlgorithm
                   -> Tree.Tree IO
                   -> String  -- ^ external merge tool command
                   -> WantGuiPause -- ^ tell whether we want GUI pause
                   -> FL (PrimOf p) wX wY
                   -> FL (PrimOf p) wX wZ
                   -> FL p wY wA
                   -> IO (Sealed (FL (PrimOf p) wA))
externalResolution diffa s1 c wantGuiPause p1_prim p2_prim pmerged = do
 -- TODO: remove the following two once we can rely on GHC 7.2 / superclass equality
 let p1 :: FL p wX wY = mapFL_FL fromPrim p1_prim
     p2 :: FL p wX wZ = mapFL_FL fromPrim p2_prim
 sa <- applyToTree (invert p1) s1
 sm <- applyToTree pmerged s1
 s2 <- applyToTree p2 sa
 let nms = listConflictedFiles pmerged
     nas = effectOnFilePaths (invert pmerged) nms
     n1s = effectOnFilePaths p1 nas
     n2s = effectOnFilePaths p2 nas
     ns = zip4 nas n1s n2s nms
     write_files tree fs = writePlainTree (Tree.filter (filterFilePaths fs) tree) "."
  in do
   former_dir <- getCurrentDirectory
   withTempDir "version1" $ \absd1 -> do
     let d1 = toFilePath absd1
     write_files s1 n1s
     setCurrentDirectory former_dir
     withTempDir "ancestor" $ \absda -> do
       let da = toFilePath absda
       write_files sa nas
       setCurrentDirectory former_dir
       withTempDir "merged" $ \absdm -> do
         let dm = toFilePath absdm
         write_files sm nms
         setCurrentDirectory former_dir
         withTempDir "cleanmerged" $ \absdc -> do
           let dc = toFilePath absdc
           cloneTree dm "."
           setCurrentDirectory former_dir
           withTempDir "version2" $ \absd2 -> do
             let d2 = toFilePath absd2
             write_files s2 n2s
             mapM_ (externallyResolveFile c wantGuiPause da d1 d2 dm) ns
             sc <- readPlainTree dc
             sfixed <- readPlainTree dm
             ftf <- filetypeFunction
             unFreeLeft `fmap` treeDiff diffa ftf sc sfixed

externallyResolveFile :: String -- ^ external merge tool command
                      -> WantGuiPause -- ^ tell whether we want GUI pause
                      -> String -- ^ path to merge base
                      -> String -- ^ path to side 1 of the merge
                      -> String -- ^ path to side 2 of the merge
                      -> String -- ^ path where resolved content should go
                      -> (FilePath, FilePath, FilePath, FilePath)
                      -> IO ()
externallyResolveFile c wantGuiPause da d1 d2 dm (fa, f1, f2, fm) = do
    putStrLn $ "Merging file "++fm++" by hand."
    ec <- run c [('1', d1</>f1), ('2', d2</>f2), ('a', da</>fa), ('o', dm</>fm), ('%', "%")]
    when (ec /= ExitSuccess) $
         putStrLn $ "External merge command exited with " ++ show ec
    when (wantGuiPause == YesWantGuiPause) $
        askEnter "Hit return to move on, ^C to abort the whole operation..."

run :: String -> [(Char,String)] -> IO ExitCode
run c replacements =
    case parseCmd replacements c of
    Left err     -> fail $ show err
    Right (c2,_) -> rr c2
    where rr (command:args) = do putStrLn $ "Running command '" ++
                                            unwords (command:args) ++ "'"
                                 exec command args (Null,Null,Null)
          rr [] = return ExitSuccess

patchsetConflictResolutions :: RepoPatch p => PatchSet p Origin wX -> Sealed (FL (PrimOf p) wX)
patchsetConflictResolutions (PatchSet NilRL _) = Sealed NilFL
patchsetConflictResolutions (PatchSet xs _)
    = --traceDoc (greenText "looking at resolutions" $$
      --         (sh $ resolveConflicts $ joinPatches $
      --              mapFL_FL (patchcontents . hopefully) $ reverseRL xs )) $
      mergeList $ map head $ resolveConflicts $ concatFL $
      mapFL_FL (patchcontents . hopefully) $ reverseRL xs
    --where sh :: [[Sealed (FL Prim)]] -> Doc
    --      sh [] = greenText "no more conflicts"
    --      sh (x:ps) = greenText "one conflict" $$ sh1 x $$ sh ps
    --      sh1 :: [Sealed (FL Prim)] -> Doc
    --      sh1 [] = greenText "end of unravellings"
    --      sh1 (Sealed x:ps) = greenText "one unravelling:" $$ showPatch x $$
    --                          sh1 ps
