-- Copyright (C) 2006 Tommy Pettersson <ptp@lysator.liu.se>
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


module Darcs.UI.CommandsAux
    ( checkPaths
    , maliciousPatches
    , hasMaliciousPath
    ) where

import Control.Monad ( when )

import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( parseFlags )
import Darcs.UI.Options.All ( restrictPaths )
import Darcs.Patch.Inspect ( PatchInspect, listTouchedFiles )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), unseal2 )
import Darcs.Util.Path ( isMaliciousPath )

-- * File paths
{-
  Darcs will operate on files and directories with the invoking user's
  privileges. The paths for these files and directories are stored in
  patches, which darcs receives in various ways. Even though darcs will not
  create patches with "unexpected" file paths, there are no such guarantees
  for received patches. A spoofed patch could inflict changes on any file
  or directory which the invoking user is privileged to modify.

  There is no one single "apply" function that can check paths, so each
  command is responsible for not applying patches without first checking
  them with one of these function when appropriate.
-}

{- |
  A convenience function to call from all darcs command functions before
  applying any patches. It checks for malicious paths in patches, and
  prints an error message and fails if it finds one.
-}
checkPaths :: PatchInspect p => [DarcsFlag] -> FL p wX wY -> IO ()
checkPaths opts patches
  = when (parseFlags restrictPaths opts && or (mapFL hasMaliciousPath patches)) $
        fail $ unlines $ ["Malicious path in patch:"] ++
                         map ("    " ++) (concat $ mapFL maliciousPaths patches) ++
                         ["", "If you are sure this is ok then you can run again with the --dont-restrict-paths option."]
           -- TODO: print patch(es)
           -- NOTE: should use safe Doc printer, this can be evil chars

-- | Filter out patches that contains some malicious file path
maliciousPatches :: PatchInspect p => [Sealed2 p] -> [Sealed2 p]
maliciousPatches = filter (unseal2 hasMaliciousPath)

hasMaliciousPath :: PatchInspect p => p wX wY -> Bool
hasMaliciousPath patch =
    case maliciousPaths patch of
      [] -> False
      _ -> True

maliciousPaths :: PatchInspect p => p wX wY -> [String]
maliciousPaths patch =
  let paths = listTouchedFiles patch in
    filter isMaliciousPath paths
