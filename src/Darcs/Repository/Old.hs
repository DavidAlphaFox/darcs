--  Copyright (C) 2002-2005,2007-2008 David Roundy
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

{-# LANGUAGE CPP, ScopedTypeVariables #-}


module Darcs.Repository.Old ( readOldRepo,
                              revertTentativeChanges, oldRepoFailMsg ) where

import Prelude hiding ( catch )

import Darcs.Util.Progress ( debugMessage, beginTedious, endTedious, finishedOneIO )
import Darcs.Util.Path ( ioAbsoluteOrRemote, toPath )
import System.IO ( hPutStrLn, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.FilePath.Posix ( (</>) )
import Darcs.Patch.PatchInfoAnd ( Hopefully, PatchInfoAnd,
                         patchInfoAndPatch,
                         actually, unavailable )

import qualified Data.ByteString.Char8 as BC (break, pack)

import Darcs.Patch ( RepoPatch, Named,
                     readPatch )

import Darcs.Patch.Witnesses.Ordered ( RL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, unseal, mapSeal )
import Darcs.Patch.Info ( PatchInfo, makeFilename, readPatchInfos, showPatchInfo )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin )
import Darcs.Repository.External
    ( gzFetchFilePS
    , Cachable(..)
    , cloneFile
    )
import Darcs.Repository.Lock ( writeBinFile )
import Darcs.Util.Printer ( renderString, RenderMode(..) )
import Darcs.Util.Global ( darcsdir )

import Control.Exception ( catch, IOException )

#include "impossible.h"

readOldRepo :: RepoPatch p => String -> IO (SealedPatchSet p Origin)
readOldRepo d = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote d
  let k = "Reading inventory of repository "++d
  beginTedious k
  readRepoPrivate k realdir "inventory" `catch`
                        (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                                  ioError e)

readRepoPrivate :: RepoPatch p => String -> FilePath -> FilePath -> IO (SealedPatchSet p Origin)
readRepoPrivate k d iname = do
    i <- gzFetchFilePS (d </> darcsdir </> iname) Uncachable
    finishedOneIO k iname
    let parse inf = parse2 inf $ d </> darcsdir </> "patches" </> makeFilename inf
        (mt, is) = case BC.break ('\n' ==) i of
                   (swt,pistr) | swt == BC.pack "Starting with tag:" ->
                                     case readPatchInfos pistr of
                                     (t:ids) -> (Just t,reverse ids)
                                     [] -> bug "bad inventory in readRepoPrivate"
                   _ -> (Nothing, reverse $ readPatchInfos i)
    Sealed ts <- unseal seal `fmap` unsafeInterleaveIO (read_ts parse mt)
    Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
    return $ seal (PatchSet ps ts)
    where read_ts :: RepoPatch p =>
                     (forall wB . PatchInfo -> IO (Sealed (PatchInfoAnd p wB)))
                  -> Maybe PatchInfo -> IO (Sealed (RL (Tagged p) Origin))
          read_ts _ Nothing = do endTedious k
                                 return $ seal NilRL
          read_ts parse (Just tag0) =
              do debugMessage $ "Looking for inventory for:\n"++ renderString Encode (showPatchInfo tag0)
                 i <- unsafeInterleaveIO $
                      do x <- gzFetchFilePS (d </> darcsdir </> "inventories" </> makeFilename tag0) Uncachable
                         finishedOneIO k (renderString Encode (showPatchInfo tag0))
                         return x
                 let (mt, is) = case BC.break ('\n' ==) i of
                                (swt,pistr) | swt == BC.pack "Starting with tag:" ->
                                                case readPatchInfos pistr of
                                                (t:ids) -> (Just t,reverse ids)
                                                [] -> bug "bad inventory in readRepoPrivate"
                                _ -> (Nothing, reverse $ readPatchInfos i)
                 Sealed ts <- fmap (unseal seal) $ unsafeInterleaveIO $ read_ts parse mt
                 Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
                 Sealed tag00 <-  parse tag0 `catch`
                                  \(e :: IOException) ->
                                        return $ seal $
                                        patchInfoAndPatch tag0 $ unavailable $ show e
                 return $ seal $ Tagged tag00 Nothing ps :<: ts
          parse2 :: RepoPatch p => PatchInfo -> FilePath
                                -> IO (Sealed (PatchInfoAnd p wX))
          parse2 i fn = do ps <- unsafeInterleaveIO $ gzFetchFilePS fn Cachable
                           return $ patchInfoAndPatch i
                             `mapSeal` hopefullyNoParseError (toPath fn) (readPatch ps)
          hopefullyNoParseError :: String -> Maybe (Sealed (Named a1dr wX))
                                -> Sealed (Hopefully (Named a1dr) wX)
          hopefullyNoParseError _ (Just (Sealed x)) = seal $ actually x
          hopefullyNoParseError s Nothing = seal $ unavailable $ "Couldn't parse file "++s
          read_patches :: RepoPatch p =>
                          (forall wB . PatchInfo -> IO (Sealed (PatchInfoAnd p wB)))
                       -> [PatchInfo] -> IO (Sealed (RL (PatchInfoAnd p) wX))
          read_patches _ [] = return $ seal NilRL
          read_patches parse (i:is) =
              lift2Sealed (:<:)
                          (read_patches parse is)
                          (parse i `catch` \(e :: IOException) ->
                           return $ seal $ patchInfoAndPatch i $ unavailable $ show e)
          lift2Sealed :: (forall wY wZ . q wY wZ -> pp wY -> r wZ)
                      -> IO (Sealed pp) -> (forall wB . IO (Sealed (q wB))) -> IO (Sealed r)
          lift2Sealed f iox ioy = do Sealed x <- unseal seal `fmap` unsafeInterleaveIO iox
                                     Sealed y <- unseal seal `fmap` unsafeInterleaveIO ioy
                                     return $ seal $ f y x


revertTentativeChanges :: IO ()
revertTentativeChanges =
    do cloneFile (darcsdir++"/inventory") (darcsdir++"/tentative_inventory")
       writeBinFile (darcsdir++"/tentative_pristine") ""

oldRepoFailMsg :: String
oldRepoFailMsg = "ERROR: repository upgrade required, try `darcs optimize --upgrade`\n"
              ++ "See http://wiki.darcs.net/OF for more details."
