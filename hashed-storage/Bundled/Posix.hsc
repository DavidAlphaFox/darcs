{-# LANGUAGE CPP, RankNTypes #-}

module Bundled.Posix( getFdStatus, getSymbolicLinkStatus, getFileStatus
                    , getFileStatusBS
                    , fileExists
                    , modificationTime, fileSize, FileStatus
                    , EpochTime, isDirectory, isRegularFile ) where

import qualified Data.ByteString.Char8 as BS
#if mingw32_HOST_OS
#else
import Data.ByteString.Unsafe( unsafeUseAsCString )
#endif
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.C.Error ( throwErrno, getErrno, eNOENT )
import Foreign.C.Types ( CTime, CInt )
import Foreign.Ptr ( Ptr )

import System.Posix.Internals
          ( CStat, c_fstat, sizeof_stat
          , st_mode, st_size, st_mtime, s_isdir, s_isreg )
#if mingw32_HOST_OS
import System.Posix.Internals ( c_stat, CFilePath )
#endif

import System.Posix.Types ( Fd(..), CMode, EpochTime )

#if mingw32_HOST_OS
import Foreign.C.String( withCWString, CWString )
#else
import Foreign.C.String ( withCString, CString )
#endif

#if mingw32_HOST_OS
import Data.Int ( Int64 )

type FileOffset = Int64
lstat :: CFilePath -> Ptr CStat -> IO CInt
lstat = c_stat
#else
import System.Posix.Types ( FileOffset )
import System.Posix.Internals( lstat )
#endif

#if mingw32_HOST_OS
bsToPath :: forall a. BS.ByteString -> (CWString -> IO a) -> IO a
bsToPath s f = withCWString (BS.unpack s) f
strToPath :: forall a. String -> (CWString -> IO a) -> IO a
strToPath = withCWString
#else
bsToPath :: forall a. BS.ByteString -> (CString -> IO a) -> IO a
bsToPath = unsafeUseAsCString
strToPath :: forall a. String -> (CString -> IO a) -> IO a
strToPath = withCString
#endif

data FileStatus = FileStatus {
    fst_exists :: !Bool,
    fst_mode :: !CMode,
    fst_mtime :: !CTime,
    fst_size :: !FileOffset
 }

getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  do_stat (c_fstat fd)

do_stat :: (Ptr CStat -> IO CInt) -> IO FileStatus
do_stat stat_func = do
  allocaBytes sizeof_stat $! \p -> do
     ret <- stat_func p
     if (ret == -1) then do err <- getErrno
                            if (err == eNOENT)
                               then return $! (FileStatus False 0 0 0)
                               else throwErrno "do_stat"
                    else do mode <- st_mode p
                            mtime <- st_mtime p
                            size <- st_size p
                            return $! FileStatus True mode mtime size
{-# INLINE  do_stat #-}

isDirectory :: FileStatus -> Bool
isDirectory = s_isdir . fst_mode

isRegularFile :: FileStatus -> Bool
isRegularFile = s_isreg . fst_mode

modificationTime :: FileStatus -> EpochTime
modificationTime = fst_mtime

fileSize :: FileStatus -> FileOffset
fileSize = fst_size

fileExists :: FileStatus -> Bool
fileExists = fst_exists

#include <sys/stat.h>

-- lstat is broken on win32 with at least GHC 6.10.3
getSymbolicLinkStatus :: FilePath -> IO FileStatus
##if mingw32_HOST_OS
getSymbolicLinkStatus = getFileStatus
##else
getSymbolicLinkStatus fp =
  do_stat (\p -> (fp `strToPath` (`lstat` p)))
##endif

getFileStatus :: FilePath -> IO FileStatus
getFileStatus fp =
  do_stat (\p -> (fp `strToPath` (`lstat` p)))

-- | Requires NULL-terminated bytestring -> unsafe! Use with care.
getFileStatusBS :: BS.ByteString -> IO FileStatus
getFileStatusBS fp =
  do_stat (\p -> (fp `bsToPath` (`lstat` p)))
{-# INLINE getFileStatusBS #-}
