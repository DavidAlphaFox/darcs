{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Mostly internal utilities for use by the rest of the library. Subject to
-- removal without further notice.
module Storage.Hashed.Utils where

import Prelude hiding ( lookup, catch )
import System.Mem( performGC )
import Bundled.Posix( getFileStatus, fileSize )
import System.Directory( getCurrentDirectory, setCurrentDirectory )
import System.FilePath( (</>), isAbsolute )
import Data.Int( Int64 )
import Data.Maybe( catMaybes )
import Control.Exception( catch, bracket, SomeException(..) )
import Control.Monad( when )
import Control.Monad.Identity( runIdentity )
import Control.Applicative( (<$>) )

import Foreign.ForeignPtr( withForeignPtr )
import Foreign.Ptr( plusPtr )
import Data.ByteString.Internal( toForeignPtr, memcpy )
import System.IO (withFile, IOMode(ReadMode), hSeek, SeekMode(AbsoluteSeek))
import Data.Bits( Bits )
#ifdef BIGENDIAN
import Data.Bits( (.&.), (.|.), shift, shiftL, rotateR )
#endif

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS

import qualified Data.Set as S
import qualified Data.Map as M

-- | Pointer to a filesystem, possibly with start/end offsets. Supposed to be
-- fed to (uncurry mmapFileByteString) or similar.
type FileSegment = (FilePath, Maybe (Int64, Int))

-- | Read in a FileSegment into a Lazy ByteString. Implemented using mmap.
readSegment :: FileSegment -> IO BL.ByteString
readSegment (f,range) = do
    bs <- tryToRead
       `catch` (\(_::SomeException) -> do
                     size <- fileSize `fmap` getFileStatus f
                     if size == 0
                        then return BS8.empty
                        else performGC >> tryToRead)
    return $ BL.fromChunks [bs]
  where
    tryToRead = do 
        case range of
            Nothing -> BS.readFile f
            Just (off, size) -> withFile f ReadMode $ \h -> do
                hSeek h AbsoluteSeek $ fromIntegral off
                BS.hGet h size 
{-# INLINE readSegment #-}

-- | Run an IO action with @path@ as a working directory. Does neccessary
-- bracketing.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory name =
    bracket
        (do cwd <- getCurrentDirectory
            when (name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd
                     `catch` \(_::SomeException) -> return ())
        . const

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p = do
  cwd <- getCurrentDirectory
  return $! if isAbsolute p then p else cwd </> p

-- Wow, unsafe.
unsafePokeBS :: BS8.ByteString -> BS8.ByteString -> IO ()
unsafePokeBS to from =
    do let (fp_to, off_to, len_to) = toForeignPtr to
           (fp_from, off_from, len_from) = toForeignPtr from
       when (len_to /= len_from) $ fail $ "Length mismatch in unsafePokeBS: from = "
            ++ show len_from ++ " /= to = " ++ show len_to
       withForeignPtr fp_from $ \p_from ->
         withForeignPtr fp_to $ \p_to ->
           memcpy (plusPtr p_to off_to)
                  (plusPtr p_from off_from)
                  (fromIntegral len_to)

align :: Integral a => a -> a -> a
align boundary i = case i `rem` boundary of
                     0 -> i
                     x -> i + boundary - x
{-# INLINE align #-}

xlate32 :: (Num a, Bits a) => a -> a
xlate64 :: (Num a, Bits a) => a -> a

#ifdef LITTLEENDIAN
xlate32 = id
xlate64 = id
#endif

#ifdef BIGENDIAN
bytemask :: (Num a, Bits a) => a
bytemask = 255

xlate32 a = ((a .&. (bytemask `shift`  0)) `shiftL` 24) .|.
            ((a .&. (bytemask `shift`  8)) `shiftL`  8) .|.
            ((a .&. (bytemask `shift` 16)) `rotateR`  8) .|.
            ((a .&. (bytemask `shift` 24)) `rotateR` 24)

xlate64 a = ((a .&. (bytemask `shift`  0)) `shiftL` 56) .|.
            ((a .&. (bytemask `shift`  8)) `shiftL` 40) .|.
            ((a .&. (bytemask `shift` 16)) `shiftL` 24) .|.
            ((a .&. (bytemask `shift` 24)) `shiftL`  8) .|.
            ((a .&. (bytemask `shift` 32)) `rotateR`  8) .|.
            ((a .&. (bytemask `shift` 40)) `rotateR` 24) .|.
            ((a .&. (bytemask `shift` 48)) `rotateR` 40) .|.
            ((a .&. (bytemask `shift` 56)) `rotateR` 56)
#endif

-- | Find a monadic fixed point of @f@ that is the least above @i@. (Will
-- happily diverge if there is none.)
mfixFrom :: (Eq a, Functor m, Monad m) => (a -> m a) -> a -> m a
mfixFrom f i = do x <- f i
                  if x == i then return i
                            else mfixFrom f x

-- | Find a fixed point of @f@ that is the least above @i@. (Will happily
-- diverge if there is none.)
fixFrom :: (Eq a) => (a -> a) -> a -> a
fixFrom f i = runIdentity $ mfixFrom (return . f) i

-- | For a @refs@ function, a @map@ (@key@ -> @value@) and a @rootSet@, find a
-- submap of @map@ such that all items in @map@ are reachable, through @refs@
-- from @rootSet@.
reachable :: forall monad key value. (Functor monad, Monad monad, Ord key, Eq value) =>
              (value -> monad [key])
           -> (key -> monad (Maybe (key, value)))
           -> S.Set key -> monad (M.Map key value)
reachable refs lookup rootSet =
    do lookupSet rootSet >>= mfixFrom expand
    where lookupSet :: S.Set key -> monad (M.Map key value)
          expand :: M.Map key value -> monad (M.Map key value)

          lookupSet s = do list <- mapM lookup (S.toAscList s)
                           return $ M.fromAscList (catMaybes list)
          expand from = do refd <- concat <$> mapM refs (M.elems from)
                           M.union from <$> lookupSet (S.fromList refd)
