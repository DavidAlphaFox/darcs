{-# OPTIONS_GHC -fno-warn-dodgy-imports #-} -- needed for GHC 7.0/7.2
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix ( sleep ) where

import Foreign.C.Types ( CInt(..), CUInt(..), CULong(..) )

foreign import stdcall "winbase.h SleepEx" c_SleepEx :: CULong -> CUInt -> IO CInt

sleep :: Integer -> IO CInt
sleep n = c_SleepEx (1000 * fromIntegral n) 1
