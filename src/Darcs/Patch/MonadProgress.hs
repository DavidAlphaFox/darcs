{-# LANGUAGE TypeSynonymInstances #-}
--  Copyright (C) 2011 Ganesh Sittampalam
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.Patch.MonadProgress
    ( MonadProgress(..)
    , ProgressAction(..)
    , silentlyRunProgressActions
    ) where

import Prelude hiding ( catch )
import Darcs.Util.Printer ( Doc )
import Darcs.Util.Printer.Color () -- for instance Show Doc
import qualified Storage.Hashed.Monad as HSM

-- |a monadic action, annotated with a progress message that could be printed out
-- while running the action, and a message that could be printed out on error.
-- Actually printing out these messages is optional to allow non-IO monads to
-- just run the action.
data ProgressAction m a =
  ProgressAction
   {paAction :: m a
   ,paMessage :: Doc
   ,paOnError :: Doc
   }

class Monad m => MonadProgress m where
  -- |run a list of 'ProgressAction's. In some monads (typically IO-based ones),
  -- the progress and error messages will be used. In others they will be
  -- ignored and just the actions will be run.
  runProgressActions :: String -> [ProgressAction m ()] -> m ()

-- |run a list of 'ProgressAction's without any feedback messages
silentlyRunProgressActions :: Monad m => String -> [ProgressAction m ()] -> m ()
silentlyRunProgressActions _ = mapM_ paAction

instance (Functor m, Monad m) => MonadProgress (HSM.TreeMonad m) where
  runProgressActions = silentlyRunProgressActions
