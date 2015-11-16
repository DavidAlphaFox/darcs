module Darcs.UI.Options
    ( module Darcs.UI.Options.Core
    , module Darcs.UI.Options.Markdown
    , DarcsOption
    , PrimDarcsOption
    , DarcsOptDescr
    , optDescr
    ) where

import Data.Functor.Compose
import System.Console.GetOpt

import Darcs.UI.Options.All
import Darcs.UI.Options.Core
import Darcs.UI.Options.Markdown
import Darcs.UI.Options.Util ( DarcsOptDescr, PrimDarcsOption )
import Darcs.Util.Path ( AbsolutePath )

-- * Type instantiations

-- | The @instance Functor OptDescr@ was introduced only in base-4.7.0.0, which is
-- why we implement it here manually.
optDescr :: AbsolutePath -> DarcsOptDescr f -> OptDescr f
optDescr path = omap ($path) . getCompose where
  omap f (Option s l a h) = Option s l (amap f a) h
  amap f (NoArg a) = NoArg (f a)
  amap f (ReqArg mkF n) = ReqArg (fmap f mkF) n
  amap f (OptArg mkF n) = OptArg (fmap f mkF) n
