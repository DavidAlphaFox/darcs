module Darcs.UI.Options.Iso where
-- * Isomorphisms

-- | Lightweight type ismomorphisms (a.k.a. invertible functions). If
-- 
-- > Iso fw bw :: Iso a b
--
-- then @fw@ and @bw@ are supposed to satisfy
--
-- prop> fw . bw = id = bw . fw
data Iso a b = Iso (a -> b) (b -> a)

-- | Lift an isomorphism between @a@ and @b@ to one between @f a@ and @f b@.
-- Like 'Functor', except we can only map invertible functions (i.e. an
-- Isomorphisms).
class IsoFunctor f where
  imap :: Iso a b -> f a -> f b

-- | Apply an iso under a functor.
under :: Functor f => Iso a b -> Iso (f a) (f b)
under (Iso fw bw) = Iso (fmap fw) (fmap bw)

-- | Apply an iso under cps (which is a cofunctor).
cps :: Iso a b -> Iso (a -> c) (b -> c)
cps (Iso fw bw) = Iso (\k -> k . bw) (\k -> k . fw)
