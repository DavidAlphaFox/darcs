{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}


module Darcs.Test.Patch.WithState
  where


import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
import Test.QuickCheck ( Gen, sized, choose )



----------------------------------------------------------------------
-- * WithState

data WithState s p wX wY = WithState {
                              wsStartState :: s wX
                            , wsPatch      :: p wX wY
                            , wsEndState   :: s wY
                            }
    deriving Eq

instance (Show1 s, Show2 p) => Show (WithState s p wX wY) where
  showsPrec d (WithState s p s')
    = showParen (d > appPrec) $ showString "WithState "
                              . showsPrec1 (appPrec+1) s
                              . showString " "
                              . showsPrec2 (appPrec+1) p
                              . showString " "
                              . showsPrec1 (appPrec+1) s'

instance (Show1 s, Show2 p) => Show2 (WithState s p) where
  showDict2 = ShowDictClass

data WithStartState s p wX = WithStartState {
                                 wssStartState :: s wX
                               , wssPatch      :: p wX
                               }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithStartState s p wX) where
   showsPrec d (WithStartState s p) = showParen (d > appPrec) $ showString "WithStartState " .
                                      showsPrec1 (appPrec + 1) s . showString " " .
                                      showsPrec1 (appPrec + 1) p

instance (Show1 s, Show1 p) => Show1 (WithStartState s p) where
   showDict1 = ShowDictClass

-- | A combination of a patch and its final state. The state, in this module, is
--   typically represented by a 'RepoModel' value. The @px@ type is typically a
--   patch type applied to its pre-state, e.g. @Prim x@.
data WithEndState s px wY = WithEndState {
                                wesPatch    :: px wY
                              , wesEndState :: s wY
                              }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithEndState s p wX) where
   showsPrec d (WithEndState p s) = showParen (d > appPrec) $ showString "WithEndState " .
                                    showsPrec1 (appPrec + 1) p . showString " " .
                                    showsPrec1 (appPrec + 1) s


instance (Show1 s, Show1 p) => Show1 (WithEndState s p) where
   showDict1 = ShowDictClass


----------------------------------------------------------------------
-- * ArbitraryState generators

-- | A type class to generate arbitrary values, threading a state through the
--   arbitrary calls. So this can be used to generate a patch that comes after
--   another patch. The post-state of the generated patch is hidden by the
--   'Sealed'.
class ArbitraryState s p where
  arbitraryState :: s wX -> Gen (Sealed (WithEndState s (p wX)))
  -- does a coarbitrary make sense?


instance ArbitraryState s p => ArbitraryState s (WithState s p) where
  arbitraryState s = do Sealed (WithEndState x s') <- arbitraryState s
                        return $ seal $ WithEndState (WithState s x s') s'


instance ArbitraryState s p => ArbitraryState s (p :> p) where
  arbitraryState s = do Sealed (WithEndState p1 s') <- arbitraryState s
                        Sealed (WithEndState p2 s'') <- arbitraryState s'
                        return $ seal $ WithEndState (p1 :> p2) s'' 

instance ArbitraryState s p => ArbitraryState s (p :> p :> p) where
  arbitraryState s0 = do Sealed (WithEndState p1 s1) <- arbitraryState s0
                         Sealed (WithEndState p2 s2) <- arbitraryState s1
                         Sealed (WithEndState p3 s3) <- arbitraryState s2
                         return $ seal $ WithEndState (p1 :> p2 :> p3) s3

arbitraryFL :: ArbitraryState s p => forall wX . Int -> s wX -> Gen (Sealed (WithEndState s (FL p wX)))
arbitraryFL 0 s = return $ seal $ WithEndState NilFL s
arbitraryFL n s = do Sealed (WithEndState x s') <- arbitraryState s
                     Sealed (WithEndState xs s'') <- arbitraryFL (n-1) s'
                     return $ seal $ WithEndState (x :>: xs) s''

instance ArbitraryState s p => ArbitraryState s (FL p) where
  arbitraryState s = sized $ \n -> do k <- choose (0, min 2 (n `div` 5))
                                      arbitraryFL k s


makeS2Gen :: ArbitraryState s p => Gen (s wX) -> Gen (Sealed2 p)
makeS2Gen stGen = do s <- stGen
                     Sealed (WithEndState p _) <- arbitraryState s
                     return $ seal2 p

makeSGen :: ArbitraryState s p => Gen (s wX) -> Gen (Sealed (p wX))
makeSGen stGen = do s <- stGen
                    Sealed (WithEndState p _) <- arbitraryState s
                    return $ seal p

makeWS2Gen :: ArbitraryState s p => Gen (s wX) -> Gen (Sealed2 (WithState s p))
makeWS2Gen stGen = do s <- stGen
                      Sealed (WithEndState wsP _) <- arbitraryState s
                      return $ seal2 wsP

makeWSGen :: ArbitraryState s p => Gen (s wX) -> Gen (Sealed (WithState s p wX))
makeWSGen stGen = do s <- stGen
                     Sealed (WithEndState wsP _) <- arbitraryState s
                     return $ seal wsP

instance (Show2 p, Show1 s) => Show1 ((WithState s p) wA) where
  showDict1 = ShowDictClass

