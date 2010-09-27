module System.Random.MWC.Monad ( -- * Random monad
                                 Rand
                               , runRand
                               , runSystemRandom
                                 -- * 
                               ) where

import Control.Applicative
import Control.Monad           (ap)
import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

import qualified System.Random.MWC as MWC
import System.Random.MWC (Gen,Variate)

----------------------------------------------------------------

-- | Random monad for mwc-random package
newtype Rand m a = Rand {
  -- | Run random monad
  runRand :: Gen (PrimState m) -> m a
  }

-- | Run monad using system random
runSystemRandom :: PrimMonad m => Rand m a -> IO a
runSystemRandom (Rand rnd) = MWC.withSystemRandom rnd

instance PrimMonad m => Functor (Rand m) where
  fmap f (Rand rnd) = Rand $ \g -> (return . f) =<< (rnd g)
  {-# INLINE fmap #-}

instance PrimMonad m => Monad (Rand m) where
  return x         = Rand (\_ -> return x)
  (Rand rnd) >>= f = Rand $ \g -> (\x -> runRand (f x) g) =<< rnd g
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}

instance (PrimMonad m) => Applicative (Rand m) where
  pure  = return
  (<*>) = ap
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

----------------------------------------------------------------

-- | Convert function to Rand monad
toRand :: PrimMonad m => (Gen (PrimState m) -> m a) -> Rand m a
toRand = Rand
{-# INLINE toRand #-}

-- | Uniformly distributed values
uniform :: (PrimMonad m, Variate a) => Rand m a
uniform = Rand MWC.uniform
{-# INLINE uniform #-}

-- | Uniformly distributed values in range
uniformR :: (PrimMonad m, Variate a) => Rand m a
uniformR = Rand MWC.uniform
{-# INLINE uniform #-}

-- | Normally distributed variables with mean 0 and 1 statndard deviation
normal :: (PrimMonad m) => Rand m Double
normal = Rand MWC.normal
{-# INLINE uniform #-}
