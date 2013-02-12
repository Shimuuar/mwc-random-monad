{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : System.Random.MWC.Monad
-- Copyright : (c) 2010-2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : alexey.skladnoy@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provide monadic interface for @mwc-random@
-- package. It's just a thin wrapper and all work is done
-- @mwc-random@.
module System.Random.MWC.Monad ( 
    -- * Random monad
    Rand
  , toRand
    -- ** Type syhnonims
  , RandIO
  , asRandIO
  , RandST
  , asRandST
    -- ** Running monad
  , runRand
  , runWithCreate
  , runWithSeed
  , runWithVector
  , runWithSystemRandom
    -- * Random numbers generation
  , uniform
  , uniformR
    -- * Seed management
  , Seed
  , toSeed
  , fromSeed
  , save
  ) where

import Control.Applicative
import Control.Monad             (ap)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class    (MonadIO(..))
import Control.Monad.ST          (ST)
import Control.Monad.Primitive   (PrimMonad, PrimState)

import Data.Word               (Word32)
import qualified Data.Vector.Generic as G

import qualified System.Random.MWC as MWC
import           System.Random.MWC   (Gen,Variate,Seed,toSeed,fromSeed)

----------------------------------------------------------------

-- | Random monad for mwc-random package
newtype Rand m a = Rand {
  -- | Run random monad
  runRand :: Gen (PrimState m) -> m a
  }

instance (Monad m) => Functor (Rand m) where
  fmap f (Rand rnd) = Rand $ \g -> (return . f) =<< (rnd g)
  {-# INLINE fmap #-}

instance (Monad m) => Monad (Rand m) where
  return           = Rand . const . return
  (Rand rnd) >>= f = Rand $ \g -> (\x -> runRand (f x) g) =<< rnd g
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}

instance (Monad m) => Applicative (Rand m) where
  pure  = return
  (<*>) = ap
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance MonadTrans Rand where
  lift = Rand . const
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Rand m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}



-- | Type synonim for ST-based Rand monad
type RandST s a = Rand (ST s) a

-- | Fix type of monad to ST
asRandST :: RandST s a -> RandST s a
asRandST = id
{-# INLINE asRandST #-}

-- | Type synonim for IO-based Rand monad
type RandIO a   = Rand IO a

-- | Fix type of monad to IO
asRandIO :: RandIO a -> RandIO a
asRandIO = id
{-# INLINE asRandIO #-}



----------------------------------------------------------------

-- | Run monad using fixed seed
runWithCreate :: PrimMonad m => Rand m a -> m a
runWithCreate m = runRand m =<< MWC.create
{-# INLINE runWithCreate #-}

-- | By creating seed from vector of values
runWithVector :: (G.Vector v Word32, PrimMonad m) => Rand m a -> v Word32 -> m a
runWithVector m v = runRand m =<< MWC.initialize v
{-# INLINE runWithVector #-}

-- | Run monad using seed
runWithSeed :: PrimMonad m => Seed -> Rand m a -> m a
runWithSeed seed m = runRand m =<< MWC.restore seed
{-# INLINE runWithSeed #-}

-- | Run monad using system random
runWithSystemRandom :: PrimMonad m => Rand m a -> IO a
runWithSystemRandom = MWC.withSystemRandom . runRand
{-# INLINE runWithSystemRandom #-}



----------------------------------------------------------------

-- | Convert function to Rand monad
toRand :: PrimMonad m => (Gen (PrimState m) -> m a) -> Rand m a
toRand = Rand
{-# INLINE toRand #-}

-- | Uniformly distributed values
uniform :: (PrimMonad m, Variate a) => Rand m a
uniform = Rand $ \g -> MWC.uniform g
{-# INLINE uniform #-}

-- | Uniformly distributed values in range
uniformR :: (PrimMonad m, Variate a) => (a,a) -> Rand m a
uniformR rng = Rand $ \g -> MWC.uniformR rng g
{-# INLINE uniformR #-}



----------------------------------------------------------------

-- | Save current seed for future reuse
save :: PrimMonad m => Rand m Seed
save = Rand MWC.save
