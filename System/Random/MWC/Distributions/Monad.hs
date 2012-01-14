module System.Random.MWC.Distributions.Monad (
    normal
  , standard
  ) where

import Control.Monad.Primitive (PrimMonad)

import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC.Monad



-- | Normally distributed variables with mean 0 and 1 standard deviation
standard :: PrimMonad m => Rand m Double
standard = toRand MWC.standard
{-# INLINE normal #-}

-- | Normally distributed variable
normal :: PrimMonad m => 
          Double                -- Mean
       -> Double                -- Variance
       -> Rand m Double
normal m s = toRand $ MWC.normal m s
{-# INLINE standard #-}
