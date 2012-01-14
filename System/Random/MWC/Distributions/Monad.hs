-- |
-- Module    : System.Random.MWC.Monad
-- Copyright : (c) 2010-2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : alexey.skladnoy@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Monadic wrapper for various distributions generators.
module System.Random.MWC.Distributions.Monad (
    normal
  , standard
  , exponential
  , gamma
  , chiSquare
  ) where

import Control.Monad.Primitive (PrimMonad)

import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC.Monad



-- | Normally distributed variables with mean 0 and 1 standard deviation
standard :: PrimMonad m => Rand m Double
standard = toRand $ \g -> MWC.standard g
{-# INLINE normal #-}

-- | Normally distributed variable
normal :: PrimMonad m => 
          Double                -- ^ Mean
       -> Double                -- ^ Standard deviation
       -> Rand m Double
normal m s = toRand $ \g -> MWC.normal m s g
{-# INLINE standard #-}

-- | Generate exponentially distributed random variate. 
exponential :: PrimMonad m =>
               Double           -- ^ Scale parameter
            -> Rand m Double
exponential x = toRand $ \g -> MWC.exponential x g
{-# INLINE exponential #-}

-- | Random variate generator for gamma distribution.
gamma :: PrimMonad m
      => Double                 -- ^ Shape parameter
      -> Double                 -- ^ Scale parameter
      -> Rand m Double
gamma a b = toRand $ \g -> MWC.gamma a b g
{-# INLINE gamma #-}

-- | Random variate generator for chi square distribution.
chiSquare :: PrimMonad m
          => Int                -- ^ Number of degrees of freedom
          -> Rand m Double
chiSquare n = toRand $ \g -> MWC.chiSquare n g
{-# INLINE chiSquare #-}
