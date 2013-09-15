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
  , truncatedExp
  , gamma
  , chiSquare
  , geometric0
  , geometric1
  ) where

import Control.Monad.Primitive.Class (MonadPrim(..))

import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC.Monad



-- | Normally distributed variables with mean 0 and 1 standard deviation
standard :: MonadPrim m => Rand m Double
standard = toRand $ \g -> MWC.standard g
{-# INLINE normal #-}

-- | Normally distributed variable
normal :: MonadPrim m =>
          Double                -- ^ Mean
       -> Double                -- ^ Standard deviation
       -> Rand m Double
normal m s = toRand $ \g -> MWC.normal m s g
{-# INLINE standard #-}

-- | Generate exponentially distributed random variate. 
exponential :: MonadPrim m =>
               Double           -- ^ Scale parameter
            -> Rand m Double
exponential x = toRand $ \g -> MWC.exponential x g
{-# INLINE exponential #-}

-- | Generate truncated exponentially distributed random variate.
truncatedExp :: MonadPrim m
             => Double          -- ^ Scale parameter
             -> (Double,Double) -- ^ Range to which distribution is
                                --   truncated. Values may be negative.
             -> Rand m Double
truncatedExp s rng = toRand $ \g -> MWC.truncatedExp s rng g
{-# INLINE truncatedExp #-}

-- | Random variate generator for gamma distribution.
gamma :: MonadPrim m
      => Double                 -- ^ Shape parameter
      -> Double                 -- ^ Scale parameter
      -> Rand m Double
gamma a b = toRand $ \g -> MWC.gamma a b g
{-# INLINE gamma #-}

-- | Random variate generator for chi square distribution.
chiSquare :: MonadPrim m
          => Int                -- ^ Number of degrees of freedom
          -> Rand m Double
chiSquare n = toRand $ \g -> MWC.chiSquare n g
{-# INLINE chiSquare #-}

-- | Random variate generator for the geometric distribution,
-- computing the number of failures before success. Distribution's
-- support is [0..].
geometric0 :: MonadPrim m
           => Double        -- ^ /p/ success probability lies in (0,1]
           -> Rand m Int
geometric0 p = toRand $ \g -> MWC.geometric0 p g
{-# INLINE geometric0 #-}

-- | Random variate generator for geometric distribution for number of
-- trials. Distribution's support is [1..] (i.e. just 'geometric0' shifted by 1).
geometric1 :: MonadPrim m
           => Double -- ^ /p/ success probability lies in (0,1]
           -> Rand m Int
geometric1 p = toRand $ \g -> MWC.geometric1 p g
{-# INLINE geometric1 #-}
