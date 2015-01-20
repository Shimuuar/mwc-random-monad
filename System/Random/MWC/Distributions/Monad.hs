{-# LANGUAGE TypeFamilies #-}
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
-- Monadic wrapper for various distributions generators.
module System.Random.MWC.Distributions.Monad (
    -- * Variates: non-uniformly distributed values
    -- ** Continuous distributions
    normal
  , standard
  , exponential
  , truncatedExp
  , gamma
  , chiSquare
  , beta
      -- ** Discrete distribution
  , categorical
  , geometric0
  , geometric1
  , bernoulli
    -- ** Multivariate
  , dirichlet
    -- * Permutations
  , uniformPermutation
  , uniformShuffle
  , uniformShuffleM
  ) where

import Control.Monad.Primitive       (PrimState)
import Control.Monad.Primitive.Class (MonadPrim(..))

import Data.Vector.Generic         (Vector)
import Data.Vector.Generic.Mutable (MVector)
import Data.Traversable            (Traversable)
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

-- | Random variate generator for Beta distribution
beta :: MonadPrim m
     => Double            -- ^ alpha (>0)
     -> Double            -- ^ beta  (>0)
     -> Rand m Double
{-# INLINE beta #-}
beta a b = toRand $ \g -> MWC.beta a b g

-- | Random variate generator for Dirichlet distribution
dirichlet :: (MonadPrim m, Traversable t)
          => t Double          -- ^ container of parameters
          -> Rand m (t Double)
{-# INLINE dirichlet #-}
dirichlet t = toRand $ \g -> MWC.dirichlet t g

-- | Random variate generator for Bernoulli distribution
bernoulli :: (MonadPrim m)
          => Double            -- ^ Probability of success (returning True)
          -> Rand m Bool
{-# INLINE bernoulli #-}
bernoulli p = toRand $ \g -> MWC.bernoulli p g

-- | Random variate generator for categorical distribution.
--
--   Note that if you need to generate a lot of variates functions
--   "System.Random.MWC.CondensedTable" will offer better
--   performance.  If only few is needed this function will faster
--   since it avoids costs of setting up table.
categorical :: (MonadPrim m, Vector v Double)
            => v Double          -- ^ List of weights [>0]
            -> Rand m Int
{-# INLINE categorical #-}
categorical v = toRand $ \g -> MWC.categorical v g


-- | Random variate generator for uniformly distributed permutations.
--   It returns random permutation of vector /[0 .. n-1]/.
--
--   This is the Fisher-Yates shuffle
uniformPermutation :: (MonadPrim m, Vector v Int)
                   => Int
                   -> Rand m (v Int)
{-# INLINE uniformPermutation #-}
uniformPermutation n = toRand $ \g -> MWC.uniformPermutation n g

-- | Random variate generator for a uniformly distributed shuffle of a
--   vector.
uniformShuffle :: (MonadPrim m, Vector v a)
               => v a
               -> Rand m (v a)
{-# INLINE uniformShuffle #-}
uniformShuffle xs = toRand $ \g -> MWC.uniformShuffle xs g

-- | In-place uniformly distributed shuffle (all shuffles are
--   equiprobable) of a vector.
uniformShuffleM :: (MonadPrim m, MVector v a, PrimState m ~ PrimState (BasePrimMonad m))
                => v (PrimState m) a
                -> Rand m ()
uniformShuffleM xs = toRand $ \g -> MWC.uniformShuffleM xs g
{-# INLINE uniformShuffleM #-}
