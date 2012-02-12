-- |
-- Module    : System.Random.MWC.CondesedTable
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : alexey.skladnoy@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Monadic wrapper for 'System.Random.MWC.CondesedTable'.
module System.Random.MWC.CondensedTable.Monad (
    -- * Condensed tables
    MWC.CondensedTable
  , MWC.CondensedTableV
  , MWC.CondensedTableU
  , genFromTable
    -- * Constructors for tables
  , MWC.tableFromProbabilities
  , MWC.tableFromWeights
  , MWC.tableFromIntWeights
    -- ** Disrete distributions
  , MWC.tablePoisson
  , MWC.tableBinomial
  ) where

import Control.Monad.Primitive         (PrimMonad)
import qualified Data.Vector.Generic as G
import System.Random.MWC.Monad
import qualified System.Random.MWC.CondensedTable as MWC


genFromTable :: (PrimMonad m, G.Vector v a) => MWC.CondensedTable v a -> Rand m a
genFromTable tbl = toRand $ \g -> MWC.genFromTable tbl g
{-# INLINE genFromTable #-}
