{-# LANGUAGE RankNTypes #-}
module Math.Probable
    ( 
      -- * 'Prob' type
      Prob,
      runProb,
      withGen,
      generator,

      -- * combinators for 'Prob'
      listOf,
      vectorOf,
      pairOf,
      tripleOf,

      -- * Uniformly distributed value generation ('Variate')
      module Math.Probable.Variate,

      -- * 'Distribution's
      module Math.Probable.Distributions
    ) 
    where

import Math.Probable.Distributions
import Math.Probable.Types
import Math.Probable.Variate

import Control.Applicative
import Control.Monad (replicateM)
import System.Random.MWC (withSystemRandom)

import qualified Data.Vector.Generic as G

-- | Run a computation that requires generating random
--   values, returning an 'a'
runProb :: Prob IO a -> IO a
runProb = withSystemRandom . runP

-- | Get access to the underlying generator
generator :: PrimMonad m 
          => Prob m (Gen (PrimState m))
generator = withGen return

-- | Use the given generator to generate
--   a list of 'n' values
listOf :: PrimMonad m
       => Int 
       -> Prob m a
       -> Prob m [a]
listOf n pr = replicateM n pr
{-# INLINE listOf #-}

-- | Use the given generator to generate
--   a 'G.Vector' of 'n' values
vectorOf :: (PrimMonad m, G.Vector v a)
         => Int
         -> Prob m a
         -> Prob m (v a)
vectorOf n pr = G.replicateM n pr
{-# INLINE vectorOf #-}

-- | Use the given generator to generate
--   a pair of values
pairOf :: PrimMonad m
       => Prob m a
       -> Prob m (a, a)
pairOf m = do
    x <- m
    y <- m
    return (x, y)

-- | Use the given generator to generate
--   a triple of values
tripleOf :: (PrimMonad m, Applicative m)
         => Prob m a
         -> Prob m (a, a, a)
tripleOf m = do
    x <- m
    y <- m
    z <- m
    return (x, y, z)
