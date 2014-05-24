{-# LANGUAGE BangPatterns, 
             TypeSynonymInstances, 
             FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             FunctionalDependencies #-}
module Math.Probable.Random where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Primitive
import qualified Data.Vector.Generic as G
import qualified System.Random     as SR
import qualified System.Random.MWC as MWC

-- class Sample a where
--     sample :: MonadRandom m => m a

-- TODO: make 'm' be: * 'g -> (a, g)' (~ 'State g') when using StdGen
--                    * 'g -> IO a' ( 'ReaderT g IO') when using MWC
newtype RandT g m a =
    RandT { runRandT :: g -> m (a, g) }

instance Monad m => Monad (RandT g m) where
    return !x = RandT $ \gen -> return (x, gen)

    m >>= f = RandT $ \gen -> 
                do (!v, gen') <- runRandT m gen
                   runRandT (f v) gen'

instance Monad m => Functor (RandT g m) where
    fmap = liftM

instance Monad m => Applicative (RandT g m) where
    pure = return

    mf <*> mx = RandT $ \gen ->
                  do (f, gen')   <- runRandT mf gen
                     runRandT (fmap f mx) gen'

class Generator g m a | g -> m where
    sample :: RandT g m a

    sampleUniform :: (a, a) -> RandT g m a

instance (MWC.Variate a, PrimState IO ~ s) => Generator (MWC.Gen s) IO a where
    sample = RandT $ \gen -> do !v <- MWC.uniform gen
                                return (v, gen)

    sampleUniform (low, hi) =
        RandT $ \gen -> do !v <- MWC.uniformR (low, hi) gen
                           return (v, gen)

instance SR.Random a => Generator SR.StdGen Identity a where
    sample = RandT $ \gen -> do (!v, !gen') <- return $ SR.random gen
                                return (v, gen')

    sampleUniform (low, hi) =
        RandT $ \gen -> do (!v, !gen') <- return $ SR.randomR (low, hi) gen
                           return (v, gen')

stdWith :: SR.StdGen 
        -> RandT SR.StdGen Identity a
        -> a
stdWith gen r = fst . runIdentity $ runRandT r gen

stdSeed :: Int 
        -> RandT SR.StdGen Identity a
        -> a
stdSeed seed r = fst . runIdentity $ runRandT r (SR.mkStdGen seed)

stdIO :: RandT SR.StdGen Identity a 
      -> IO a
stdIO r = do
    gen <- SR.getStdGen
    return $! fst (runIdentity (runRandT r gen))

mwc :: PrimState IO ~ s
    => RandT (MWC.Gen s) IO a
    -> IO a
mwc r = MWC.withSystemRandom 
      . MWC.asGenIO $ \gen -> do
         (v, _) <- runRandT r gen
         return v

listOf :: Monad m 
       => Int
       -> RandT g m a
       -> RandT g m [a] 
listOf n r = replicateM n r

vectorOf :: Monad m 
         => G.Vector v a 
         => Int
         -> RandT g m a
         -> RandT g m (v a)
vectorOf n r = G.fromList `fmap` listOf n r
-- or maybe 'V.replicateM n r' is faster?
