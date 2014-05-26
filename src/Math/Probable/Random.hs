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
import Data.Int
import Data.Word
import qualified Data.Vector.Generic as G
import qualified System.Random     as SR
import qualified System.Random.MWC as MWC


-- TODO? make 'm' be: * 'g -> (a, g)' (~ 'State g') when using StdGen
--                    * 'g -> IO a' ( 'ReaderT g IO') when using MWC
-- to avoid passing around MWC's gen
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

class Monad m => Generator g m a | g -> m where
    sample :: RandT g m a

    sampleUniform :: (a, a) -> RandT g m a

int :: Generator g m Int 
    => RandT g m Int
int = sample

int8 :: Generator g m Int8
     => RandT g m Int8
int8 = sample

int16 :: Generator g m Int16
      => RandT g m Int16
int16 = sample

int32 :: Generator g m Int32
      => RandT g m Int32
int32 = sample

int64 :: Generator g m Int64
      => RandT g m Int64
int64 = sample

word :: Generator g m Word
     => RandT g m Word
word = sample

word8 :: Generator g m Word8
      => RandT g m Word8
word8 = sample

word16 :: Generator g m Word16
       => RandT g m Word16
word16 = sample

word32 :: Generator g m Word32
       => RandT g m Word32
word32 = sample

word64 :: Generator g m Word64
       => RandT g m Word64
word64 = sample

float :: Generator g m Float
      => RandT g m Float
float = sample

double :: Generator g m Double
       => RandT g m Double
double = sample

integer :: Generator g m Integer
        => RandT g m Integer
integer = sample

char :: Generator g m Char
     => RandT g m Char
char = sample

bool :: Generator g m Bool
     => RandT g m Bool
bool = sample

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

vectorOf :: ( Monad m 
            , G.Vector v a )
         => Int
         -> RandT g m a
         -> RandT g m (v a)
vectorOf n r = G.fromList `fmap` listOf n r
-- or maybe 'V.replicateM n r' is faster?

purely :: RandT g Identity a
       -> g
       -> a
purely r gen = fst . runIdentity $ runRandT r gen

purelyWith :: RandT g Identity a
           -> (s -> g)
           -> s
           -> a
purelyWith r seedToGen seed =
    fst . runIdentity $ runRandT r (seedToGen seed)

impurely :: RandT g IO a
         -> IO g
         -> IO a
impurely r getGen = do
    gen <- getGen
    (v, _) <- runRandT r gen
    return $! v