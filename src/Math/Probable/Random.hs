{-# LANGUAGE BangPatterns,
             TypeFamilies #-}
module Math.Probable.Random where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Int
import Data.Word
import qualified Data.Vector.Generic as G
import qualified System.Random.MWC as MWC


-- TODO? make 'm' be: * 'g -> (a, g)' (~ 'State g') when using StdGen
--                    * 'g -> IO a' ( 'ReaderT g IO') when using MWC
-- to avoid passing around MWC's gen
newtype RandT m a =
    RandT { runRandT :: MWC.Gen (PrimState m) -> m a }

instance Monad m => Monad (RandT m) where
    return x = RandT $ \_ -> return x
    {-# INLINE return #-}

    (RandT g) >>= f = 
        RandT $ \gen -> do
            !v <- g gen
            !res <- runRandT (f v) gen
            return res
    {-# INLINE (>>=) #-}

instance Monad m => Functor (RandT m) where
    fmap f r = RandT $ \gen -> return . f =<< runRandT r gen
    {-# INLINE fmap #-}

instance Monad m => Applicative (RandT m) where
    pure = return
    {-# INLINE pure #-}

    (<*>) = ap
    {-# INLINE (<*>) #-}

int :: PrimMonad m => RandT m Int
int = RandT MWC.uniform
{-# INLINE int #-}

int8 :: PrimMonad m => RandT m Int8
int8 = RandT MWC.uniform
{-# INLINE int8 #-}

int16 :: PrimMonad m => RandT m Int16
int16 = RandT MWC.uniform
{-# INLINE int16 #-}

int32 :: PrimMonad m => RandT m Int32
int32 = RandT MWC.uniform
{-# INLINE int32 #-}

int64 :: PrimMonad m => RandT m Int64
int64 = RandT MWC.uniform
{-# INLINE int64 #-}

word :: PrimMonad m => RandT m Word
word = RandT MWC.uniform
{-# INLINE word #-}

word8 :: PrimMonad m => RandT m Word8
word8 = RandT MWC.uniform
{-# INLINE word8 #-}

word16 :: PrimMonad m => RandT m Word16
word16 = RandT MWC.uniform
{-# INLINE word16 #-}

word32 :: PrimMonad m => RandT m Word32
word32 = RandT MWC.uniform
{-# INLINE word32 #-}

word64 :: PrimMonad m => RandT m Word64
word64 = RandT MWC.uniform
{-# INLINE word64 #-}

float :: PrimMonad m => RandT m Float
float = RandT MWC.uniform
{-# INLINE float #-}

double :: PrimMonad m => RandT m Double
double = RandT MWC.uniform
{-# INLINE double #-}

bool :: PrimMonad m => RandT m Bool
bool = RandT MWC.uniform
{-# INLINE bool #-}

uniformIn :: (MWC.Variate a, PrimMonad m) => (a, a) -> RandT m a
uniformIn (a, b) = RandT $ MWC.uniformR (a, b)

mwc :: RandT IO a
    -> IO a
mwc = MWC.withSystemRandom 
    . MWC.asGenIO 
    . runRandT
{-# INLINE mwc #-}

listOf :: Monad m 
       => Int
       -> RandT m a
       -> RandT m [a] 
listOf n r = replicateM n r
{-# INLINE listOf #-}

vectorOfVariate :: (PrimMonad m, MWC.Variate a, G.Vector v a)
                => Int
                -> RandT m (v a)
vectorOfVariate n = 
    RandT $ \gen -> MWC.uniformVector gen n
{-# INLINE vectorOfVariate #-}

vectorOf :: (Monad m, G.Vector v a)
         => Int
         -> RandT m a
         -> RandT m (v a)
vectorOf n r =
    RandT $ \gen -> G.replicateM n (runRandT r gen)
{-# INLINE vectorOf #-}
