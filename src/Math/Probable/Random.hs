{-# LANGUAGE BangPatterns,
             TypeFamilies #-}

-- |
-- Module       : Math.Probable
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- Portability  : GHC
-- 
-- Random number generation based on 'MWC.Gen',
-- defined as a Monad transformer.
-- 
-- Quickstart, in ghci:
--
-- > λ> import Math.Probable
-- > λ> import Control.Applicative
-- > λ> mwc double
-- > 0.2756820707828763
-- > λ> mwc word64
-- > 12175918187293541909
-- > λ> mwc $ (,) <$> bool <*> intIn (0, 10)
-- > (True,7)
-- > λ> mwc $ do { n <- intIn (1, 10) ; listOf n (listOf 2 bool) }
-- > [ [False,True],[True,False],[False,True],[False,False],[False,False],
-- >   [False,True],[True,False],[True,False],[True,True],[False,False]
-- > ]
--
-- This module features a bunch of combinators that can help you create
-- some random generation descriptions easily, and in a very familiar style.
--
-- You can easily combine them through the 'Monad' instance for 'RandT'
-- which really just make sure everyone gets a 'MWC.Gen' (from mwc-random)
-- eventually. This of course makes 'RandT' a 'Functor' and an 'Applicative'.
--
-- > import Math.Probable
-- >
-- > data Person = 
-- >   Person { name   :: String
-- >          , age    :: Int
-- >          , salary :: Double
-- >          }
-- >     deriving (Eq, Show)
-- > 
-- > randomPerson :: PrimMonad m 
-- >              => RandT m Person
-- > randomPerson = do
-- >     -- we pick a random length
-- >     -- for the person's name
-- >     nameLen <- intIn (3, 10) 
-- >                            
-- >     -- and just express what a random Person
-- >     -- should be, Applicative-style
-- >     Person <$> pickName nameLen    -- pick a name
-- >            <*> intIn (0, 100)      -- an Int between 0 and 100
-- >            <*> doubleIn (0, 10000) -- a Double between 0 and 10000
-- >
-- >     where pickName nameLen = do
-- >               -- the initial, between 'A' and 'Z'
-- >               initial <- chr `fmap` intIn (65, 90)
-- >  
-- >               (initial:) `fmap` 
-- >               -- the rest, between 'a' and 'z'
-- >                   listOf (nameLen - 1)
-- >                          (chr `fmap` intIn (97, 122))
-- 
-- This is all nice, but how do we actually sample such a Person?
-- You just have to call 'mwc':
-- 
-- > λ> mwc randomPerson
-- > Person {name = "Ojeesra", age = 83, salary = 3075.9945184521885}
-- 
-- So any value of type 'RandT m a' is something that you'll eventually 
-- run in 'm' (hence 'IO' or 'ST' 's') for generating a /random value/ of
-- type 'a'. Note that 'mwc' forces the execution using 'withSystemRandom'
-- and gets you back in 'IO', whereas 'mwcST' gets you back in 'ST' 's'.
-- 
-- My simple name generation routine can help you pick a name for your baby,
-- if you are having one soon.
-- 
-- > λ> map name `fmap` mwc (listOf 10 randomPerson)
-- > ["Npujbc","Faidx","Zusha","Ghbipic","Ljaestei","Fktcfonnxe","Hlvkolds","Zpws","Zgmrkrdv","Rhcd"]
--
-- If we were to make a generator that could generate more familiar
-- and creativity-free names, we wouldn't sample uniformly
-- from the alphabet.

module Math.Probable.Random 
    ( -- * 'RandT' type
      RandT(..)

    , -- * Actually generating random values
      mwc, mwcST
    
    , -- * Combinators for generating individual values
      uniformIn

    , int, int8, int16, int32, int64
    , intIn, int8In, int16In, int32In, int64In

    , word, word8, word16, word32, word64
    , wordIn, word8In, word16In, word32In, word64In

    , float, double
    , floatIn, doubleIn

    , bool
    
    , -- * Filling containers with random values
      listOf, vectorOf, vectorOfVariate
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Int
import Data.Word
import qualified Data.Vector.Generic as G
import qualified System.Random.MWC as MWC

-- | 'RandT' type, equivalent to a
--   'ReaderT' ('MWC.Gen' ('PrimState' m))
--   
--   This lets you build simple or complex random generation
--   routines without having the generator passed all around
--   and just run the whole thing in the end, most likely 
--   by using 'mwc'.
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

-- | Generate a random 'Int'. The whole 'Int' range is used.
--
-- > λ> mwc int
-- > 8354496680947360541
int :: PrimMonad m => RandT m Int
int = RandT MWC.uniform
{-# INLINE int #-}

-- | Generate a random 'Int' in the given range.
--
-- > λ> mwc $ intIn (0, 10)
-- > 7
intIn :: PrimMonad m => (Int, Int) -> RandT m Int
intIn (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE intIn #-}

-- | Generate a random 'Int8'. The whole 'Int8' range is used.
--
-- > λ> mwc int8
-- > -65
int8 :: PrimMonad m => RandT m Int8
int8 = RandT MWC.uniform
{-# INLINE int8 #-}

-- | Generate a random 'Int8' in the given range
--
-- > λ> mwc $ int8In (-10, 10)
-- > -3
int8In :: PrimMonad m => (Int8, Int8) -> RandT m Int8
int8In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE int8In #-}

-- | Generate a random 'Int16'. The whole 'Int16' range is used.
--
-- > λ> mwc int16
-- > 15413
int16 :: PrimMonad m => RandT m Int16
int16 = RandT MWC.uniform
{-# INLINE int16 #-}

-- | Generate a random 'Int16' in the given range
--
-- > λ> mwc $ int16In (-500, 30129)
-- > 9501
int16In :: PrimMonad m => (Int16, Int16) -> RandT m Int16
int16In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE int16In #-}

-- | Generate a random 'Int32'. The whole 'Int32' range is used.
--
-- > λ> mwc int32
-- > 1774441747
int32 :: PrimMonad m => RandT m Int32
int32 = RandT MWC.uniform
{-# INLINE int32 #-}

-- | Generate a random 'Int32' in the given range.
--
-- > λ> mwc $ int32In (-500, 30129)
-- > 8012
int32In :: PrimMonad m => (Int32, Int32) -> RandT m Int32
int32In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE int32In #-}

-- | Generate a random 'Int64'. The whole 'Int64' range is used.
--
-- > λ> mwc int64
-- > -2596387699802756017
int64 :: PrimMonad m => RandT m Int64
int64 = RandT MWC.uniform
{-# INLINE int64 #-}

-- | Generate a random 'Int64' in the given range.
--
-- > λ> mwc $ int64In (-2^30, 30)
-- > -630614786
int64In :: PrimMonad m => (Int64, Int64) -> RandT m Int64
int64In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE int64In #-}

-- | Generate a random 'Word'. The whole 'Word' range is used.
-- 
-- > λ> mwc word
-- > 3106215968599504888
word :: PrimMonad m => RandT m Word
word = RandT MWC.uniform
{-# INLINE word #-}

-- | Generate a random 'Word' in the given range.
--
-- > λ> mwc $ wordIn (1, 64)
-- > 28
wordIn :: PrimMonad m => (Word, Word) -> RandT m Word
wordIn (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE wordIn #-}

-- | Generate a random 'Word8'. The whole 'Word8' range is used.
-- 
-- > λ> mwc word8
-- > 231
word8 :: PrimMonad m => RandT m Word8
word8 = RandT MWC.uniform
{-# INLINE word8 #-}

-- | Generate a random 'Word8' in the given range
-- 
-- > λ> mwc $ word8In (2, 15)
-- > 3
word8In :: PrimMonad m => (Word8, Word8) -> RandT m Word8
word8In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE word8In #-}

-- | Generate a random 'Word16'. The whole 'Word16' range is used.
--
-- > λ> mwc word16
-- > 31127
word16 :: PrimMonad m => RandT m Word16
word16 = RandT MWC.uniform
{-# INLINE word16 #-}

-- | Generate a random 'Word16' in the given range.
--
-- > λ> mwc $ word16In (2^13, 2^14)
-- > 8885
word16In :: PrimMonad m => (Word16, Word16) -> RandT m Word16
word16In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE word16In #-}

-- | Generate a random 'Word32'. The whole 'Word32' range is used.
--
-- > λ> mwc word32
-- > 3917666696
word32 :: PrimMonad m => RandT m Word32
word32 = RandT MWC.uniform
{-# INLINE word32 #-}

-- | Generate a random 'Word32' in the given range.
--
-- > λ> mwc $ word32In (100, 330)
-- > 125
word32In :: PrimMonad m => (Word32, Word32) -> RandT m Word32
word32In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE word32In #-}

-- | Generate a random 'Word64'. The whole 'Word64' range is used.
--
-- > λ> mwc word64
-- > 12496697905424132339
word64 :: PrimMonad m => RandT m Word64
word64 = RandT MWC.uniform
{-# INLINE word64 #-}

-- | Generate a random 'Word64' in the given range.
--
-- > λ> mwc $ word64In (2^45, 2^46)
-- > 59226619151303
word64In :: PrimMonad m => (Word64, Word64) -> RandT m Word64
word64In (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE word64In #-}

-- | Generate a random 'Float' between 0 (excluded)
--   and 1 (included)
-- 
-- > λ> mwc float
-- > 0.11831179
float :: PrimMonad m => RandT m Float
float = RandT MWC.uniform
{-# INLINE float #-}

-- | Generate a random 'Float' in the given range
--
-- > λ> mwc $ floatIn (0.20, 3.14)
-- > 1.3784513
floatIn :: PrimMonad m => (Float, Float) -> RandT m Float
floatIn (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE floatIn #-}

-- | Generate a random 'Double' between 0 (excluded)
--   and 1 (included)
-- 
-- > λ> mwc double
-- > 0.7689412928620208
double :: PrimMonad m => RandT m Double
double = RandT MWC.uniform
{-# INLINE double #-}

-- | Generate a random 'Double' in the given range
-- 
-- > λ> mwc $ doubleIn (-30.121121445, 0.129898878612)
-- > -13.612464813256999
doubleIn :: PrimMonad m => (Double, Double) -> RandT m Double
doubleIn (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE doubleIn #-}

-- | Generate a random 'Bool'
--
-- > λ> mwc bool
-- > False
bool :: PrimMonad m => RandT m Bool
bool = RandT MWC.uniform
{-# INLINE bool #-}

-- | A generic function for sampling uniformly any type
--   that implements 'MWC.Variate'.
--
--   All the 'xxxIn' functions from this module just
--   call 'MWC.uniformR'.
uniformIn :: (MWC.Variate a, PrimMonad m) => (a, a) -> RandT m a
uniformIn (a, b) = RandT $ MWC.uniformR (a, b)
{-# INLINE uniformIn #-}

-- | Take a 'RandT' value and run it in 'IO',
--   generating all the random values described by
--   the 'RandT'. It just uses 'MWC.withSystemRandom'
--   so you really should try hard to put your whole
--   random generation logic in 'RandT' and call 
--   'mwc' in the end, thus initialising the generator
--   only once and generating everything with it.
--
--   See the documentation for 'MWC.withSystemRandom' for more about this. 
-- 
-- > λ> mwc $ (+2) `fmap` int8
-- > 34
mwc :: RandT IO a
    -> IO a
mwc = MWC.withSystemRandom 
    . MWC.asGenIO 
    . runRandT
{-# INLINE mwc #-}

-- | If for some reason you have a 'RandT' ('ST' 's')
--   you can run it from 'IO' just like we do with 'mwc'.
-- 
-- > λ> mwcST $ listOf 4 bool
-- > [False,False,True,True]
mwcST :: RandT (ST s) a
      -> IO a
mwcST = MWC.withSystemRandom
      . MWC.asGenST
      . runRandT
{-# INLINE mwcST #-}

-- | Repeatedly run a random computation
--   yielding a value of type 'a' to get 
--   a list of random values of type 'a'.
-- 
-- > λ> mwc (listOf 30 float)
-- > [ 5.438623e-2,0.78114086,0.4954672,0.5958733,0.47243807,5.883485e-2
-- > , 5.500287e-2,0.79262286,0.5528683,0.7628807,0.80705905,0.15368962
-- > , 0.8654971,0.4560417,0.23922172,0.5069659,0.8130155,0.6559351
-- > , 1.31405e-2,0.25705606,0.7134138,0.79111993,0.7529769,0.10573909
-- > , 0.37731406,0.6289338,0.85156864,0.15691182,0.9910314,8.133593e-2
-- > ]
--
-- > λ> mwc (sum `fmap` listOf 30 float)
-- > 15.037931
listOf :: Monad m 
       => Int
       -> RandT m a
       -> RandT m [a] 
listOf n r = replicateM n r
{-# INLINE listOf #-}

-- | A function for generating a vector
--   of the given length for values
--   whose types are instances of 'MWC.Variate'.
-- 
--   This function is generic in the type of vector it returns,
--   any instance of 'G.Vector' will do.
-- 
--   It's just a wrapper arround 'MWC.uniformVector'
--   and doesn't really use the 'Monad' instance of 'RandT'.
--
--   But if you want to have a vector of 'Person's, 
--   you have to use 'vectorOf'.
-- 
-- > λ> import qualified Data.Vector.Unboxed as V
-- > λ> :set -XScopedTypeVariables
-- > λ> v :: V.Vector Double <- mwc $ vectorOfVariate 10
-- > λ> V.mapM_ print v
-- > 3.8565084196117705e-2
-- > 0.575103826646098
-- > 0.379710162825715
-- > 0.4066991135077237
-- > 0.9778431248247549
-- > 0.3786223745680838
-- > 0.4361789615081698
-- > 0.9904407826187301
-- > 0.2951087330670904
-- > 0.1533350329892028
vectorOfVariate :: (PrimMonad m, MWC.Variate a, G.Vector v a)
                => Int
                -> RandT m (v a)
vectorOfVariate n = 
    RandT $ \gen -> MWC.uniformVector gen n
{-# INLINE vectorOfVariate #-}

-- | A function for generating a vector of the given
--   length with random values /of any type/ 
--   (in contrast to 'vectorOfVariate').
--
--   It is generic in the 'G.Vector' instance it
--   hands you back. It's implemented in terms of
--   'G.replicateM' and has been benchmarked to perform
--   as well as 'MWC.uniformVector' on simple types
--   ('MWC.uniformVector' can't generate values for types
--   that don't have a 'MWC.Variate' instance).
--
-- > λ> import qualified Data.Vector.Unboxed as V
-- > λ> :set -XScopedTypeVariables
-- > λ> v :: V.Vector Int <- mwc $ vectorOf 10 int
-- > λ> V.mapM_ print v
-- > -3920053790769159788
-- > 3983393642052845448
-- > 1528310798822685910
-- > 3522283620461337684
-- > 6451017362937898910
-- > 1929485210691770214
-- > 8547527164583329795
-- > 3298785082692387491
-- > 4019024417224980311
-- > -5216301990322376953
vectorOf :: (Monad m, G.Vector v a)
         => Int
         -> RandT m a
         -> RandT m (v a)
vectorOf n r =
    RandT $ \gen -> G.replicateM n (runRandT r gen)
{-# INLINE vectorOf #-}
