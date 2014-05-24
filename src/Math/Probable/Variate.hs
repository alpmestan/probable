{-# LANGUAGE TypeFamilies #-}

module Math.Probable.Variate where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import Data.Char (chr)
import Math.Probable.Types

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.Vector.Generic  as G
import qualified System.Random.MWC    as MWC

-- | Generate uniformly distributed random
--   values for the following types:
--   'Bool', 'Double', 'Float', 'Int', 'Int8', 'Int16',
--   'Int32', 'Int64', 'Word', 'Word8', 'Word16',
--   'Word32', 'Word64'
--    and pairs, triples, quadruples combinations of 
--    'Variate' instances, in addition to your 
--    own instances (if any).
--
--  * For fixed width integral types, the type's
--    entire range is used
--
--  * For floating point numbers, the range
--    (0,1] is used (i.e. 0 excluded)
variate :: (PrimMonad m, MWC.Variate a) => Prob m a
variate = withGen MWC.uniform
{-# INLINE variate #-}

-- | Same as 'variate', except that we specify
--   a range in which the value must fall into.
-- 
-- * For integral types, the range is inclusive
--
-- * For floating point types, the range 
--   (a, b] is used, if we ignore rounding errors
variateIn :: (PrimMonad m, MWC.Variate a)   
          => (a, a)
          -> Prob m a
variateIn (a, b) = withGen $ MWC.uniformR (a, b)
{-# INLINE variateIn #-}

-- | Generate a value of an 'Enum'erable type
--   by generating an 'Int' and using 'toEnum'
enum :: (PrimMonad m, Functor m, Enum a) 
     => Prob m a
enum = toEnum `fmap` variate

-- | Generate a Char uniformly 
--   between 'A' and 'z'
alpha :: PrimMonad m => Prob m Char
alpha = do
    w <- variateIn (97, 122 :: Int)
    return $! chr w

char :: (Functor m, PrimMonad m) => Prob m Char
char = toEnum `fmap` variateIn (minc, maxc)
    where minc = fromEnum (minBound :: Char)
          maxc = fromEnum (maxBound :: Char)

string :: (Functor m, PrimMonad m) 
       => Int 
       -> Prob m String
string n = replicateM n char

text :: (Functor m, PrimMonad m) 
     => Int 
     -> Prob m T.Text
text n = T.pack `fmap` string n

lazytext :: (Functor m, PrimMonad m) 
         => Int 
         -> Prob m LT.Text 
lazytext n = LT.pack `fmap` string n

bytestring :: (Functor m, PrimMonad m) 
           => Int 
           -> Prob m B.ByteString
bytestring n = B.pack `fmap` replicateM n variate

lazybytestring :: (Functor m, PrimMonad m) 
               => Int 
               -> Prob m LB.ByteString
lazybytestring n = LB.pack `fmap` replicateM n variate