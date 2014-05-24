{-# LANGUAGE GeneralizedNewtypeDeriving, 
             BangPatterns, 
             TupleSections, 
             TypeSynonymInstances, 
             FlexibleInstances #-}

module Math.Probable.Distribution.Finite where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List

newtype P = P Double
    deriving (Eq, Ord, Fractional, Num, Real, RealFrac)

prob :: P -> Double
prob (P x) = x

instance Show P where
  show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits = round (1000 * p)
          intPart = digits `div` 10  :: Int
          fracPart = digits `mod` 10 :: Int

-- | An event, and the probability that it happens
data Event a = Event a {-# UNPACK #-} !P
    deriving (Eq, Show)

never :: Event a
never = Event undefined 0

instance Functor Event where
    fmap f (Event evt p) = Event (f evt) p

instance Applicative Event where
    pure evt = Event evt 1

    Event f p1 <*> Event e p2 
        | p1 == 0 || p2 == 0 = never 
        | otherwise          = Event (f e) (p1*p2)

instance Monad Event where
    return evt = Event evt 1

    (Event evt p) >>= f | p == 0 = never
                        | otherwise = Event e' (p*p')
        where Event e' p' = f evt


-- * Monad Transformer
newtype EventT m a = EventT { runEventT :: m (Event a) }

instance Monad m => Functor (EventT m) where
    fmap = liftM

instance (Functor m, Monad m) => Applicative (EventT m) where
    pure = return

    mf <*> x = mf >>= \f -> fmap f x

instance Monad m => Monad (EventT m) where
    return = lift . return

    m >>= f = EventT go
        where go = do 
                ph <- runEventT m
                case ph of
                    Event e p1 | p1 == 0   -> return never
                               | otherwise -> 
                                    do Event e' p2 <- runEventT (f e)
                                       return $ Event e' (p1 * p2)


instance MonadTrans EventT where
    lift x = EventT (liftM return x)

-- | A finite distribution, i.e a distribution of probabilities 
--   over a finite set.
class (Functor d, Monad d) => Finite d where
    weighted :: [(a, Double)] -> d a

uniform :: Finite d => [a] -> d a
uniform = weighted . map (,1)

type Fin = EventT []

exact :: Fin a -> [Event a]
exact = runEventT

instance Finite Fin where
    weighted l = EventT $ map weight l

        where weight (x, w) = Event x $ P (w / total)
              total         = foldl' (\w (_, w') -> w + w') 0 l

