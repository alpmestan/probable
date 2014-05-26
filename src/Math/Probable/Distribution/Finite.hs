{-# LANGUAGE GeneralizedNewtypeDeriving, 
             BangPatterns, 
             TupleSections, 
             TypeSynonymInstances,
             UndecidableInstances,
             FlexibleContexts, 
             FlexibleInstances #-}

module Math.Probable.Distribution.Finite where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Math.Probable.Random

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

-- | a distribution of probabilities 
--   over a finite set.
class (Functor d, Monad d) => FromFinite d where
    weighted :: [(a, Double)] -> d a

uniform :: FromFinite d => [a] -> d a
uniform = weighted . map (,1)
{-# INLINE uniform #-}

type Fin = EventT []

exact :: Fin a -> [Event a]
exact = runEventT

instance FromFinite Fin where
    weighted l = EventT $ map weight l

        where weight (x, w) = Event x $ P (w / total)
              total         = foldl' (\w (_, w') -> w + w') 0 l

instance Generator g m Double => FromFinite (RandT g m) where
    weighted = liftF . weighted

liftF :: Generator g m Double => Fin a -> RandT g m a
liftF dist = do
    d <- double
    pick (P d) (exact dist)

pick :: Monad m => P -> [Event a] -> m a
pick _ [] = error "Dist (RandT g m).pick: no values to pick from"
pick n (Event x p : evts)
    | n <= p    = return x
    | otherwise = pick (n-p) evts

