{-# LANGUAGE GeneralizedNewtypeDeriving, 
             BangPatterns, 
             TupleSections,
             FlexibleInstances, 
             TypeSynonymInstances #-}

-- |
-- Module       : Math.Probable
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- Portability  : GHC
-- 
-- /Fun with finite distributions!/
--
-- This all pretty much comes from Eric Kidd's series
-- of blog posts at <http://www.randomhacks.net/probability-monads/>.
--
-- I have adapted it a bit by making it fit into my own
-- random generation/sampling scheme. 
--
-- The idea and purpose of this module should be clear after going
-- through an example.
--
-- 

module Math.Probable.Distribution.Finite 
    ( -- * Probability type 
      P(..), prob

      -- * 'Event' type
    , Event(..), never
      -- * 'EventT' monad transformer
    , EventT(..)
      -- * Finite distributions: 'Finite' and 'Fin'
    , Finite(..), Fin, exact, uniformly
      -- * Bayes' rule: 'FinBayes'
    , FinBayes, bayes, condition
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Math.Probable.Random

-- | Probability type: wrapper around Double
--   for a nicer Show instance and for more easily
--   enforcing normalization of weights
newtype P = P Double
    deriving (Eq, Ord, Fractional, Num, Real, RealFrac)

-- | Get the underlying probability 
prob :: P -> Double
prob (P x) = x

instance Show P where
  show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits = round (1000 * p)
          intPart = digits `div` 10  :: Int
          fracPart = digits `mod` 10 :: Int

-- | An event, and its probability
data Event a = Event a {-# UNPACK #-} !P
    deriving (Eq, Show)

-- | This event never happens (probability of 0)
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


-- | 'EventT' monad transformer
-- 
-- It pairs a value with a probability within the 'm' monad
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

-- | Create a 'Finite' distribution over the values in
--   the list, each with an equal probability
uniformly :: Finite d => [a] -> d a
uniformly = weighted . map (,1)
{-# INLINE uniformly #-}

-- | 'Fin' is just 'EventT []'
--
-- You can think of 'Fin a' meaning '[Event a]'
-- i.e a list of the possible outcomes of type 'a'
-- with their respective probability
type Fin = EventT []

-- | See the outcomes of a finite distribution and their probabilities
exact :: Fin a -> [Event a]
exact = runEventT
{-# INLINE exact #-}

-- | 'FinBayes' is 'Fin' with a 'MaybeT' layer
-- 
-- What is that for? The 'MaybeT' lets us express
-- the fact that what we've drawn from the distribution
-- isn't of interest anymore. Example:
--
-- > data Wine = Good | Bad deriving (Eq, Show)
-- > 
-- > wines :: Fin Wine
-- > wines = weighted [(Good, 0.2), (Bad, 0.8)]
-- >
-- > twoWines :: Fin (Wine, Wine)
-- > twoWines = (,) <*> wines <$> wines
-- >
-- > decentMeal :: Fin (Wine, Wine)
-- > decentMeal = do
-- >   (wine1, wine2) <- twoWines
-- >   -- we only consider the outcomes of 'twoWines' 
-- >   -- where at least one of the two wines is good
-- >   -- because we're having a nice meal and are looking
-- >   -- for a decent pair of wine
-- >   condition (wine1 == Good || wine2 == Good)
-- >   return (wine1, wine2)
type FinBayes = MaybeT Fin

condition :: Bool -> FinBayes ()
condition = MaybeT . return . toMaybe
    where toMaybe True = Just ()
          toMaybe False = Nothing

bayes :: FinBayes a -> Fin a
bayes = onlyJust . runMaybeT

onlyJust :: Fin (Maybe a) -> Fin a
onlyJust dist
    | total > 0 = EventT (map adjust filtered)
    | otherwise = EventT []
  where filtered = catMaybes' (runEventT dist)
        total = sum (map proba filtered)
        adjust (Event x p) = Event x (p / total)
        proba (Event _ p) = p
        -- value (Event x _) = x

catMaybes' :: [Event (Maybe a)] -> [Event a]
catMaybes' [] = []
catMaybes' (Event Nothing _ : xs) =
  catMaybes' xs
catMaybes' (Event (Just x) p : xs) =
  Event x p : catMaybes' xs

-- | a distribution of probabilities 
--   over a finite set.
class (Functor d, Monad d) => Finite d where
    
    weighted :: [(a, Double)] -> d a

instance Finite Fin where
    weighted l = EventT $ map weight l

        where weight (x, w) = Event x $ P (w / total)
              total         = foldl' (\w (_, w') -> w + w') 0 l

instance PrimMonad m => Finite (RandT m) where
    weighted = liftF . weighted

liftF :: PrimMonad m => Fin a -> RandT m a
liftF dist = do
    d <- double
    pick (P d) (exact dist)

pick :: Monad m => P -> [Event a] -> m a
pick _ [] = error "pick: no value to pick"
pick n (Event x p : evts)
    | n <= p    = return x
    | otherwise = pick (n-p) evts

instance Finite FinBayes where
    weighted = lift . weighted