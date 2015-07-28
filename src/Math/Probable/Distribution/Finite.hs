{-# LANGUAGE GeneralizedNewtypeDeriving, 
             CPP,
             BangPatterns, 
             TupleSections,
             FlexibleInstances, 
             TypeSynonymInstances #-}
-- |
-- Module       : Math.Probable.Distribution.Finite
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
-- through an example. First, let's import the library and 'vector'.
--
-- > import Math.Probable
-- > import qualified Data.Vector as V
--
-- We are going to talk about Books, and particularly about whether a given
-- book is interesting or not.
--
-- > data Book = Interesting 
-- >           | Boring
-- >     deriving (Eq, Show)
-- 
-- Let's say we have very particular tastes, and that we think
-- that only 20% of all books are interesting (that's not so small actually. oh well).
-- 
-- > bookPrior :: Finite d => d Book
-- > bookPrior = weighted [ (Interesting, 0.2) 
-- >                      , (Boring, 0.8) 
-- >                      ]
-- 
-- 'weighted' belongs to the 'Finite' class, which represents 
-- types that can somehow represent a distribution over a finite set.
-- That makes our distribution polymorphic in how we will use it. Awesome!
-- 
-- So how does it look?
--
-- > λ> exact bookPrior -- in ghci
-- > [Event Interesting 20.0%,Event Boring 80.0%]
--
-- 'exact' takes 'Fin' 'a' and gives you the
-- inner list that 'Fin' uses to represent the distribution.
--
-- Now, what if we pick two books? First, how do we even do that?
-- Well, any instance of 'Finite' must be a 'Monad', so you have your
-- good old /do notation/. The ones provided by this package also
-- provide 'Functor' and 'Applicative' instances, but let's use
-- do.
-- 
-- > twoBooks :: Finite d => d (Book, Book)
-- > twoBooks = do
-- >     book1 <- bookPrior
-- >     book2 <- bookPrior
-- >     return (book1, book2)
--
-- Nothing impressive. We pick a book with the prior
-- we defined above, then another, pair them together
-- and hand the pair back. What this will actually do
-- is behave just like in the list monad, but in addition
-- to this it will combine the probabilities of the various
-- events we could be dealing with in the appropriate way.
-- 
-- So, how about we verify what I just said:
--
-- > λ> exact twoBooks
-- > [ Event (Interesting,Interesting) 4.0%
-- > , Event (Interesting,Boring) 16.0%
-- > , Event (Boring,Interesting) 16.0%
-- > , Event (Boring,Boring) 64.0%
-- > ]
--
-- Nice! Let's take a look at a more complicated scenario now.
--
-- What if we wanted to take a look at the same distribution,
-- with just a difference: we want at least one of the books to
-- be an Interesting one.
-- 
-- > oneInteresting :: Fin (Book, Book)
-- > oneInteresting = bayes $ do -- notice the call to bayes
-- >     (b1, b2) <- twoBooks
-- >     condition (b1 == Interesting || b2 == Interesting)
-- >     return (b1, b2)
-- 
-- We get two books from the previous distribution, and use 'condition'
-- to restrict the current distribution to the values of b1 and b2
-- that verify our condition. This lifts us in the 'FinBayes' type,
-- where our probabilistic computations can "fail" in some sense. 
-- If you want to discard values and restrict the ones on which you'll
-- run further computations, use 'condition'. 
--
-- However, how do we view the distribution now, without having all
-- those 'Maybe's in the middle? That's what 'bayes' is for. It runs
-- the computations for the distribution and discards all the ones
-- where any 'condition' wasn't satisfied. In particular, it means
-- it hands you back a normal 'Fin' distribution.
--
-- If we run this one:
--
-- > λ> exact oneInteresting
-- > [ Event (Interesting,Interesting) 11.1%
-- > , Event (Interesting,Boring) 44.4%
-- > , Event (Boring,Interesting) 44.4%
-- > ]
--
-- Note that these finite distribution types support random sampling too:
--
-- * If one of your distributions has a type like "Finite d => d X",
--   you can actually consider it as a 'RandT' value, from which you can sample.
-- 
-- * If you have a 'Fin' distribution, you can use 'liftF' (lift 'Fin')
--   to randomly sample an element from it, by more or less following 
--   the distribution's probabilities.
--
-- > -- example of the former
-- > sampleBooks :: RandT IO (V.Vector Book)
-- > sampleBooks = vectorOf 10 bookPrior
-- 
-- > λ> mwc sampleBooks
-- > fromList [Interesting,Boring,Boring,Boring,Boring
-- >          ,Boring,Boring,Interesting,Boring,Boring]
--
-- > λ> mwc $ listOf 4 (liftF oneInteresting) -- example of the latter
-- > [ (Boring,Interesting)
-- > , (Boring,Interesting)
-- > , (Boring,Interesting)
-- > , (Interesting,Boring)
-- > ]
module Math.Probable.Distribution.Finite 
    ( -- * Probability type 
      P(..), prob

      -- * 'Event' type
    , Event(..), never
      -- * 'EventT' monad transformer
    , EventT(..)
      -- * Finite distributions: 'Finite' and 'Fin'
    , Finite(..), Fin, exact, uniformly, liftF
      -- * Bayes' rule: 'FinBayes'
    , FinBayes, bayes, condition, onlyJust
    ) where

#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
#endif

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
--
-- > λ> prob (P 0.1)
-- > 0.1
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
-- 
-- > never = Event undefined 0
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
-- 
-- > λ> exact $ uniformly [True, False]
-- > [Event True 50.0%,Event False 50.0%]
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
--
-- > λ> exact $ uniformly [True, False]
-- > [Event True 50.0%,Event False 50.0%]
-- 
-- > λ> data Fruit = Apple | Orange deriving (Eq, Show)
-- > λ> exact $ uniformly [Apple, Orange]
-- > [Event Apple 50.0%,Event Orange 50.0%]
-- 
-- > λ> exact $ weighted [(Apple, 0.8), (Orange, 0.2)]
-- > [Event Apple 80.0%,Event Orange 20.0%]
exact :: Fin a -> [Event a]
exact = runEventT
{-# INLINE exact #-}

-- | 'FinBayes' is 'Fin' with a 'MaybeT' layer
-- 
-- What is that for? The 'MaybeT' lets us express
-- the fact that what we've drawn from the distribution
-- isn't of interest anymore, using 'condition',
-- and observing the remaining cases, using 'bayes',
-- to get back to a normal finite distribution. Example:
--
-- > data Wine = Good | Bad deriving (Eq, Show)
-- > 
-- > wines :: Finite d => d Wine
-- > wines = weighted [(Good, 0.2), (Bad, 0.8)]
-- >
-- > twoWines :: Finite d => d (Wine, Wine)
-- > twoWines = (,) <*> wines <$> wines
-- >
-- > decentMeal :: FinBayes (Wine, Wine)
-- > decentMeal = do
-- >   (wine1, wine2) <- twoWines
-- >   -- we only consider the outcomes of 'twoWines' 
-- >   -- where at least one of the two wines is good
-- >   -- because we're having a nice meal and are looking
-- >   -- for a decent pair of wine
-- >   condition (wine1 == Good || wine2 == Good)
-- >   return (wine1, wine2)
-- >
-- > -- to view the distribution, applying
-- > -- Bayes' rule on our way:
-- > exact (bayes decentMeal)
type FinBayes = MaybeT Fin

-- | This is the core of 'FinBayes'. If the 'Bool' is false,
--   the current computation is shortcuited (sent to a 'Nothing'
--   in 'MaybeT') and won't be included when running the distribution
--   with 'bayes'. See the documentation of 'FinBayes' for an example.
condition :: Bool -> FinBayes ()
condition = MaybeT . return . toMaybe
    where toMaybe True = Just ()
          toMaybe False = Nothing

-- | This functions discards all the elements of the distribution
--   for which the call to 'condition' yielded 'Nothing'.
--   While 'condition' does the mapping to 'Maybe' values,
--   this function discards all of those values for which the condition
--   was not met.
bayes :: FinBayes a -> Fin a
bayes = onlyJust . runMaybeT

-- | Keeps only the 'Just's and remove the 'Maybe' layer
--   in the distribution.
onlyJust :: Fin (Maybe a) -> Fin a
onlyJust dist
    | total > 0 = EventT (map adjust filtered)
    | otherwise = EventT []
  where filtered = catMaybes' (runEventT dist)
        total = sum (map proba filtered)
        adjust (Event x p) = Event x (p / total)
        proba (Event _ p) = p
        -- value (Event x _) = x

-- | This function, used by 'onlyJust', discards all the events
--   holding a 'Nothing'.
catMaybes' :: [Event (Maybe a)] -> [Event a]
catMaybes' [] = []
catMaybes' (Event Nothing _ : xs) =
  catMaybes' xs
catMaybes' (Event (Just x) p : xs) =
  Event x p : catMaybes' xs

-- | T distribution of probabilities 
--   over a finite set.
class (Functor d, Monad d) => Finite d where
    
    -- | The only requirement is to somehow
    --   be able to represent the distribution
    --   corresponding to the list given as argument, e.g:
    --
    -- > weighted [(True, 0.8), (False, 0.2)]
    --
    -- It should also be able to handle the normalization for you.
    --
    -- > weighted [(True, 8), (False, 2)]
    weighted :: [(a, Double)] -> d a

instance Finite Fin where
    weighted l = EventT $ map weight l

        where weight (x, w) = Event x $ P (w / total)
              total         = foldl' (\w (_, w') -> w + w') 0 l

instance PrimMonad m => Finite (RandT m) where
    weighted = liftF . weighted

-- | Make finite distributions ('Fin') citizens of
--   'RandT' by simply sampling an element at random
--   while still approximately preserving the distribution
--
-- > λ> mwc . liftF $ uniformly [True, False]
-- > False
-- > λ> mwc . liftF $ uniformly [True, False]
-- > True
-- > λ> mwc . liftF $ weighted [("Haskell", 99), ("PHP", 1)]
-- > "Haskell"
liftF :: PrimMonad m => Fin a -> RandT m a
liftF dist = do
    d <- double
    pick (P d) (exact dist)

-- | Look for an 'Event' in that list that has
--   a probability superior to the one given as an
--   argument.
pick :: Monad m => P -> [Event a] -> m a
pick _ [] = error "pick: no value to pick"
pick n (Event x p : evts)
    | n <= p    = return x
    | otherwise = pick (n-p) evts

instance Finite FinBayes where
    weighted = lift . weighted
