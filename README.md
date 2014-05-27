probable
========

Simple random value generation for haskell, using an efficient
random generator and minimizing system calls.

You can see how it looks in [examples](https://github.com/alpmestan/probable/tree/master/examples), or below.

_Note_: This is still WIP.

## Example

Simple example of random generation for your types, using _probable_.

``` haskell
module Main where

import Control.Applicative
import Control.Monad
import Math.Probable

import qualified Data.Vector.Unboxed as VU

data Person = Person 
    { age    :: Int
    , weight :: Double
    , salary :: Int
    } deriving (Eq, Show)

person :: RandT IO Person
person = 
    Person <$> uniformIn (1, 100)
           <*> uniformIn (2, 130)
           <*> uniformIn (500, 10000)

randomPersons :: Int -> IO [Person]
randomPersons n = mwc $ listOf n person

randomDoubles :: Int -> IO (VU.Vector Double)
randomDoubles n = mwc $ vectorOf n double

main :: IO ()
main = do
	randomPersons 10 >>= mapM_ print
	randomDoubles 10 >>= VU.mapM_ print
```

Distributions over finite sets, sampling from them and using Bayes' rule:

``` haskell
module Main where

import Math.Probable

import qualified Data.Vector as V

data Book = Interesting 
		  | Boring
	deriving (Eq, Show)

bookPrior :: Finite d => d Book
bookPrior = weighted [ (Interesting, 0.2) 
					 , (Boring, 0.8) 
					 ]

twoBooks :: Finite d => d (Book, Book)
twoBooks = do
	book1 <- bookPrior
	book2 <- bookPrior
	return (book1, book2)

sampleBooks :: RandT IO (V.Vector Book)
sampleBooks = vectorOf 10 bookPrior

oneInteresting :: Fin (Book, Book)
oneInteresting = bayes $ do
	(b1, b2) <- twoBooks
	condition (b1 == Interesting || b2 == Interesting)
	return (b1, b2)

main :: IO ()
main = do
	print $ exact bookPrior
	mwc sampleBooks >>= print
	print $ exact twoBooks
	print $ exact oneInteresting
```