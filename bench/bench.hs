module Main where

import Control.Applicative
import Control.Monad
import Criterion.Main

import qualified Data.Vector.Unboxed as V
import Math.Probable

randomInts :: Int -> IO (V.Vector Int)
randomInts n = runProb (vectorOf n variate)

data Person = Person 
	{ age    :: Int
	, weight :: Double
	, name   :: String
	, salary :: Int
	} deriving (Eq, Show)

person :: Prob IO Person
person = 
	Person <$> variateIn (1, 100)
		   <*> variateIn (2, 130)
		   <*> replicateM 15 alpha
		   <*> variateIn (500, 10000)

randomPersons :: Int -> IO [Person]
randomPersons n = runProb $ replicateM n person

main :: IO ()
main = do 
	defaultMain 
		[ 
		  bgroup "ints" [ bench "generate 1000"  $ whnfIO (randomInts 1000)
	                    , bench "generate 50000" $ whnfIO (randomInts 50000)
	                    ]
	    , bgroup "persons" [ bench "generate 1000"  $ whnfIO (randomPersons 1000) 
	    				   , bench "generate 50000" $ whnfIO (randomPersons 50000)
	    				   ]
	    ]
