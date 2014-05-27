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