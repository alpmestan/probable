module Main where

import Control.Applicative
import Control.Monad
import Math.Probable

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
main = randomPersons 10 >>= mapM_ print