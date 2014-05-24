{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Monad
import Criterion.Main

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import Math.Probable
import Math.Probable.Random

randomInts :: Int -> IO (U.Vector Int)
randomInts n = mwc (vectorOf n sample)

data Person = Person 
    { age    :: Int
    , weight :: Double
    , salary :: Int
    } deriving (Eq, Show)

person :: (Generator g m Double, Generator g m Int, Monad m) 
       => RandT g m Person
person = 
    Person <$> sampleUniform (1, 100)
           <*> sampleUniform (2, 130)
           <*> sampleUniform (500, 10000)

randomPersons :: Int -> IO (V.Vector Person)
randomPersons n = mwc $ vectorOf n person

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
