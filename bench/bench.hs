{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Monad
import Criterion.Main

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import Math.Probable

randomInts :: Int -> IO (U.Vector Int)
randomInts n = mwc (vectorOf n sample)

-- | Dummy 'Person' type
data Person = Person 
    { age    :: Int
    , weight :: Double
    , salary :: Int
    } deriving (Eq, Show)

person :: (Generator g m Double, Generator g m Int) 
       => RandT g m Person
person = 
    Person <$> sampleUniform (1, 100)
           <*> sampleUniform (2, 130)
           <*> sampleUniform (500, 10000)

randomPersons :: Int -> IO (V.Vector Person)

-- | Time to benchmark!
main :: IO ()
main = do 
    defaultMain 
        [ 
            bgroup "ints" [ 
                bgroup "1000" [ bench "Prob" $ whnfIO (probInts 1000)
                              , bench "Rand" $ whnfIO (randInts 1000)
                              ],
                bgroup "50000" [ bench "Prob" $ whnfIO (probInts 50000) 
                               , bench "Rand" $ whnfIO (randInts 50000)
                               ]
            ],
          
            bgroup "persons" [ 
                bgroup "1000" [ bench "Prob" $ whnfIO (probPersons 1000)
                              , bench "Rand" $ whnfIO (randPersons 1000)
                              ],
                bgroup "50000" [ bench "Prob" $ whnfIO (probPersons 50000) 
                               , bench "Rand" $ whnfIO (randPersons 50000)
                               ]
            ]
        ]
