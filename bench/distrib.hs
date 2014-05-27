module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Criterion.Main
import System.Random.MWC

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import Math.Probable

n :: Int
n = 100000

data Fruit = Apple | Banana | Orange
    deriving (Eq, Show)

fruitDist :: FromFinite d => d Fruit
fruitDist = weighted [ (Apple, 0.3)
                     , (Banana, 0.6)
                     , (Orange, 0.1)
                     ]
liftF' :: Fin a -> IO a
liftF' dist = do
    d <- withSystemRandom . asGenIO $ uniform
    pick (P d) (exact dist)

instance FromFinite IO where
    weighted = liftF' . weighted

observe1 :: RandT IO Fruit -> IO (V.Vector Fruit)
observe1 = mwc . vectorOf5 n

observe2 :: RandT IO Fruit -> IO (V.Vector Fruit)
observe2 r = mwc . RandT $ \gen ->
   V.replicateM n (runRandT r gen)

observe4 :: IO Fruit
         -> IO (V.Vector Fruit)
observe4 fr = 
    V.replicateM n (fr gen)


-- | Time to benchmark!
main :: IO ()
main = do 
    defaultMain 
        [ 
            bgroup "big vector of fruits"
                [ bench "observe1" $ whnfIO (observe1 fruitDist)
                , bench "observe2" $ whnfIO (observe2 fruitDist)
                , bench "observe3" $ whnfIO (observe4 fruitDist)
                ]
        ]
