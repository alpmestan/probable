module Main where

import Control.Applicative
import Control.Monad
import Criterion.Main
import Math.Probable

import qualified Control.Monad.Random as MR
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector         as V

-- | Dummy 'Person' type
data Person = Person 
    { age    :: Int
    , weight :: Double
    , name   :: String
    , salary :: Int
    } deriving (Eq, Show)

-- | Generating a 'V.Vector' of 'Int' with probable
probInts :: Int -> IO (VU.Vector Int)
probInts n = runProb (vectorOf n variate)

-- | Generating a 'Person' with probable
person :: Prob IO Person
person = 
    Person <$> variateIn (1, 100)
           <*> variateIn (2, 130)
           <*> replicateM 8 alpha
           <*> variateIn (500, 10000)

-- | Generating a 'V.Vector' of 'Person's with probable
probPersons :: Int -> IO (V.Vector Person)
probPersons n = runProb (vectorOf n person)

-- | Generating a 'V.Vector' of 'Int's with MonadRandom
randInts :: Int -> IO (VU.Vector Int)
randInts n = 
    MR.evalRandIO $ VU.fromList `fmap` replicateM n MR.getRandom

-- | Generating a 'Person' with MonadRandom
randPerson :: MR.RandomGen g => MR.Rand g Person
randPerson = 
    Person <$> MR.getRandomR (1, 100)
           <*> MR.getRandomR (2, 130)
           <*> replicateM 8 randChar
           <*> MR.getRandomR (500, 10000)

    where randChar = MR.getRandom

-- | Generating a 'V.Vector' of 'Person's with MonadRandom
randPersons :: Int -> IO (V.Vector Person)
randPersons n = 
    MR.evalRandIO $ V.fromList `fmap` replicateM n randPerson

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
