{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List
import Math.Probable

-- | We have 3 distinct doors
data Door = D1 | D2 | D3
    deriving (Eq, Show)

-- | Our 3 doors in a list
doors :: [Door]
doors = [D1, D2, D3]

-- | We can either Win or Lose
data Result = Win | Lose
    deriving (Eq, Show)

keep :: Door -> Door -> Door
keep chosen _ = chosen

switch :: Door -> Door -> Door
switch chosen opened = head $ doors \\ [chosen, opened]

resultOf :: (Door -> Door -> Door) -> Door -> Door -> Door -> Result
resultOf strategy chosen opened cardoor =
    case strategy chosen opened == cardoor of
        True  -> Win
        False -> Lose

-- maybe this should be in the library, with a more general type...
collect :: FinBayes Result -> (Event Result, Event Result)
collect = f . exact . bayes
    where f = toPair . foldl' combine (0, 0)
          combine (!winP, !loseP) (Event Win p)  = (winP+p, loseP)
          combine (!winP, !loseP) (Event Lose p) = (winP,   loseP+p)
          toPair (winP, loseP) = (Event Win winP, Event Lose loseP)

result :: (Door -> Door -> Door)
       -> FinBayes Result
result strategy = do
    carDoor    <- uniformly doors
    chosenDoor <- uniformly doors
    openedDoor <- uniformly $ doors \\ [carDoor, chosenDoor]
    let res = resultOf strategy chosenDoor openedDoor carDoor
    return res

main :: IO ()
main = do
    putStrLn $ "Using the conservative strategy: "
            ++ show (collect $ result keep)

    putStrLn $ "Switching: "
            ++ show (collect $ result switch)
