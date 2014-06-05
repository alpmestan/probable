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

-- | Knowing the door we have chosen so far
--   and the one that's been opened
--   we decide to keep the one we've picked
keep :: Door -> Door -> Door
keep chosen _ = chosen

-- | Knowing the door we have chosen so far
--   and the one that's been opened
--   we decide to switch to the third one
switch :: Door -> Door -> Door
switch chosen opened = head $ doors \\ [chosen, opened]

-- | Given one of the two functions above ("strategies"),
--   and (the chosen door, the already opened one, and 
--   the one with the car)
--   we check wether we won or not
resultOf :: (Door -> Door -> Door) -> Door -> Door -> Door -> Result
resultOf strategy chosen opened cardoor =
    case strategy chosen opened == cardoor of
        True  -> Win
        False -> Lose

-- | Given a strategy to adopt, what's the distribution of Win/Lose ?
result :: (Door -> Door -> Door)
       -> Fin Result
result strategy = do
    -- we pick a door uniformly for hiding the car
    carDoor    <- uniformly doors

    -- we pick a door uniformly for the player
    chosenDoor <- uniformly doors

    -- we open a door that neither hides the car (for suspense)
    -- nor the one the player has picked (but they can be the same door)
    openedDoor <- uniformly $ doors \\ [carDoor, chosenDoor]

    -- ok, the player tells us whether he decides to keep or switch
    -- we now check whether he wins or not
    let res = resultOf strategy chosenDoor openedDoor carDoor

    -- we return the result, to make this a distribution of results
    return res

-- | Given a strategy to adopt, distribution of doors for a Win ?
--   this uses Bayes' rule, and consequently lives in 'FinBayes'
result' :: (Door -> Door -> Door)
        -> FinBayes (Door, Door, Door)
result' strategy = do
    -- we pick a door uniformly for hiding the car
    carDoor    <- uniformly doors

    -- we pick a door uniformly for the player
    chosenDoor <- uniformly doors

    -- we open a door that neither hides the car (for suspense)
    -- nor the one the player has picked (but they can be the same door)
    openedDoor <- uniformly $ doors \\ [carDoor, chosenDoor]

    -- ok, the player tells us whether he decides to keep or switch
    -- we now check whether he wins or not
    let res = resultOf strategy chosenDoor openedDoor carDoor

    -- here we discard all the combinations that don't lead to Win
    condition (res == Win)

    -- and return the combination
    return (chosenDoor, openedDoor, carDoor)

main :: IO ()
main = do
    putStrLn $ "Using the conservative strategy: "
            ++ show (exact $ result keep)
    -- Using the conservative strategy: (Event Win 33.3%,Event Lose 66.7%)

    putStrLn $ "Switching: "
            ++ show (exact $ result switch)
    -- Switching: (Event Win 66.7%,Event Lose 33.3%)

    putStrLn "---"

    putStrLn $ "Winning (initial door, opened door, car door)'s - CONSERVATIVE:"
    mapM_ print . exact . bayes $ result' keep
    -- Event (D1,D2,D1) 16.7%
    -- Event (D1,D3,D1) 16.7%
    -- Event (D2,D1,D2) 16.7%
    -- Event (D2,D3,D2) 16.7%
    -- Event (D3,D1,D3) 16.7%
    -- Event (D3,D2,D3) 16.7%

    putStrLn "---"

    putStrLn $ "Winning (initial door, opened door, car door)'s - SWITCHING:"
    mapM_ print . exact . bayes $ result' switch
    -- Event (D2,D3,D1) 16.7%
    -- Event (D3,D2,D1) 16.7%
    -- Event (D1,D3,D2) 16.7%
    -- Event (D3,D1,D2) 16.7%
    -- Event (D1,D2,D3) 16.7%
    -- Event (D2,D1,D3) 16.7%
