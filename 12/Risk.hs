{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
  x <- diceAndSort attackers
  y <- diceAndSort defenders
  let results = zipWith (>) x y

  -- Apply list of True/False results on Battlefield
  return $ foldr bodyCount battlefield results

    where
      Battlefield a d = battlefield
      attackers = min 3 (a - 1)
      defenders = min 2  d

      diceAndSort :: Int -> Rand StdGen [Int]
      diceAndSort n = (map unDV) . sortDesc <$> dice n
        where
          dice n = replicateM n die
          sortDesc = reverse . sort

      bodyCount victory (Battlefield a d) =
        if victory
        then Battlefield  a   (d-1)
        else Battlefield (a-1) d


-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a < 2 || d <= 0 = return b
  | otherwise       = battle b >>= invade


-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- replicateM 1000 (invade b)
  let victoryCount = length $ filter id (isVictory <$> results)
  return $ fromIntegral victoryCount / 1000
    where
      isVictory :: Battlefield -> Bool
      isVictory (Battlefield a d) = a > d


main = do
  let battlefield = Battlefield 9 9
  print =<< (evalRandIO $ battle battlefield)
  print =<< (evalRandIO $ invade battlefield)
  print =<< (evalRandIO $ successProb battlefield)
