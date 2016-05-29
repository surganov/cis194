module Golf where

import Data.List
import Data.List.Split


-- Exercise 1. Hopscotch
skips :: [a] -> [[a]]
skips xs = zipWith every [1..(length xs)] (repeat xs)
  where
    every n xs = extractBy orderedPairs
      where
        extractBy    = fmap fst . filter ((== n) . snd)
        orderedPairs = zip xs $ cycle [1..n]

exercise1 = do
  print $ skips "ABCD"       == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips [1]          == [[1]]
  print $ skips [True,False] == [[True,False], [False]]


-- Exercise 2. Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldMap maxima $ triples xs
  where
    triples xs = zip3 xs (drop 1 xs) (drop 2 xs)
    maxima (x,y,z) = if (x < y) && (y > z) then [y] else []

exercise2 = do
  print $ localMaxima [2,9,5,6,1] == [9,6]
  print $ localMaxima [2,3,4,1,5] == [4]
  print $ localMaxima [1,2,3,4,5] == []


-- Exercise 3. Histogram
histogram :: [Integer] -> String
histogram nums = unlines . transpose $ reverse . createRow <$> occurrences
  where
    occurrences = (\(x:xs) -> (show x, length xs)) <$> (group . sort . (++ [0..9])) nums
    maxHeight   = maximum $ snd <$> occurrences
    createRow (x,y) = concat $ legend ++ bar where
      legend = [x ++ "="]
      bar    = take maxHeight (replicate y "*" ++ repeat " ")

exercise3 = do
  putStr "\n"
  putStrLn $ histogram [1,1,1,5]
  putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]


main = do
  exercise1
  exercise2
  exercise3
