import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)


-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

exercise1 = print $ take 30 fibs1


-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

exercise2 = print $ take 30 fibs2


-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show x = foldr (\x acc -> (show x) ++ "," ++ acc) "…" $ take 20 (streamToList x)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

streamFromList xs = case cycle xs of
  (x:xs) -> Cons x (streamFromList xs)

exercise3 = do
  print $ streamFromList [0..]
  print $ streamFromList [1..5]
  print $ streamFromList ['a'..'z']


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = streamFromList [x]

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

exercise4 = do
  print $ streamRepeat 42
  print $ streamMap (*2) (streamFromList [1..])
  print $ streamFromSeed (*2) 1


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams zeroes powersOfTwo
  where
    interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))
    zeroes = streamRepeat 0
    powersOfTwo = streamFromList $ toInteger . highestPowerOfTwo <$> [2,4..]
    highestPowerOfTwo = length . rightmostZeroes . toBitstring
      where
        toBitstring n = showIntAtBase 2 intToDigit n ""
        rightmostZeroes = takeWhile (=='0') . reverse

exercise5 = do
  print $ nats
  print $ ruler


main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
