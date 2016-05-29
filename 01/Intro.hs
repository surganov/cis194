import Data.Char (digitToInt)


-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
           | otherwise = (toInteger . digitToInt) <$> show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

exercise1 = do
  print $ toDigits 1234 == [1,2,3,4]
  print $ toDigitsRev 1234 == [4,3,2,1]
  print $ toDigits 0 == []
  print $ toDigits (-17) == []


-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

exercise2 = do
  print $ doubleEveryOther [8,7,6,5] == [16,7,12,5] 
  print $ doubleEveryOther [1,2,3] == [1,4,3]


-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . fmap (sum . toDigits)

exercise3 = do
  print $ sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5


-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

exercise4 = do
  print $ validate 4012888888881881 == True
  print $ validate 4012888888881882 == False


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1, p2)]
hanoi numDiscs p1 p2 p3 =
  hanoi (numDiscs - 1) p1 p3 p2 ++ hanoi 1 p1 p2 p3 ++ hanoi (numDiscs - 1) p3 p2 p1

exercise5 = do
  print $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]


main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
