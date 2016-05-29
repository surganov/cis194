import Data.List


-- Exercise 1. Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product
  . map (subtract 2)
  . filter even

fun2' :: Integer -> Integer
fun2' = sum
  . filter even
  . takeWhile (> 1)
  . iterate (\n ->
      if even n
        then n `div` 2
        else 3 * n + 1)

exercise1 = do
  print $ fun1 [6, 18, 4] == fun1' [6, 18, 4]
  print $ fun2 10 == fun2' 10


-- Exercise 2. Folding with trees
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

treeLevel :: Tree a -> Integer
treeLevel Leaf           = 0
treeLevel (Node n _ _ _) = n

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr treeInsert Leaf
  where
    treeInsert x Leaf = Node 0 Leaf x Leaf
    treeInsert x (Node n left root right)
      | left > right = Node (treeLevel newRight + 1) left root newRight
      | otherwise = Node (treeLevel newLeft + 1) newLeft root right
        where
          newRight = treeInsert x right
          newLeft = treeInsert x left

exercise2 = do
  print $ foldTree "ABCDEFGHIJ"
  print $ foldTree "ABCDEFGHIJ" ==
    Node 3
      (Node 2
        (Node 0 Leaf 'F' Leaf)
        'I'
        (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
      'J'
      (Node 2
        (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
        'H'
        (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))


-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = odd . foldl countTrue 0
  where
    countTrue acc x = if x then acc + 1 else acc

map' :: (a -> b) -> [a] -> [b]
map' f = foldr foo []
  where
    foo x acc = f x : acc

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

exercise3 = do
  print $ xor [False, True, False] == True
  print $ xor [False, True, False, False, True] == False
  print $ map (*2) [1..5] == map' (*2) [1..5]
  print $ foldl (-) 0 [1..10] == myFoldl (-) 0 [1..10]
  print $ foldl addBang "" "ABC" == myFoldl addBang "" "ABC"
    where addBang = (\acc x -> acc ++ [x] ++ "!")


-- Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (+1) . (*2) <$> [1..n] \\ nonPrimes
  where
    nonPrimes = takeWhile (<n) $ map (uncurry ij2ij) $ cartProd [1..] [1..]
      where
        ij2ij i j = i + j + 2 * i * j

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

exercise4 = do
  print $ sieveSundaram 100


main = do
  exercise1
  exercise2
  exercise3
  exercise4
