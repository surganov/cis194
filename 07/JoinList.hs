{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Sized
import Scrabble
import Data.Monoid
import StringBuffer
import Buffer
import Editor
import System.IO.Unsafe


-- Exercise 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y
infixr 5 +++

tag :: Monoid m => JoinList m a -> m
tag jl = case jl of
  Empty        -> mempty
  Single m _   -> m
  Append m _ _ -> m

someJoinList =
  Append (Product 210)
    (Append (Product 30)
      (Single (Product 5) 'y')
      (Append (Product 6)
        (Single (Product 2) 'e')
        (Single (Product 3) 'a')))
    (Single (Product 7) 'h')

exercise1 = do
  print . getProduct . tag $ someJoinList
  print . getProduct . tag $ Empty
  print $ (Empty :: JoinList (Product Int) Char)
  print $ (Empty +++ Empty :: JoinList (Product Int) Char)
  print $ Single (Product 2) 'a' +++ Single (Product 3) 'b'


-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[]     !!?      _    = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl = jlToList jl !!? i

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' 0 (Single _ a) = Just a
indexJ' n (Append _ jl1 jl2)
  | n < size1 = indexJ' n jl1
  | otherwise = indexJ' (n - size1) jl2
    where
      size1 = getSize . size . tag $ jl1
indexJ' _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl@(Single _ _)
  | n <= 0 = jl
dropJ n jl@(Append m jl1 jl2)
  | n >= size0 = Empty
  | n < size1  = (dropJ n jl1) +++ jl2
  | n > 0      = dropJ (n - size1) jl2
  | otherwise  = jl
    where
      size0 = getSize . size $ m
      size1 = getSize . size . tag $ jl1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl@(Single _ _)
  | n > 0 = jl
takeJ n jl@(Append m jl1 jl2)
  | n >= size0 = jl
  | n >= size1 = jl1 +++ takeJ (n-size1) jl2
  | n > 0 = takeJ n jl1
    where
      size0 = getSize . size $ m
      size1 = getSize . size . tag $ jl1
takeJ _ _ = Empty

az :: JoinList Size Char
az = foldr1 (+++) $ Single (Size 1) <$> ['a'..'z']

exercise2 = do
  print $ indexJ 7 az == indexJ' 7 az
  print $ Just 'a'    == indexJ' 0  az
  print $ Nothing     == indexJ' 42 az
  print $ Nothing     == indexJ' (-3) az
  print $ ['f'..'z']  == jlToList (dropJ 5 az)
  print $ ['a'..'e']  == jlToList (takeJ 5 az)


-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

exercise3 = print $ scoreLine "yay " +++ scoreLine "haskell!"


-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString l = unlines (jlToList l)

  fromString xs = foldr1 (+++) $ createLine <$> lines xs
    where
      createLine s = Single (scoreString s, Size 1) s

  line = indexJ'

  replaceLine _ "" b = b
  replaceLine n l b  = case indexJ' n b of
    Nothing -> b
    Just _ -> takeJ n b +++ newline +++ dropJ (n + 1) b
      where
        newline = fromString l :: JoinList (Score, Size) String

  numLines = getSize . size . tag
  value    = getScore . score . tag

exercise4 = runEditor editor $ (fromString
  "A\nChristmas\nCarol" :: JoinList (Score, Size) String)


main = do
  exercise1
  exercise2
  exercise3
  exercise4
