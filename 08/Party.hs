{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid ((<>))
import Data.Tree


-- Exercise 1
instance Monoid GuestList where
  mempty  = GL [] 0
  (GL e1 f1) `mappend` (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL ([e] ++ es) (empFun e + f)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

exercise1 = do
  print $ glCons e mempty <> gl == glCons e gl
  print $ moreFun gl (glCons e mempty) == gl
    where
      e = Emp "Joe" 5
      gl = GL [Emp "Stan" 9, Emp "Bob" 3] 12


-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root xs) = f root (treeFold f <$> xs)

exercise2 = print $ treeFold (\x xs -> glCons x $ mconcat xs) testCompany


-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss)
  where
    withoutBoss = foldMap (uncurry moreFun) results
    withBoss = glCons boss $ foldMap snd results

exercise3 = do
  print $ nextLevel boss guestLists
    where
      boss = Emp "Joe" 5
      guestLists = [(GL [Emp "Stan" 9] 9, GL [Emp "Bob" 3] 3)]


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

exercise4 = print $ maxFun testCompany


-- Exercise 5
formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines (empName <$> lst)

exercise5 = readFile "company.txt" >>= computeGuestList >>= putStr
  where
    computeGuestList = return . formatGL . maxFun . read


main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
