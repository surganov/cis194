{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified Data.Map as M


-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n k) = eval n + eval k
eval (Mul n k) = eval n * eval k

exercise1 = do
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr expr = case parseExp Lit Add Mul expr of
  Just a -> Just (eval a)
  Nothing -> Nothing

exercise2 = do
  print $ evalStr "(2+3)*4" == Just 20
  print $ evalStr "2+3*4" == Just 14
  print $ evalStr "2+3*" == Nothing


-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

exercise3 = do
  print $ (reify $ mul (add (lit 2) (lit 3)) (lit 4))
    == Mul (Add (Lit 2) (Lit 3)) (Lit 4)


-- Exercise 4
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = if (a > 0) then True else False
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

exercise4 = do
  mapM_ print (testExp :: Maybe Integer)
  mapM_ print (testExp :: Maybe Bool)
  mapM_ print (testExp :: Maybe MinMax)
  mapM_ print (testExp :: Maybe Mod7)


-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VVar Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var x = VVar 42

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add a b = (\vs -> (+) <$> (a vs) <*> (b vs))
  mul a b = (\vs -> (*) <$> (a vs) <*> (b vs))

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


exercise6 = do
  print $ (withVars [("x", 6)] $ add (lit 3) (var "x")) == Just 9
  print $ (withVars [("x", 6)] $ add (lit 3) (var "y")) == Nothing
  print $ (withVars [("x", 6), ("y", 3)]
    $ mul (var "x") (add (var "y") (var "x"))) == Just 54

main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise6
