{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List.Split

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


-- Exercise 1
instance Functor Parser where
  fmap g (Parser f) = Parser $ fmap (first g) . f

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

exercise1 = do
  print $ first toLower ('A', "BC")
  print $ runParser (satisfy isUpper) "ABC"
  print $ runParser (toLower <$> satisfy isUpper) "ABC"


-- Exercise 2
instance Applicative Parser where
  pure a = Parser $ \xs -> Just (a, xs)
  Parser f <*> Parser g = Parser (\xs -> f xs >>= h)
    where
      h (p, ys) = first p <$> g ys

type Name = String
data Employee = Emp { name :: Name, phone :: String }
  deriving (Show)

parseEmployee = Emp <$> parseName <*> parsePhone
  where
    parsePhone = show <$> posInt
    parseName  = Parser f
      where
        f [] = Nothing
        f xs = case splitOn ", " xs of
            [a, b] -> Just (a, b)
            _      -> Nothing

exercise2 = do
  print $ runParser parseEmployee "John Moe, 2564213"
  print $ runParser parseEmployee "Melissa13"
  print $ runParser (Emp <$> pure "Jessica" <*> show <$> posInt) "8675309"


-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

exercise3 = do
  print $ runParser abParser  "abcdef"
  print $ runParser abParser  "abcdef" == Just (('a','b'),"cdef")
  print $ runParser abParser  "aebcdf" == Nothing
  print $ runParser abParser_ "abcdef" == Just ((),"cdef")
  print $ runParser abParser_ "aebcdf" == Nothing
  print $ runParser intPair "12 34"    == Just ([12,34],"")


-- Exercise 4
instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f xs = runParser p1 xs <|> runParser p2 xs

exercise4 = do
  print $ runParser (char 'a' <|> char 'b') "abc"


-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)

exercise5 = do
  print $ runParser intOrUppercase "342abcd" == Just ((), "abcd")
  print $ runParser intOrUppercase "XYZ"     == Just ((), "YZ")
  print $ runParser intOrUppercase "foo"     == Nothing


main = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
