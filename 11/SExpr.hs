{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

exercise1 = do
  print $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
  print $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"  == Just ("ABC","dEfgH")
  print $ runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" == Just ("","abcdeFGh")
  print $ runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"  == Nothing

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

exercise2 = do
  print $ runParser spaces "   abc"    == Just ("   ", "abc")
  print $ runParser ident "foobar baz" == Just ("foobar"," baz")
  print $ runParser ident "foo33fA"    == Just ("foo33fA","")
  print $ runParser ident "2bad"       == Nothing
  print $ runParser ident ""           == Nothing

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
  where
    parseAtom = A <$> (I <$> ident <|> N <$> posInt)
    parseComb = Comb <$> (char '(' *> many parseSExpr <* char ')')

exercise3 = do
  print $ runParser (spaces *> posInt) "     345" == Just (345,"")
  print $ runParser parseSExpr "5"
  print $ runParser parseSExpr "foo3"
  print $ runParser parseSExpr "(bar (foo) 3 5 874)"
  print $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
  print $ runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"

main = do
  exercise1
  exercise2
  exercise3
