{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

class Scored a where
  score :: a -> Score

instance Scored Score where
  score = id

instance Scored a => Scored (a,b) where
  score = score . fst

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scoreChar :: Char -> Score
scoreChar char
  | l `elem` "AEILNORSTU" = Score 1
  | l `elem` "DG"         = Score 2
  | l `elem` "BCMP"       = Score 3
  | l `elem` "FHVWY"      = Score 4
  | l `elem` "K"          = Score 5
  | l `elem` "JX"         = Score 8
  | l `elem` "QZ"         = Score 10
  | otherwise             = Score 0
    where
      l = toUpper char

scoreString :: String -> Score
scoreString xs = mconcat (scoreChar <$> xs)
