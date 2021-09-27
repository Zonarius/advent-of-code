{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Control.Applicative
import Data.Char (isSpace, isDigit)
import Data.List (find, sort)
import Data.Maybe (isJust, fromJust)
import Data.Functor
import Data.Either
import Debug.Trace
import qualified Data.Set as S

main :: IO ()
main = interact app

app :: String -> String
app = show . app' . parseIt input

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Right x) = x
  justRight (Left x) = error "Parsing failed"

type Group = [Answers]
type Answers = S.Set Char

input :: Parser [Group]
input = answers `sepBy` (endOfLine *> endOfLine)

answers :: Parser [Answers]
answers = answer `sepBy1` endOfLine

answer :: Parser Answers
answer = S.fromList . unpack <$> takeWhile1 (not . isEndOfLine)

uniqueAnswers :: Group -> Int
uniqueAnswers = length . intersections

intersections :: (Foldable f, Ord a) => f (S.Set a) -> S.Set a
intersections = foldl1 S.intersection

app' :: [Group] -> Int
app' = sum . fmap uniqueAnswers