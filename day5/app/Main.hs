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

main :: IO ()
main = interact app
-- main = print $ app "FBFBBFFRLR"

app :: String -> String
app = show . app' . parseIt input

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Right x) = x
  justRight (Left x) = error "Parsing failed"

data Seat = Seat {
  _row :: Int,
  _col :: Int
}

input :: Parser [Seat]
input = seat `sepBy` endOfLine

seat :: Parser Seat
seat = Seat <$> row <*> col

row :: Parser Int
row = sum <$> sequence [
      bd 64
    , bd 32
    , bd 16
    , bd 8
    , bd 4
    , bd 2
    , bd 1]

col :: Parser Int
col = sum <$> sequence [
      bd 4
    , bd 2
    , bd 1]

bd :: Int -> Parser Int
bd x = ((char 'L' <|> char 'F') $> 0)
   <|> ((char 'B' <|> char 'R') $> x)

seatId :: Seat -> Int
seatId (Seat r c) = (r * 8) + c

app' :: [Seat] -> Int
app' = mySeatId . findSeat . succList . fmap seatId where
  findSeat = fromJust . find (\(x,y) -> y - x > 1)
  mySeatId (x,_) = x + 1

succList :: Ord a => [a] -> [(a,a)]
succList xs = zip sorted (tail sorted) where
     sorted = sort xs
