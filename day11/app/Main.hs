{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Attoparsec.Text
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Control.Applicative
import Data.Char (isSpace, isDigit)
import Data.List (find, sort)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Functor
import Data.Either
import Debug.Trace
import Data.Tuple (swap)
import Data.Foldable (foldl', fold)
import Data.List (inits)
import Data.Array as A

main :: IO ()
main = interact app'

main2 :: IO ()
main2 = do
  inp <- readFile "./testinput"
  putStrLn $ app' inp

app' :: String -> String
app' = show . app . parseIt input

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Left x) = error $ "Parsing failed: " ++ x
  justRight (Right x) = x

data State = Floor
           | Empty
           | Occupied
           deriving (Eq, Ord, Show)

type WaitingRoom = A.Array Int (A.Array Int State)

input :: Parser WaitingRoom
input = toArray <$> row `sepBy` endOfLine where
  row :: Parser (A.Array Int State)
  row = toArray . fmap toState . unpack <$> takeWhile1 (not.isEndOfLine)

toState :: Char -> State
toState '.' = Floor
toState 'L' = Empty
toState '#' = Occupied
toState _   = error "Invalid char"

toArray :: Show a => [a] -> A.Array Int a
toArray xs = A.listArray (0, length xs) (traceShowId xs)

app :: WaitingRoom -> WaitingRoom
app wr = if wr == next then wr else app next where
  next = step wr

step :: WaitingRoom -> WaitingRoom
step wr = 
