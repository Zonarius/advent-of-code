{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Tuple (swap)
import Data.Foldable (foldl', fold)
import Data.List (inits)

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


input :: Parser [Int]
input = decimal `sepBy` endOfLine

preamble = 25

findInvalid :: [Int] -> Int
findInvalid inp = fromJust $ snd <$> find (not.fst) (isValid <$> [0..]) where
  isValid :: Int -> (Bool, Int)
  isValid i 
    | i < preamble = (True, inp !! i)
    | otherwise    = ((inp !! i) `elem` sums, inp !! i) where
      sums = sums' (drop (i - preamble) inp)
      sums' inp' = sums'' (tail inp') (init inp') ++ sums'' (init inp') (tail inp')
      sums'' xs ys = do
        x <- xs
        y <- ys
        return $ x + y

app :: [Int] -> Int
app inp = findSumSet inp where
  inv = findInvalid inp
  findSumSet xs = let smallestSum = find (\(_,sum) -> sum >= inv) (tail $ zip (tail $ inits xs) (scanl1 (+) xs)) in
    case smallestSum of
      Just ss -> if snd ss == inv
        then uncurry (+) (minmax $ fst ss)
        else findSumSet $ tail xs
      Nothing -> findSumSet $ tail xs

minmax :: (Ord a) => [a] -> (a,a)
minmax xs = (minimum xs, maximum xs)