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
import qualified Data.Map as M
import Data.Map ((!))

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

app :: [Int] -> Int
app xs = let sorted = sort $ 0 : (maximum xs + 3) : xs
             diffs  = zipWith (-) (tail sorted) sorted
             tly    = tally diffs
             in tly ! 1 * tly ! 3

tally :: (Ord a, Eq a) => [a] -> M.Map a Int
tally = foldl' fld mempty where
  fld set x = M.insert x (1 + fromMaybe 0 (M.lookup x set)) set