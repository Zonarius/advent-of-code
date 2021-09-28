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
import qualified Data.IntMap as IM
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
app xs = let (x:xs') = sort (0:xs) in
  app' xs' (IM.singleton x 1) where
  app' [] _       = error "no bueno"
  app' [x] set    = addUp x set
  app' (x:xs) set = app' xs $ IM.insert x (addUp x set) set
  addUp x set = sum (fmap (\i -> fromMaybe 0 $ IM.lookup (x-i) set) [1..3])