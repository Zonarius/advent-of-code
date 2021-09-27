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
import Data.Graph (graphFromEdges, transposeG, reachable)

main :: IO ()
main = interact app

app :: String -> String
app = show . app' . parseIt input

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Right x) = x
  justRight (Left x) = error "Parsing failed"

data BagType = BagType {
  _texture  :: Text,
  _color    :: Text
} deriving (Show, Eq, Ord)

data Rule = Rule {
  _container :: BagType,
  _contains  :: [(Int, BagType)]
} deriving Show

input :: Parser [Rule]
input = rule `sepBy1` endOfLine 

rule :: Parser Rule
rule = Rule <$> bagType
            <*  " contain "
            <*> containsList
            <*  "."

bagType :: Parser BagType
bagType = BagType <$> takeTill isSpace
                  <*  space
                  <*> takeTill isSpace
                  <* space
                  <*  ("bags" <|> "bag")

containsList :: Parser [(Int, BagType)]
containsList = (contains `sepBy1` ", ")
           <|> ("no other bags" $> [])

contains :: Parser (Int, BagType)
contains = (,) <$> decimal
               <*  space
               <*> bagType

app' :: [Rule] -> Int
app' rules = let nodeList = mRule <$> rules
                 mRule (Rule from to) = (from, from, snd <$> to)
                 (graph',nodeFromVertex,vertexFromKey) = graphFromEdges nodeList
                 graph = transposeG graph'
                 myBag = BagType "shiny" "gold" in
  length (reachable graph (fromJust $ vertexFromKey myBag)) - 1
