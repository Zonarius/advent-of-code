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
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.Tuple (swap)
import Data.Foldable (foldl')
import qualified IncMap as IM

main :: IO ()
main = interact app

main2 :: IO ()
main2 = do
  inp <- readFile "./testinput"
  putStrLn $ app inp

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
                 mRule (Rule from to) = (from, (), swap <$> to)
                 (graph,nodeFromVertex,vertexFromKey) = graphFromEdges nodeList
                 myBag = BagType "shiny" "gold" in
  countBags graph (vertexFromKey myBag) - 1 where
    countBags gr n = 1 + foldl' (\sum (suc,times) -> sum + (times * countBags gr suc)) 0 (lsuc gr n)

graphFromEdges :: (Ord node, Eq node) => [(node, nlab, [(node,elab)])] -> (Gr nlab elab, Node -> node, node -> Node)
graphFromEdges inp = (graph,nodeFromVertex,vertexFromKey) where
  (nodes, edges, map) = foldl' fld ([], [], IM.empty) inp
  graph               = mkGraph nodes edges
  nodeFromVertex i    = IM.getValue i map
  vertexFromKey  x    = IM.getIndex x map
  fld (nodes, edges, map) (node, nlab, newEdges) =
    let map' = foldl' (flip IM.insert) map (node : (fst <$> newEdges))
        nodes' = (IM.getIndex node map', nlab) : nodes
        edges' = foldl' fld2 edges newEdges
        fld2 edges (to, elab) = (IM.getIndex node map', IM.getIndex to map', elab) : edges
        in
      (nodes', edges', map')
