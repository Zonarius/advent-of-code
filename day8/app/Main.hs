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
import Data.Tuple (swap)
import Data.Foldable (foldl')
import Data.IntSet as IS
import Data.Array as A

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

data Program = Program {
  _visited         :: IS.IntSet,
  _nextInstruction :: Int,
  _acc             :: Int,
  _instructions    :: A.Array Int Instruction
} deriving Show

data Instruction = Nop
                 | Acc Int
                 | Jmp Int
                 deriving Show

input :: Parser Program
input = Program mempty 0 0 . toArray <$> instruction `sepBy1` endOfLine <?> "input"

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

instruction :: Parser Instruction
instruction = ((Nop <$  ("nop " *> signed decimal)) <?> "nop")
          <|> ((Acc <$> ("acc " *> signed decimal)) <?> "acc")
          <|> ((Jmp <$> ("jmp " *> signed decimal)) <?> "jmp")


app' :: Program -> Int
app' = _acc . run where
  run p@(Program vis i _ _) = if IS.member i vis
  then p
  else run (step p)

step :: Program -> Program
step (Program vis i acc ins) = case ins ! i of
  Nop   -> Program (IS.insert i vis) (i + 1) acc ins
  Acc x -> Program (IS.insert i vis) (i + 1) (acc + x) ins
  Jmp x -> Program (IS.insert i vis) (i + x) acc ins