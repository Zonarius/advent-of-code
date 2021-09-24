module Main where
import Data.Array.IArray
import Data.Attoparsec.Text
import Data.Text ( Text, pack )
import Control.Applicative

data Field = Empty | Tree deriving Show
type GameMap = Array Int GameRow
type GameRow = Array Int Field

main :: IO ()
main = interact day3

day3 :: String -> String
day3 = show . day3' . parseIt parseMap

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Right x) = x
  justRight (Left x) = error "Parsing failed"

parseMap :: Parser GameMap
parseMap = zeroArr <$> parseRow `sepBy` endOfLine

parseRow :: Parser GameRow
parseRow = zeroArr <$> many1 (toField <$> (char '.' <|> char '#'))

toField :: Char -> Field
toField '.' = Empty
toField '#' = Tree
toField x   = error $ "Unknown map element " ++ show x

zeroArr :: [a] -> Array Int a
zeroArr xs = listArray (0, length xs - 1) xs

day3' :: GameMap -> Int
day3' map = day3'' 0 0 where
  day3'' x y | x > xBound = 0
  day3'' x y              = fieldValue x y + day3'' (x+1) ((y + 3) `mod` (yBound + 1))
  xBound = snd $ bounds map
  yBound = snd $ bounds $ map ! 0
  fieldValue x y = case map ! x ! y of
    Empty -> 0
    Tree  -> 1
