module Main where

import Data.Text (Text, pack, unpack)
import Data.Attoparsec.Text
import Data.Maybe

data Password = Password {
  countRange :: (Int,Int),
  required   :: Char,
  password   :: Text
} deriving Show

main :: IO ()
main = interact day2

day2 :: String -> String
day2 = show . day2' . parseInput

day2' :: [Password] -> Int
day2' = length . filter isValid

-- isValid :: Password -> Bool
-- isValid (Password (lb,ub) req pw) = 
--   let c = length $ filter (==req) (unpack pw) in
--     c >= lb && c <= ub

isValid :: Password -> Bool
isValid (Password (i1,i2) req pw) = 
  let pws = unpack pw 
      isCorrect = (==req) in
        isCorrect (pws !! (i1 - 1)) /=
        isCorrect (pws !! (i2 - 1))

parseInput :: String -> [Password]
parseInput = uRight . parseOnly lineP . pack where
  lineP = pwParser `sepBy` endOfLine
  pwParser = Password <$> range 
                    <*  skipSpace
                    <*> anyChar
                    <* char ':'
                    <*  skipSpace
                    <*> takeTill isEndOfLine
                    
  range = (,) <$> decimal <* char '-' <*> decimal

uRight :: Show a => Either a b -> b
uRight (Right b) = b
uRight (Left a) = error $ show a