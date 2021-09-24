module Main where

import Data.Attoparsec.Text
import Data.Text ( Text, pack )
import Control.Applicative
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (isJust)

main :: IO ()
main = interact app

app :: String -> String
app = show . app' . parseIt fields

parseIt :: Parser a -> String -> a
parseIt p = justRight . parseOnly p . pack where
  justRight (Right x) = x
  justRight (Left x) = error "Parsing failed"

type Field = (Text,Text)
type Passport = [Field]

requiredFields = pack <$> ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optionalFields = pack <$> ["cid"]
fieldKeys = requiredFields ++ optionalFields

fields :: Parser [Passport]
fields = passport `sepBy` endOfLine

passport :: Parser Passport
passport = field `sepBy` space

field :: Parser Field
field = (,) <$> fieldKey <* char ':' <*> fieldValue where
  fieldKey = choice (string <$> fieldKeys)
  fieldValue = takeTill isSpace

app' :: [Passport] -> Int
app' = length . filter isValid

isValid :: Passport -> Bool
isValid = (==length requiredFields) . length . filter isRequired

isRequired :: Field -> Bool
isRequired (k,_) = isJust $ find (==k) requiredFields
