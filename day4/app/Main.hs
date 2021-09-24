{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Control.Applicative
import Data.Char (isSpace, isDigit)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Functor
import Data.Either
import Debug.Trace

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

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optionalFields = ["cid"]
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
isValid = (==length requiredFields) . length . filter isValidField

isValidField :: Field -> Bool
isValidField (k,x) = case k of
  "byr" -> parsable byr x
  "iyr" -> parsable iyr x
  "eyr" -> parsable eyr x
  "hgt" -> parsable hgt x
  "hcl" -> parsable hcl x
  "ecl" -> parsable ecl x
  "pid" -> parsable pid x
  _     -> False

parsable :: Parser a -> Text -> Bool
parsable p t = case parseOnly p t of
  Left x -> trace (show x) False
  Right _ -> True

byr :: Parser Int
byr = filterP "Must be in range" (inRange 1920 2002) decimal

iyr :: Parser Int
iyr = filterP "Must be in range" (inRange 2010 2020) decimal

eyr :: Parser Int
eyr = filterP "Must be in range" (inRange 2020 2030) decimal

data HeightUnit = CM | Inch
hgt :: Parser (Int, HeightUnit)
hgt = filterP "invalid Height" validHgt $ (,) <$> decimal <*> (
    "cm" $> CM <|>
    "in" $> Inch
  ) where
    validHgt (x,CM)   = inRange 150 193 x
    validHgt (x,Inch) = inRange 59 76 x

hcl :: Parser Text
hcl = filterP "haircolor must be 6 hexadecimal digits" (hasLength 6) $
  "#" *> takeWhile1 (inClass "0-9a-f")

ecl :: Parser Text
ecl = "amb" <|> "blu" <|> "brn" <|> "gry" <|> "grn" <|> "hzl" <|> "oth"

pid :: Parser Int
pid = readT <$> filterP "pid must be 9 digits" (hasLength 9) (takeWhile1 isDigit)

inRange :: Ord a => a -> a -> a -> Bool
inRange l b x = x >= l && x <= b

readT :: Read a => Text -> a
readT = read . unpack

hasLength :: Int -> Text -> Bool
hasLength i = (==i) . T.length

filterP :: String -> (a -> Bool) -> Parser a -> Parser a
filterP m f p = do
  val <- p
  if f val 
    then return val
    else fail m
