module Main where
import Data.List
import Data.Maybe
import Debug.Trace

main :: IO ()
main = interact day1

day1 :: String -> String 
day1 = show . day1' . map read . lines

day1' :: [Int] -> Int
day1' xs = head $ do
  let xs' = zip [1..] xs
  (xi, x) <- xs'
  (yi, y) <- xs'
  (zi, z) <- xs'
  if xi == yi || yi == zi || xi == zi || x + y + z /= 2020 then [] else
    return $ x * y * z

-- day1' :: [Int] -> Int
-- day1' xs = head $ do
--   let xs' = zip [1..] xs
--   (xi, x) <- xs'
--   (yi, y) <- xs'
--   if xi == yi || x + y /= 2020 then [] else
--     return $ traceShowId x * traceShowId y