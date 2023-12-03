module Main where

import Data.Char (isDigit)
import Data.List (elemIndex)
import Data.Text (pack, unpack, replace)

main = interact $ (++ "\n") . show . (\x -> (calibrate x, calibrate' x))

calibrate :: String -> Integer
calibrate = sum . map (read . firstlast . filter isDigit) . lines

firstlast :: [a] -> [a]
firstlast [x] = [x, x]
firstlast (x : xs) = x : [last xs]
firstlast _ = []


-- Part two

calibrate' :: String -> Integer
calibrate' = sum . map (read . firstlast . filter isDigit . toNumber) . lines . unlines . lines

r' :: String -> String -> String -> String
r' from to = unpack . replace (pack from) (pack to) . pack

toNumber :: String -> String
toNumber = r' "one" "one1one" 
  . r' "two" "two2two" 
  . r' "three" "three3three" 
  . r' "four" "four4four" 
  . r' "five" "five5five" 
  . r' "six" "six6six" 
  . r' "seven" "seven7seven" 
  . r' "eight" "eight8eight" 
  . r' "nine" "nine9nine" 

