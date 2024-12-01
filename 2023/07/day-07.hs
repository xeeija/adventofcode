import Control.Applicative (liftA2, liftA3)
import Data.List (elemIndices, group, sort, sortBy)
import Data.Ord (Down(Down), comparing)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve . lines

solve :: [String] -> Integer
solve = sum . zipWith (*) [1..] . map (read . (!! 1)) . sortBy (comparing (Down . rank . head)) . map words

rank :: String -> (Int, Down Int, [Int])
rank = liftA3 (,,) (length . kind) (Down . numKind) ordKind

-- rank x = (length (kind x), Down (numKind x), ordKind x)

kind :: String -> [String]
kind = group . sort

numKind :: String -> Int
numKind = maximum . map length . kind

ordKind :: [Char] -> [Int]
ordKind = map (head . flip elemIndices "AKQJT98765432")

-- Alternative for kind: get first 2 "lengths" instead of only maximum (and compare additional things)

-- Part Two

-- joker :: String -> [Int]
-- joker = map (liftA2 (,) (uncurry elemIndices) id) . zip "AKQT98765432" . repeat
joker = concat . zipWith (\x y -> replaceJoker y x ('J' `elemIndices` y)) "AKQT98765432" . repeat

-- replaceJoker :: String -> Char -> [Int] -> [String]
-- replaceJoker :: [a] -> a -> [Int] -> [[a]]
replaceJoker hand c = map (\x -> take x hand ++ [c] ++ drop (x + 1) hand)
