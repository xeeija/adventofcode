import Control.Applicative (liftA2, liftA3)
import Data.List (nub, sort, group, sortBy, elemIndices, maximumBy)
import Data.Ord (comparing, Down (Down))
import Data.Maybe (catMaybes)
import Data.Function (on)

main :: IO ()
main = interact $ (++ "\n") . show . solve . lines

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


-- solve = sortBy (comparing (\(a:_, i) -> Down (rank a))) . flip zip [1..] . map words
