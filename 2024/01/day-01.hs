import Control.Applicative (liftA2)
import Data.List.Split (splitOn)
import Data.List (transpose, sort, group, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve :: String -> Int
solve = sum . map (\(a:b:_) -> abs (a - b)) . transpose . map sort . parse

parse :: String -> [[Int]]
parse = transpose . map (map read . words) . lines

-- Part Two

solve2 :: String -> Int
solve2 = sum . (\(xs:ys:_) -> map (similarity (occurences ys)) xs) . parse

-- occurences :: String -> [Int]
occurences :: [Int] -> [(Int, Int)]
occurences = map (liftA2 (,) head length) . group . sort
-- occurences = map (\xs -> (head xs, length xs)) . group . sort . (!! 1) . parse

similarity :: (Num a, Eq a) => [(a, a)] -> a -> a
similarity occ x = x * fromMaybe 0 (lookup x occ)

