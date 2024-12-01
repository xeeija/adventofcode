import Control.Applicative (liftA2)
import Data.List.Split (splitOn)
import Data.List (transpose, sort, group, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, fromMaybe)

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

-- Examples

-- >>> solve "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
-- 11

-- >>> parse "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
-- [[3,4,2,1,3,3],[4,3,5,3,9,3]]

-- >>> occurences [4,3,5,3,9,3]
-- [(3,3),(4,1),(5,1),(9,1)]

-- >>> solve2 "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
-- 31
