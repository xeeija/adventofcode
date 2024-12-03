import Control.Applicative (liftA2)
import Data.List (subsequences, inits, tails)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve :: String -> Int
solve = length . filter id . map (safe . diff) . parse

parse :: String -> [[Integer]]
parse = map (map read . words) . lines

diff :: Num c => [c] -> [c]
diff xs = zipWith subtract xs (tail xs)

safe :: (Ord a, Num a) => [a] -> Bool
safe xs
  | head xs > 0 = all (\x -> x > 0 && x <= 3) xs
  | head xs <= 0 = all (\x -> x < 0 && x >= -3) xs

-- Part Two

solve2 :: String -> Int
solve2 = length . filter id . map (any (safe . diff) . dampener) . parse

dampener :: [a] -> [[a]]
dampener xs = (filter ((== (length xs - 1)) . length) . subsequences) xs


-- alternative to get sublist options
-- possibilities :: [a] -> [[a]]
-- possibilities xs = xs : zipWith (++) (inits xs) (tail (tails xs))
-- possibilities xs = zip (inits xs) (tail (tails xs))

-- >>> possibilities [7,6,4,2,1]
-- [([],[6,4,2,1]),([7],[4,2,1]),([7,6],[2,1]),([7,6,4],[1]),([7,6,4,2],[])]


-- solve2 = length . filter id . map (any (null . safe' . diff) . dampener) . parse

-- safe' :: (Ord a, Num a) => [a] -> [a]
-- safe' xs
--   | head xs > 0 = filter (\x -> x <= 0 || x > 3) xs
--   | head xs < 0 = filter (\x -> x > 0 || x < -3) xs
--   | head xs == 0 = xs


-- Examples

-- >>> diff [7,6,4,2,1]
-- [-1,-2,-2,-1]

-- >>> solve "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
-- 2

-- >>> solve2 "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
-- 4

-- >>> (parse) "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]

-- >>> (map (diff) . parse) "1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5"
-- [[1,5,1,1],[-2,-1,-4,-1],[2,-1,2,1]]

-- >>> (map (safe' . diff) . parse) "1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5"
-- [[5],[-4],[-1]]

-- >>> (dampener) [1,3,2,4,5]
-- [[1,3,2,4],[1,3,2,5],[1,3,4,5],[1,2,4,5],[3,2,4,5]]

-- >>> (map (length . safe' . diff) . dampener) [1,3,2,4,5]
-- [1,1,0,0,2]

-- >>> map (any (== 0) . map (length . safe' . diff) . dampener) [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
-- [True,False,False,True,True,True]
