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

