import Control.Applicative (liftA2)
import Data.List (transpose)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2 . map (drop 1 . words) . lines

solve :: [[String]] -> Int
solve = product . map records . transpose . map (map read)

solve2 :: [[String]] -> Int
solve2 = records . map (read . foldr1 (++))

records :: [Int] -> Int
records (x:y:_) = length [n * (x - n) | n <- [1..x-1], n * (x - n) > y]

-- >>> solve . map (drop 1 . words) . lines $ "Time:        35     93     73     66\nDistance:   212   2060   1201   1044"
-- 114400

-- >>> records [15,40]
-- 8

-- Part Two

-- >>> map (foldr1 (++) . drop 1 . words) . lines $ "Time:        7     15     30\nDistance:   9   40   200"
-- >>> length . solve2 . map (drop 1 . words) . lines $ "Time:        7     15     30\nDistance:   9   40   200"
-- ["71530","940200"]
-- 71503

-- >>> length . solve2 . map (drop 1 . words) . lines $ "Time:        35     93     73     66\nDistance:   212   2060   1201   1044"
-- 21039729
