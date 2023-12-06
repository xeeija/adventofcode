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

