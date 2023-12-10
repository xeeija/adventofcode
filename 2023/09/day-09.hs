import Control.Applicative (liftA2)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2 . map read . words . lines

solve :: [[Int]] -> Int
solve = sum . map predict

predict :: [Int] -> Int
predict xs = if all (== 0) xs then 0 else last xs + predict (zipWith (-) (tail xs) xs)

-- Part Two

solve2 = map (predict . reverse)

-- alternative
next :: [Int] -> Int
next = sum . foldl (flip (scanl (-))) []

