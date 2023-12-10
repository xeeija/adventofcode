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

-- >>> next [1,3,6,10,15,21]
-- [21,6,1,0,0,0]

-- >>> flip (scanl (-)) [1,3,6,10,15,21] 0
-- [0,-1,-4,-10,-20,-35,-56]

-- solve2 :: [String] -> Int
-- solve2 = sum . map (predict . reverse . map read . words)

-- solve xs = zipWith (-) (tail xs) xs

-- >>> predict [21,15,10,6,3,1]
-- 28

-- >>> solve ["0 3 6 9 12 15","1 3 6 10 15 21","10 13 16 21 30 45"]
-- >>> solve ["15 12 9 6 3 0", "21 15 10 6 3 1", "45 30 21 16 13 10"]
-- >>> solve2 ["0 3 6 9 12 15","1 3 6 10 15 21","10 13 16 21 30 45"]
-- 114
-- 2
-- 2

-- >>> solve2 [[0,3,6,9,12,15],[1,3,6,10,15,21],[10,13,16,21,30,45]]
-- [-3,0,5]
