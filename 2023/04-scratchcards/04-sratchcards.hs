import Control.Applicative (liftA2)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

-- solve :: String -> Integer
-- solve = sum . map (score . length . winning . parse) . lines

-- -1 -> (+ (-1)) or flip (-) 1 or subtract 1
solve :: String -> Integer
solve = sum . map (floor . (2 ^^) . subtract 1 . length . winning . parse) . lines

parse :: [Char] -> [[Integer]]
parse = map (map read . words) . splitOn " | " . last . splitOn ": "

-- score :: Int -> Int
-- score 0 = 0
-- score 1 = 1
-- score x = 2 ^ (x - 1)

winning :: Eq a => [[a]] -> [a]
winning (xs:winning:_) = filter (`elem` winning) xs

-- >>> parse "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- [[83,86,6,31,17,9,48,53],[41,48,83,86,17]]

-- >>> solve "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
-- 13

-- Part Two

-- parse = map (map read . words) . splitOn " | " . last . splitOn ": "

solve2 :: String -> Int
solve2 = sum . (\xs -> addCopies 1 (replicate (length xs) 1) xs) . map (length . winning . parse) . lines

-- >>> solve2 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
-- 30


addCopies :: Int -> [Int] -> [Int] -> [Int]
addCopies i acc [] = acc
addCopies i acc (w:wins) = addCopies (i + 1) (calculate acc (replicate w copy) i) wins 
  where copy = if i < length acc then acc !! (i - 1) else 0


calculate :: [Int] -> [Int] -> Int -> [Int]
calculate cards xs i = zipWith (+) cards (replicate i 0 ++ xs ++ [0,0..])

-- addCopies' :: [Int] -> [[Int]] -> Int -> [[Int]]
-- addCopies' [] acc i = acc
-- addCopies' (w:wins) acc i = addCopies' wins (calculate (head acc) (replicate w copyIndex) i : acc) (i + 1)
--   where copyIndex = if i < length (head acc) then head acc !! i else 0

-- >>> calculate cards (replicate 4 1) 1
-- [1,2,2,2,2,1]

--                |            |         +1

-- >>> calculate [1,2,2,2,2,1] (replicate 2 2) 2
-- [1,2,4,4,2,1]

--                                        w xi indent
-- >>> calculate [1,2,4,4,2,1] (replicate 2 4) 3
-- [1,2,4,8,6,1]

-- >>> addCopies 1 [1,1,1,1,1,1] [4,2,2,1,0,0] 
-- >>> addCopies 1 [1,1,1,1,1,1] [4,2,2,1,0,0] 
-- >>> addCopies' [4,2,2,1,0,0] [[1,1,1,1,1,1]] 1
-- [1,2,4,8,14,1]
-- [1,2,4,8,14,1]
-- [[1,2,4,8,12,1],[1,2,4,8,12,1],[1,2,4,8,12,1],[1,2,4,8,6,1],[1,2,4,4,2,1],[1,2,2,2,2,1],[1,1,1,1,1,1]]

-- zip3 [1..] [4,2,2,1,0,0] (reverse [[1,2,4,7,9,1],[1,2,4,7,9,1],[1,2,4,7,9,1],[1,2,4,7,5,1],[1,2,4,4,2,1],[1,2,2,2,2,1],[1,1,1,1,1,1]])

-- >>> zip3 [1..] [4,2,2,1,0,0] (reverse [[1,2,4,8,12,1],[1,2,4,8,12,1],[1,2,4,8,12,1],[1,2,4,8,6,1],[1,2,4,4,2,1],[1,2,2,2,2,1],[1,1,1,1,1,1]])
-- [(1,4,[1,1,1,1,1,1]), (2,2,[1,2,2,2,2,1]), (3,2,[1,2,4,4,2,1]), (4,1,[1,2,4,8,6,1]), (5,0,[1,2,4,8,12,1]), (6,0,[1,2,4,8,12,1])]

-- addCopies :: [Int] -> [Int] -> Int -> [Int]
-- addCopies [] acc i = acc
-- addCopies (w:wins) acc i = addCopies wins (calculate acc (replicate w copy) i) (i + 1)
--   where copy = if i < length acc then acc !! (i - 1) else 0

-- What solve2 is doing
--   [4,2,2,1, 0,0] -- # winning numbers
--   [1,1,1,1, 1,1] -- start with 1 for every card
-- + [> 1 1 1  1  ] -- card 1: 4
--   [1,2,2,2, 2,1]
-- + [  > 2 2     ] -- card 2: 2
--   [1,2,4,4, 2,1]
-- + [    > 4  4  ] -- card 3: 2
--   [1,2,4,8, 6,1]
-- + [      >  8  ] -- card 4: 1
--   [1,2,4,8,14,1]
-- + [            ] -- card 5: 0
--   [1,2,4,8,14,1]
-- + [            ] -- card 6: 0
--   [1,2,4,8,14,1]

