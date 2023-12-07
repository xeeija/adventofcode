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

-- >>> solve $ lines "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
-- 6440

-- rank x = (length (kind x), Down $ numKind x, map (map (head . flip elemIndices "AKQJT98765432")) (kind x))

-- >>> map (kind . head . words) $ lines "AAAAA 765\nAA8AA 684\n23332 28\n23432 220\nA23A4 483\n23456 483"
-- [["AAAAA"],["8","AAAA"],["22","333"],["22","33","4"],["2","3","4","AA"],["2","3","4","5","6"]]

-- >>> solve ["32T3K","T55J5","KK677","KTJJT","QQQJA"]
-- C:\Users\xeeija\Code\adventofcode\2023\07-camel-cards\07-camel-cards.hs:17:16-47: Non-exhaustive patterns in lambda

-- >>> kind $ "AAAAA"
-- >>> kind $ "AA8AA"
-- >>> kind $ "23332"
-- >>> kind $ "23432"
-- >>> kind $ "A23A4"
-- >>> kind $ "23456"
-- ["AAAAA"]
-- ["8","AAAA"]
-- ["22","333"]
-- ["22","33","4"]
-- ["2","3","4","AA"]
-- ["2","3","4","5","6"]

-- >>> kind $ "32T3K"
-- >>> kind $ "KTJJT"
-- >>> kind $ "KK677"
-- >>> kind $ "T55J5"
-- >>> kind $ "QQQJA"
-- ["2","33","K","T"]
-- ["JJ","K","TT"]
-- ["6","77","KK"]
-- ["555","J","T"]
-- ["A","J","QQQ"]


-- >>> ("AAAAA", rank "AAAAA")
-- >>> ("AA8AA", rank "AA8AA")
-- >>> ("23332", rank "23332")
-- >>> ("23432", rank "23432")
-- >>> ("A23A4", rank "A23A4")
-- >>> ("23456", rank "23456")
-- ("AAAAA",(1,Down 5,[0,0,0,0,0]))
-- ("AA8AA",(2,Down 4,[0,0,6,0,0]))
-- ("23332",(2,Down 3,[12,11,11,11,12]))
-- ("23432",(3,Down 2,[12,11,10,11,12]))
-- ("A23A4",(4,Down 2,[0,12,11,0,10]))
-- ("23456",(5,Down 1,[12,11,10,9,8]))

-- >>> ("32T3K", rank "32T3K")
-- >>> ("KTJJT", rank "KTJJT")
-- >>> ("KK677", rank "KK677")
-- >>> ("T55J5", rank "T55J5")
-- >>> ("QQQJA", rank "QQQJA")
-- ("32T3K",(4,Down 2,[11,12,4,11,1]))
-- ("KTJJT",(3,Down 2,[1,4,3,3,4]))
-- ("KK677",(3,Down 2,[1,1,8,7,7]))
-- ("T55J5",(3,Down 3,[4,9,9,3,9]))
-- ("QQQJA",(3,Down 3,[2,2,2,3,0]))
