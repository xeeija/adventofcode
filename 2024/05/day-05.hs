import GHC.Base (liftA2)
import Data.List (intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve = sumMiddle . correctOrder

-- solve = map (\x -> flip (!!) (div (length x))) . correctOrder
-- solve = liftA2 (\ru up -> filter (correct ru) up) rules updates

-- correctOrder :: [Char] -> [[Integer]]
correctOrder = parse (filter . correct)

-- correctOrder = liftA2 (filter . correct) rules updates . splitOn "\n\n"

-- parse :: ([[Integer]] -> [[Integer]] -> c) -> [Char] -> c
parse fn = liftA2 fn rules updates . splitOn "\n\n"

-- sumMiddle :: [[Integer]] -> Integer
sumMiddle = sum . map (\x -> x !! div (length x) 2)

-- rules :: String -> [[Integer]]
rules = map (map read . splitOn "|") . lines . head -- . splitOn "\n\n"

-- updates :: [String] -> [[Integer]]
updates = map (map read . splitOn ",") . lines . (!! 1)

-- rules -> pages -> bool
-- correct :: [Int] -> Int -> [Int] -> Bool
-- correct after x xs = x `notElem` after 
-- correct xs = [before x xs | x <- xs]

-- correct :: Eq a => [[a]] -> [a] -> Bool
correct ru xs = all null [filterRules x ru `intersect` takeWhile (/= x) xs | x <- xs]

-- correct' xs ru = [(x, filterRules x ru, before x xs) | x <- xs]

-- filterRules :: Eq b => b -> [[b]] -> [b]
filterRules x = map (!! 1) . filter ((== x) . head)

-- type RuleSet = (Integer, [Integer])

-- rulesMap :: [RuleSet] -> [[Int]] -> [RuleSet]
-- rulesMap acc xs = [(head x, [x !! 1]) | x <- xs]

