import Data.List (intersect, sort)
import Data.List.Split (chunksOf)

main :: IO ()
main = interact $ show . (\x -> (solve x, solve3 x))

solve :: String -> Int
solve = sum . map rucksack . lines

rucksack :: [Char] -> Int
rucksack = prio . fromEnum . head . uncurry intersect . halve

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

prio :: (Ord a, Num a) => a -> a
prio x = if x > 90 then x - 96 else x - 38

solve3 :: String -> Int
solve3 = sum . map (prio . fromEnum . head . (\[a, b, c] -> a `intersect` b `intersect` c)) . chunksOf 3 . lines
