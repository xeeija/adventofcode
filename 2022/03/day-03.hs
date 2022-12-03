import Data.List (sort)
import Data.List.Split (chunksOf)

main = interact $ show . (\x -> (solve x, solve3 x))

solve = sum . map rucksack . lines

rucksack = prio . fromEnum . duplicate . mapPair sort . halve

halve xs = splitAt (length xs `div` 2) xs

mapPair f (a, b) = (f a, f b)

duplicate ([a], _) = a
duplicate (_, [b]) = b
duplicate (a : as, b : bs)
  | a == b = a
  | a < b = duplicate (as, b : bs)
  | otherwise = duplicate (a : as, bs)

prio x = if x > 90 then x - 96 else x - 38

-- Part 2

solve3 = sum . map (prio . fromEnum . badge . map sort) . chunksOf 3 . lines

badge :: [String] -> Char
badge ([a] : _) = a
badge (_ : [b] : _) = b
badge (_ : _ : [c] : _) = c
badge ((a : as) : (b : bs) : (c : cs) : _)
  | a == b && b == c = a
  | min3 == a = badge [as, b : bs, c : cs]
  | min3 == b = badge [a : as, bs, c : cs]
  | otherwise = badge [a : as, b : bs, cs]
  where
    min3 = minimum [a, b, c]

