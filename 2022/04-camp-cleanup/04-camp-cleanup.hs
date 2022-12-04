import Data.List (intersect)
import Data.List.Split (linesBy)

main = interact $ show . (\x -> (solve x, solve2 x))

solve = length . filter sublist . map parse . lines

parse :: [Char] -> [Int]
parse = (\(x : y : _) -> x ++ y) . map (map read . linesBy (== '-')) . linesBy (== ',')

sublist (a : b : c : d : _) = (a <= c && b >= d) || (c <= a && d >= b)

-- Part 2

solve2 = length . filter (not . null) . map (overlap . parse) . lines

overlap (a : b : c : d : _) = [a .. b] `intersect` [c .. d]

-- >>> solve "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
-- 2

-- >>> solve2 "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
-- 4
