import GHC.Base (liftA2)
import Data.List (elemIndices, nub)

type Tuple a = (a, a)
type Point = Tuple Int

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2


solve :: String -> Int
solve = length . signals antinodes' . lines

-- signals :: (Eq a, Num n, Enum n) => ((Int, Int) -> [(n, Int)] -> [a]) -> [[Char]] -> [a]
signals :: (Eq a) => (Point -> [Point] -> [a]) -> [String] -> [a]
signals fn xs = nub $ concatMap (fn (bounds xs) . (`antennas` xs)) ['0'..'z']
  where
    bounds x = (length x, length (head x))

-- antennas :: (Num a1, Enum a1, Eq a2) => a2 -> [[a2]] -> [(a1, Int)]
antennas :: (Eq a) => a -> [[a]] -> [Point]
antennas x = concatMap (uncurry (zip . repeat)) . filter (not . null . snd) . zip [0..] . map (elemIndices x)
-- antennas x = concatMap (\(x,xs) -> zip (repeat x) xs) . filter (not . null . snd) . zip [0..] . map (elemIndices x)

antinodes' :: Point -> [Point] -> [Point]
antinodes' max xs = filter (valid max) [antinode' a b | a <- xs, b <- xs, a /= b]

antinode' :: Point -> Point -> Point
antinode' a b = a +: (b -: a)

-- valid :: (Ord a, Num a) => (a, b) -> (a, a) -> Bool
valid :: Point -> Point -> Bool
valid (rMax, cMax) (r, c) = and [r >= 0, r < rMax, c >= 0, c < rMax]


(-:) :: Point -> Point -> Point
(-:) (r1, c1) (r2, c2) = (r2 - r1, c2 - c1)

-- (+:) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+:) :: Point -> Point -> Point
(+:) (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)


-- Part Two

solve2 :: String -> Int
solve2 = length . signals resonantAntinodes' . lines

resonantAntinodes' :: Point -> [Point] -> [Point]
resonantAntinodes' max xs = concat [nodes a b | a <- xs, b <- xs, a /= b]
  where nodes a b = takeWhile (valid max) (resonantNodes' a b)

resonantNodes' :: Point -> Point -> [Point]
resonantNodes' a b = iterate (+: (b -: a)) a

-- "resonantNodes" is a superset for "antinode" from part 1
-- antinode == (head . tail . resonantNodes)


-- Unused

-- equivalent to "signal" above, but with list comprehension
-- signals' :: (Eq a) => (Point -> [Point] -> [a]) -> [String] -> [a]
-- signals' fn xs = nub $ concat [fn bounds (antennas fr xs) | x <- xs, fr <- ['0'..'z']]
--   where
--     bounds = (length xs, length (head xs))

-- antinodes :: (Ord a, Num a) => (a, b) -> [(a, a)] -> [(a, a)]
-- antinodes :: Point -> [Point] -> [Point]
-- antinodes max xs = concatMap (\a -> filter (valid max) (map (uncurry antinode' . (a,)) (filter (/= a) xs))) xs

-- antinode :: (Num a, Num b) => ((a, b), (a, b)) -> (a, b)
-- antinode :: (Point, Point) -> Point
-- antinode (a, b) = a +: (b -: a)
-- antinode (a, b) = a +: distance b a
-- antinode'' (a,b) = (a, distance b a, a +: distance b a)

-- resonantAntinodes :: (Ord a, Num a) => (a, b) -> [(a, a)] -> [(a, a)]
-- resonantAntinodes :: Point -> [Point] -> [Point]
-- resonantAntinodes max xs = concatMap (\a -> concatMap (takeWhile (valid max) . resonantNodes . (a,)) (filter (/= a) xs)) xs

-- -- resonantNodes :: (Num a, Num b) => ((a, b), (a, b)) -> [(a, b)]
-- resonantNodes :: (Point, Point) -> [Point]
-- resonantNodes (a, b) = iterate (+: (b -: a)) a



-- distance :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
-- distance :: Point -> Point -> Point
-- distance (r1, c1) (r2, c2) = (r2 - r1, c2 - c1)

-- alternative signature
-- distance' :: ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
-- distance' = uncurry (-) *** uncurry (-)

-- antinode a b = nub [
--   -- a +: distance a b, 
--   a +: distance b a,
--   b +: distance a b
--   -- b +: distance b a
--   ]

-- antinode' a b = [
--   -- show a ++ "+: dist(" ++ show a ++ "," ++ show b ++ ")", 
--   show a ++ "+: dist(" ++ show b ++ "," ++ show a ++ ")",
--   show b ++ "+: dist(" ++ show a ++ "," ++ show b ++ ")"
--   -- show b ++ "+: dist(" ++ show b ++ "," ++ show a ++ ")"
--   ]

-- findAntinodes :: Char -> [String] -> 
-- findAntinodes x tiles = antennas x

-- allAntinodes :: ([String] -> (Int, Int) -> [String]) -> [String] -> [(Int, Int)] -> [[String]]
-- allAntinodes xs = scanl (\acc a -> map antinode (repeat a) (filter (/= a) xs)) [] xs
-- allAntinodes xs = scanl (\acc a -> acc ++ map (antinode a) (filter (/= a) xs)) [] xs


-- allAntinodes'' maxR maxC xs = foldr (\(i,a) acc -> acc ++ map (antinode'' . (a,)) (filter (/= a) xs)) [] (zip [0..] (reverse xs))
-- allAntinodes'' (rMax, cMax) xs = foldr (\a acc -> acc ++ filter valid (map (antinode'' . (a,)) (filter (/= a) xs))) [] xs

-- allAntinodes' :: (Ord p1, Ord p2, Num p1, Num p2) => p1 -> p2 -> [(p1, p2)] -> [(Int, [(p1, p2)])]
-- allAntinodes' maxR maxC xs = foldr (\(i,a) acc -> acc ++ [take i $ map ((i,) . antinode' a) (filter (/= a) xs)]) [] (zip [0..] (reverse xs))

-- allAntinodes :: (Ord p1, Ord p2, Num p1, Num p2) => p1 -> p2 -> [(p1, p2)] -> [[(p1, p2)]]
-- allAntinodes maxR maxC xs = foldr (\(i,a) acc -> acc ++ map (filter valid . take i . antinode a) (filter (/= a) xs)) [] (zip [0,2..] xs)
-- allAntinodes maxR maxC xs = foldr (\(i,a) acc -> acc ++ map (filter valid . take i . antinode a) (filter (/= a) xs)) [] (zip [0..] (reverse xs))
--   where
--     valid (r, c) = and [r >= 0, r < maxR, c >= 0, c < maxC]

