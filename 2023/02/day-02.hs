import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, maximumBy)
import Data.Function (on)

type Game = [[Pick]]
type Pick = (Integer, String)

main = interact $ (++ "\n") . show . (\x -> (solve x, solve2 x))

solve :: String -> Integer
solve = sum . map fst . filter (all (all valid) . snd) . map parse . lines

parse :: String -> (Integer, Game)
parse = (\(id : game : _) -> (read (drop 5 id), parseGame game)) . splitOn ": "

valid :: Pick -> Bool
-- validPick :: (Ord a, Num a) => (a, String) -> Bool
valid (x, "red") = x <= 12
valid (x, "green") = x <= 13
valid (x, "blue") = x <= 14
valid (x, _) = False

parseGame :: String -> Game
parseGame = map (map ((\(x:color) -> (read x, head color)) . splitOn " ") . splitOn ", ") . splitOn "; "

-- Part Two

solve2 :: String -> Integer
solve2 = sum . map (product . minCubes . parse) . lines

minCubes :: (Integer, Game) -> [Integer]
minCubes = map (fst . maximumBy (compare `on` fst)) . groupBy ((==) `on` snd) . sortOn snd . concat . snd

