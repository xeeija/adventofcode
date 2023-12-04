import Data.List.Split (splitOn)
import Data.List (groupBy, sortOn, maximumBy)
import Data.Function (on)
import Control.Applicative (liftA2)
import Control.Monad (liftM2)

type Game = [[Pick]]
type Pick = (Integer, String)

-- main = interact $ (++ "\n") . show . (\x -> (solve x, solve2 x))

main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

-- main = interact $ (++ "\n") . show . liftM2 ($) [solve, solve2] . replicate 1

-- main = interact $ (++ "\n") . show . (,) <$> solve <*> solve2

test :: String -> (Integer, Integer)
test = liftA2 (,) solve solve2

test2 :: String -> [Integer]
test2 = liftM2 ($) [solve, solve2] . replicate 1

test3 = (,) <$> solve <*> solve2
test3 :: String -> (Integer, Integer)

-- >>> test "Game 1: 13 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue\nGame 2: 11 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- >>> test2 "Game 1: 13 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue\nGame 2: 11 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- >>> test3 "Game 1: 13 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue\nGame 2: 11 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- (3,360)
-- [3,360]
-- (3,360)

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

-- >>> solve "Game 1: 13 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue\nGame 2: 11 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- 3

-- >>> all (all valid) [[(13,"red"),(1,"green")],[(4,"green"),(3,"red"),(5,"blue")]]
-- False

-- >>> parseGame "6 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- [[(6,"green"),(3,"blue")],[(3,"red"),(1,"green")],[(4,"green"),(3,"red"),(5,"blue")]]


-- minCubes2 :: String -> Integer
-- minCubes2 = product . map (fst . maximumBy (compare `on` fst)) . concatMap (groupBy ((==) `on` snd) . sortOn snd . concat . snd . parse) . lines

-- >>> minCubes "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -- \nGame 2: 11 green, 3 blue; 3 red, 1 green; 4 green, 3 red, 5 blue"
-- 48

-- >>> solve2 $ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
-- 60
