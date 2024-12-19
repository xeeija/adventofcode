import GHC.Base (liftA2)
import Data.List.Split (splitOn)
import Data.List (intercalate, sortOn)
import Text.Regex.TDFA ((=~))
import Data.Ord (Down(Down))

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve2 (const 0)

-- Attempt 2

-- length . filter id === sum . map fromEnum 
solve2 :: String -> Int
solve2 = sum . map fromEnum . liftA2 (\a -> map (\x -> x == a x)) (possible2 . patternRegex2 . parsePatterns2) parseDesigns

parseDesigns :: String -> [String]
parseDesigns = lines . last . splitOn "\n\n"

-- make a tuple of functions for sorting, because tuple sorts by its items from left to right
-- or with monoid <> operator/function, which the tuple uses
-- so, first by length (Down = desc), then by item itself (with id function)
-- sortPatterns = sortOn (liftA2 (,) (Down . length) id)
sortPatterns :: [String] -> [String]
sortPatterns = sortOn (\x -> (Down $ length x, x))

-- sorted pattern with length desc and sorted pattern seems to have helped compared to v1 (~1 mmin runtime)
parsePatterns2 :: String -> String
parsePatterns2 = intercalate "|" . sortPatterns . splitOn ", " . head . splitOn "\n\n"

patternRegex2 :: String -> String
patternRegex2 patterns = "(" ++ patterns ++ ")*"

possible2 :: String -> String -> String
possible2 pattern design = design =~ pattern



-- Unused

-- Part 1 Attempt 1 

-- too slow
-- solve1 :: [Char] -> Int
-- -- solve = length . filter id . liftA2 (map . possible) (patternRegex . parsePatterns) parseDesigns
-- solve1 = length . filter id . liftA2 map (possible . patternRegex1 . parsePatterns) parseDesigns


-- parsePatterns = intercalate "|" . splitOn ", " . head . splitOn "\n\n"

-- patternRegex1 patterns = "^(" ++ patterns ++ ")+$"

-- parseDesigns = lines . last . splitOn "\n\n"

-- possible :: String -> String -> Bool
-- possible pattern design = design =~ pattern
