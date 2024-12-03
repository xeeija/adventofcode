import Control.Applicative (liftA2)
import Text.Regex.TDFA
import Data.List.Split (splitWhen, splitOneOf, splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve :: String -> Int
solve = sum . map product . parse . muls

parse :: [String] -> [[Int]]
parse = map (mapMaybe readMaybe . splitOneOf "(,)")

-- muls :: RegexContext Regex r (AllTextMatches [] String) => r -> [String]
muls x = getAllTextMatches (x =~ "mul\\([0-9]+,[0-9]+\\)") :: [String]

-- Part Two

solve2 :: String -> Int
solve2 = solve . enabled

enabled :: String -> String
enabled = concatMap (head . splitOn "don't()") . splitOn "do()"

