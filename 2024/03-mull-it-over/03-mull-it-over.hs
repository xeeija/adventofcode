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


-- Examples

-- >>> muls "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- ["mul(2,4)","mul(5,5)","mul(11,8)","mul(8,5)"]

-- >>> (clean . muls) "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- [[2,4],[5,5],[11,8],[8,5]]

-- >>> (solve) "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- 161


-- >>> (enabled) "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- "xmul(2,4)&mul[3,7]!^?mul(8,5))"

-- >>> (solve2) "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- 48
