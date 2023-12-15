import Control.Applicative (liftA2)
import Data.List
import Data.List.Split
import Data.Ord (Down(Down))

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve3 . lines

solve = sum . weight . tiltNorth

weight = zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

tiltNorth = transpose . map (intercalate "#" . map (sortOn Down) . linesBy (== '#')) . transpose

-- Part Two

solve2 = sum . weight . (!! 4) . iterate spin

solve3 = zip [500..] . map (sum . weight) . take 50 . drop 500 . iterate spin

tilt =  map (foldr1 (++) . map sort . split (oneOf "#"))

turn = transpose . reverse

spin = (!! 4) . iterate (tilt . turn)

spin' x y = take y . drop x . iterate (tilt . turn)

spin'' x = (!! x) . iterate spin

-- TODO: determine pattern and stop, if we are at the pattern
-- or rather: skip (desired cycle div. pattern length) and jump straight to last
-- 1. drop the first bit (like 500) -> maybe not needed with find pattern
-- 2. determine pattern length (14 in my case)
-- -> pattern https://stackoverflow.com/questions/70140102/find-longest-repeating-sequence-in-list
-- 3. get the 1.000.000.000 mod pattern length's element (in my case (1 mrd - 500) mod 14 = 10)

