import GHC.Base (liftA2)
import Data.List.Split (splitOn)
import Data.List (partition, transpose, group)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve (const 0)


solve :: String -> Int
solve = length . filter id . allFit . parse

parse :: String -> ([[Int]], [[Int]])
parse = mapTuple (map (heights . lines)) . partition ((== '#') . head) . splitOn "\n\n"

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

heights :: [String] -> [Int]
heights = map (pred . length . head . group) . transpose

allFit :: ([[Int]], [[Int]]) -> [Bool]
allFit (ls, ks) = [all (>= 0) $ zipWith (-) k l | l <- ls, k <- ks]
