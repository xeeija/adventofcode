import GHC.Base (liftA2)
import Data.Bits
import Data.List (zipWith4)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve :: String -> Int
solve = sum . map (day secret . read) . lines

-- x * 64 = x << 6, x / 32 = >> 5, x * 2048 = << 11

-- secret :: (Bits a, Num a) => a -> a
secret :: Int -> Int
secret = mix 11 . mix (-5) . mix 6

mix :: (Bits a, Num a) => Int -> a -> a
mix b n = (n .^. shift n b) .&. 0xffffff

-- mix b n = prune $ shift n b .^. n

-- prune :: (Bits a, Num a) => a -> a
-- prune = (.&.) 0xFFFFFF -- (1 << 24)

day :: (c -> c) -> c -> c
day f = (!! 2000) . iterate f


-- Part Two


-- solve2 :: String -> (S.Set [Int], [M.Map [Int] Int])
solve2 :: String -> (Int, [Int])
solve2 = liftA2 bestChange id (S.unions . map M.keysSet) . map (changesMap 2000 . read) . lines


bestChange :: (Foldable t, Num v, Ord v, Ord k) => [M.Map [k] v] -> t [k] -> (v, [k])
bestChange maps = foldl findBest (0, [])
  where
    findBest acc@(accVal, _) k = if total k > accVal then (total k, k) else acc
    total k = sum $ map (M.findWithDefault 0 k) maps

-- fold: look at the prices of all buyers for every tuple of changes
-- and set it as new max if it has more total bananas then previous


price :: Int -> [Int]
price = map (`mod` 10) . iterate secret

diff :: Num a => [a] -> [a]
diff = zipWith subtract <*> drop 1

changes :: [a] -> [[a]]
changes = zipWith4 (\a b c d -> [a,b,c,d]) <*> drop 1 <*> drop 2 <*> drop 3
-- changes = zip4 <*> drop 1 <*> drop 2 <*> drop 3

-- changes' xs = zipWith4 (\a b c d -> [a,b,c,d]) xs (drop 1 xs) (drop 2 xs) (drop 3 xs)

priceChanges :: Int -> [(Int, [Int])]
priceChanges = liftA2 zip id (changes . diff) . price
-- priceChanges = liftA2 zip price (changes . diff . price)

priceChanges' :: Int -> [([Int], Int)]
priceChanges' = liftA2 zip (changes . diff) (drop 4) . price
-- equivalent to:


changesMap :: Int -> Int -> M.Map [Int] Int
changesMap n = M.fromListWith (\a b -> b) . (take n . priceChanges')

-- (\a b -> b) == flip const == const id

