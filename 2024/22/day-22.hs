import GHC.Base (liftA2)
import Data.Bits

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve (const 0)

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

