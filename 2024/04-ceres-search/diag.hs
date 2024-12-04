import Data.List (transpose)

main = interact $ (++ "\n") . unlines . diag . lines

diag = transpose . zipWith (\i x -> replicate i ' ' ++ x) [0..]
