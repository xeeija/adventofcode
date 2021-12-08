module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)

  putStrLn "Window size (2 or 3): "
  n <- readLn

  print $ choose n f

-- main = print . solve =<< readFile "input.txt"

choose 3 = solve3
choose _ = solve

solve :: String -> Int
solve = length . filter (> 0) . (\xs -> zipWith (-) (tail xs) xs) . map read . lines


solve3 = length . filter (> 0) . (\xs -> zipWith (-) (tail xs) xs) . sum3 . map read . lines

sum3 xs = zipWith3 (\a b c -> a + b + c) (drop 2 xs) (tail xs) xs

