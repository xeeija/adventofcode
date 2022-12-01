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

-- >>> solve "199\n200\n208\n200"
-- 2

solve3 = length . filter (> 0) . (\xs -> zipWith (-) (tail xs) xs) . sum3 . map read . lines

sum3 xs = zipWith3 (\a b c -> a + b + c) (drop 2 xs) (tail xs) xs

-- >>> sum3 [199,200,208,210,200,207,240]
-- [607,618,618,617,647]

-- >>> choose 3 "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"
-- 5
