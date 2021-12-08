module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head (filter (\(x : xs) -> x /= '-') args))

  print $ choose args (lines f)

choose :: [String] -> [String] -> Int
choose args
  | "-2" `elem` args = dive'
  | otherwise = dive

dive :: [String] -> Int
dive = uncurry (*) . foldr (move . words) (0, 0)
  where
    move :: [String] -> (Int, Int) -> (Int, Int)
    move ("forward" : n : _) (x, y) = (x + read n, y)
    move ("down" : n : _) (x, y) = (x, y + read n)
    move ("up" : n : _) (x, y) = (x, y - read n)


dive' :: [String] -> Int
dive' = (\(x, y, a) -> x * y) . foldl move (0, 0, 0) . map words
  where
    move :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
    move (x, y, a) ("forward" : n : _) = (x + read n, y + (a * read n), a)
    move (x, y, a) ("down" : n : _) = (x, y, a + read n)
    move (x, y, a) ("up" : n : _) = (x, y, a - read n)

