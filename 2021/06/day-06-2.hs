module Main where

import Data.List
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ head $ filter (\(x : xs) -> x /= '-') args

  -- print $ choose args (map read $ words f)

  mapM_ printWithDate $ zip [5, 10 ..] $ map (choose args (map read $ words f)) [5, 10 .. (read $ args !! 2)]

printWithDate :: Show a => a -> IO ()
printWithDate x = do
  print x
  t <- getCurrentTime
  print t

-- import Control.Monad ((<=<))
-- (print <=< withDate) ==

withDate :: (a, b) -> IO (a, b, String)
withDate (x, y) = do
  t <- getCurrentTime
  return (x, y, show t)

choose :: [String] -> [Int] -> Int -> Int
choose args
  | "-2" `elem` args = flip solve -- (read $ args !! 2) -- 256 is too much, because exponential growth
  | otherwise = flip solve -- 80

solve :: Int -> [Int] -> Int
solve n = length . (!! n) . simulate

simulate :: [Int] -> [[Int]]
simulate = iterate (tick =<<)

tick :: Int -> [Int]
tick 0 = [6, 8]
tick n = [n - 1]

