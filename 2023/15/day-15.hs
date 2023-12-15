import Control.Applicative (liftA2)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve

solve = sum . map hash . splitOn ","

hash = foldl (\acc x -> (acc + fromEnum x) * 17 `mod` 256) 0

