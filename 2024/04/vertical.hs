import Data.List (transpose)

main :: IO ()
main = interact $ (++ "\n") . unlines . map reverse . transpose . lines

