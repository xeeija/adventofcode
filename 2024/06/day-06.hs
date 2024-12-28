import GHC.Base (liftA2, liftA3)
import Data.List (transpose)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve (const 0)

solve :: String -> Int
solve = positions . head . dropWhile (notElem '^' . head) . iterate (rotate' . map walk) . parse
-- solve = iterate (rotate' . map walk) . parse

positions :: [String] -> Int
positions = succ . sum . map (length . filter (== 'x'))

parse = map reverse . transpose . lines

-- rotate 90 deg counter clockwise, so -90deg
rotate' :: [[a]] -> [[a]]
rotate' = transpose . map reverse


-- replace "...^.....#" with "...xxxxx^#", replace visited tiles with x
walk :: String -> String
walk xs = start ++ replace middle ++ end
  where
    (start, rest) = span (/= '^') xs
    (middle, end) = span (/= '#') rest
    replace [] = []
    replace xs = replicate (length xs - 1) 'x' ++ "^"

