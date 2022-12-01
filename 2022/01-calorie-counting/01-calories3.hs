import Data.List (groupBy, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Text.Read (readMaybe)

main = interact calories3

calories3 :: String -> String
calories3 = show . sum . foldl top3 [0] . map (sum . mapMaybe readMaybe) . groupBy (\x y -> y /= "") . lines
  where
    top3 acc x
      | x > minimum acc = take 3 $ sortOn Down (x : acc)
      | otherwise = acc

-- >>> calories3 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
-- "45000"
