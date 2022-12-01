import Data.List (groupBy, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Text.Read (readMaybe)

main = interact calories3

calories3 :: String -> String
calories3 = show . sum . foldl top3 [0] . map (sum . mapMaybe readMaybe . lines) . splitOn "\n\n"
  where
    top3 acc x
      | x > minimum acc = take 3 $ sortOn Down (x : acc)
      | otherwise = acc

