import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main = interact calories

calories :: String -> String
calories = show . maximum . map (sum . mapMaybe readMaybe . lines) . splitOn "\n\n"

