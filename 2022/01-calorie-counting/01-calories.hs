import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main = interact calories

calories :: String -> String
calories = show . maximum . map (sum . mapMaybe readMaybe . lines) . splitOn "\n\n"

-- >>> calories "1000\n2000\n3000\n\n4000"
-- "6000"
