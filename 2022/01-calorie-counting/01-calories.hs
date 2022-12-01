import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main = interact calories

calories :: String -> String
calories = show . maximum . map (sum . mapMaybe readMaybe) . groupBy (\x y -> y /= "") . lines

-- >>> calories "1000\n2000\n3000\n\n4000"
-- "6000"
