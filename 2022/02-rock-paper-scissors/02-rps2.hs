main :: IO ()
main = interact $ show . rps'

rps' :: String -> Int
rps' = sum . map play' . lines

play' :: [Char] -> Int
play' (a : _ : 'X' : _) = if x > 3 then x - 3 else x where x = win a + 1
play' (a : _ : 'Y' : _) = 3 + score a
play' (a : _ : 'Z' : _) = 6 + win a

score :: Char -> Int
score = (`mod` 23) . flip (-) 64 . fromEnum

win :: Char -> Int
win 'A' = 2
win 'B' = 3
win a = 1

-- >>> rps' "A Y\nB X\nC Z"
-- 12

-- >>> score 'C' + 1
-- >>> win 'C'
-- 4
-- 1
