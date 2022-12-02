main :: IO ()
main = interact $ show . rps

rps :: String -> Int
rps = sum . map play . lines

play :: String -> Int
play (a : _ : b : _)
  | ((score a + 1) `mod` 3) == score b `mod` 3 = 6 + score b
  | score a == score b = 3 + score b
  | otherwise = score b

score :: Char -> Int
score = (`mod` 23) . flip (-) 64 . fromEnum

-- >>> rps "A Y\nB X\nC Z"
-- 15

-- AX = Rock, BY = Paper, CZ = Scissors
