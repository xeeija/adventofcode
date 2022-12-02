main :: IO ()
main = interact $ show . (\x -> (rps x, rps' x))

rps :: String -> Int
rps = sum . map play . lines

play :: String -> Int
play (a : _ : b : _)
  | ((score a + 1) `mod` 3) == score b `mod` 3 = 6 + score b
  | score a == score b = 3 + score b
  | otherwise = score b

score :: Char -> Int
score = (`mod` 23) . flip (-) 64 . fromEnum

rps' :: String -> Int
rps' = sum . map play' . lines

play' :: [Char] -> Int
play' (a : _ : 'X' : _) = if l > 3 then l - 3 else l where l = score a + 2
play' (a : _ : 'Y' : _) = 3 + score a
play' (a : _ : 'Z' : _) = 6 + if w > 3 then w - 3 else w where w = score a + 1

-- AX = Rock, BY = Paper, CZ = Scissors

-- >>> rps "A Y\nB X\nC Z"
-- 15

-- >>> rps' "A Y\nB X\nC Z"
-- 12
