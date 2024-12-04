import Control.Monad (liftM3, liftM4)
import Data.List (transpose, zip4)
import Text.Regex.TDFA

main :: IO ()
main = interact $ (++ "\n") . show . liftM3 (,,) (sum . solve) solve (sum . solve2)

-- solve :: String -> [Integer]
solve = map (sum . map (count 0)) . parse''

-- count :: Num t => t -> String -> t
count acc s@('X':'M':'A':'S':_) = count (acc + 1) (tail s)
count acc s@('S':'A':'M':'X':_) = count (acc + 1) (tail s)
count acc s = if length s < 4 then acc else count acc (tail s)

-- parse'' :: String -> [[String]]
parse'' = liftM4 (\a b c d -> [a,b,c,d]) id vertical diagonal (diagonal . vertical) . lines

-- vertical :: [[a]] -> [[a]]
vertical = map reverse . transpose

-- diagonal :: [String] -> [String]
diagonal = transpose . zipWith (\i x -> replicate i ' ' ++ x) [0..]


-- Part Two

-- type Triple = (String, String, String)

solve2 = map (mas 0) . parse2

-- mas :: Int -> Triple -> Int
-- mas :: Int -> (String, String, String) -> Int
mas acc x@(as@('M':_:'M':a), bs@(_:'A':_:b), cs@('S':_:'S':c)) = found acc x
mas acc x@(as@('S':_:'S':a), bs@(_:'A':_:b), cs@('M':_:'M':c)) = found acc x 
mas acc x@(as@('M':_:'S':a), bs@(_:'A':_:b), cs@('M':_:'S':c)) = found acc x 
mas acc x@(as@('S':_:'M':a), bs@(_:'A':_:b), cs@('S':_:'M':c)) = found acc x 
mas acc (a:as, b:bs, c:cs) = if length as < 2 then acc else mas acc (as, bs, cs)

-- found :: Int -> Triple -> Int
-- found :: Int -> (String, String, String) -> Int
found acc (as, bs, cs) = mas (acc + 1) (tail as, tail bs, tail cs)

-- parse2 :: String -> [Triple]
-- parse2 :: String -> [(String, String, String)]
parse2 = (zip3 <*> drop 1 <*> drop 2) . lines

-- equivalent to the above pointfree style
-- parse2 = (\xs -> zip3 xs (drop 1 xs) (drop 2 xs)) . lines

-- Examples

-- >>> parse2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n.........."
-- [(".M.S......","..A..MSMS.",".M.S.MAA.."),("..A..MSMS.",".M.S.MAA..","..A.ASMSM."),(".M.S.MAA..","..A.ASMSM.",".M.S.M...."),("..A.ASMSM.",".M.S.M....",".........."),(".M.S.M....","..........","S.S.S.S.S."),("..........","S.S.S.S.S.",".A.A.A.A.."),("S.S.S.S.S.",".A.A.A.A..","M.M.M.M.M."),(".A.A.A.A..","M.M.M.M.M.","..........")]

-- >>> solve2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n.........."
-- [1,2,2,0,0,0,4,0]


-- Unused Part 1

-- used to convert input to horizontal, vertical and diagonal grid for "easy" search
-- main = interact $ (++ "\n") . (unlines . map unlines . parse'')

-- something is incorrect with counting with this function, because parse'' works now
-- solve = length . liftA2 (++) (xmas "XMAS") (xmas "SAMX") . parse'
-- solve = map (liftA2 (\a b -> [length a, length b]) (xmas "XMAS") (xmas "SAMX")) . parse''

-- xmas :: String -> [String] -> [[String]]
-- xmas r = filter (not . null) . map (matches r)
--   where
--     matches :: String -> String -> [String]
--     matches re x = getAllTextSubmatches (x =~ re)

-- xmas :: [String] -> [[String]]
-- xmas = filter (not . null) . map (\x -> getAllTextMatches (x =~ "(?=XMAS|SAMX)"))

-- xmas :: [String] -> [[(Int, Int)]]
-- xmas = filter (not . null) . map (\x -> getAllMatches (x =~ "XMAS|SAMX"))

-- parse = liftM2 (++) horizontal vertical . lines

-- parseDiag = liftM2 (++) diag (map reverse . diag . map reverse) . lines

-- parseDiag1 = diag . lines
-- parseDiag2 = map reverse . diag . map reverse . lines

-- parse' = liftM4 (\a b c d -> [a,b,c,d]) horizontal vertical diag diag2 . lines
-- -- parse' = liftM4 (\a b c d -> a ++ b ++ c ++ d) horizontal vertical diag (diag . reverse) . lines

-- horizontal = id
-- vertical = map reverse . transpose

-- -- diag :: [String] -> [String]
-- diag = transpose . zipWith (\i x -> replicate i ' ' ++ x) [0..]
-- -- diag = liftA2 (++) transpose id . transpose . zipWith (\i x -> replicate i ' ' ++ x) [0..]

-- diag2 = transpose . zipWith (\i x -> replicate i ' ' ++ reverse x) [0..]

-- diag' :: (Int -> String -> String) -> [String] -> [String]
-- diag' f = transpose . zipWith f [0..]

-- -- shift :: Int -> String -> String
-- shift i x = replicate i ' ' ++ x

-- -- shift' :: Int -> String -> String
-- shift' i x = x ++ replicate i ' '

-- shift2 i l x = replicate i ' ' ++ x ++ replicate l ' '



-- Examples

-- ..X...
-- .SAMX.
-- .A..A.
-- XMAS.S
-- .X....

-- ..X...
--  .SAMX.
--   .A..A.
--    XMAS.S
--     .X....

--     ..X...
--    .SAMX.
--   .A..A.
--  XMAS.S
-- .X....

-- >>> parse "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."
-- ["..X...",".SAMX.",".A..A.","XMAS.S",".X....","...X.",".SAMX","XA.A.",".M.S.",".XA..","...S."]

-- >>> (xmas . parse) "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."
-- [["SAMX"],["XMAS"],["SAMX"]]

-- >>> (diag' shift . lines) "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."

-- >>> (diag . reverse . lines) "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."

-- >>> (parse') "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."

-- >>> (xmas . parse') "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X...."
-- [["SAMX"],["XMAS"],["SAMX"],["SAMX"]]


-- >>> (length . xmas . parse') "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- 19

-- >>> (parse') "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- [["....XXMAS.",".SAMXMS...","...S..A...","..A.A.MS.X","XMASAMX.MM","X.....XA.A","S.S.S.S.SS",".A.A.A.A.A","..M.M.M.MM",".X.X.XMASX"],["....XXS...",".S..M..A.X",".A.AA.S.M.",".MS.S..A.X","XX.AA.S.M.","XM..M..A.X","MSAMXXS.MM","A..S.A.A.A","S...M.S.MS","...XMASAMX"],[".         ","..        ",".S.       ",".A..      ","XM..X     ","XXSAMX    ","MM..A.S   ","AS.AS...  ","S.A.A.SA. ","...MM.....","..SX.SAMX","...X....","XMASAMX","M.....","ASAMX","S..M","AMA","MS","X"],["         .","        .S","       ..A","      X..M","     M..SX","    AMSAMX","   S..M.X.","  ASAX..M."," M..XMASA.","XMAS.A..S.","S....SA..","AMAS.A..","M....M.","XMAS.X","....X","XMAS","...","X.","."]]

-- >>> (length . xmas . parse) "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- 4

-- >>> (length . xmas . parseDiag) "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- 5

-- >>> (solve) "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- [[3,2],[1,2],[1,4],[1,4]]

-- >>> (parseDiag1) "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- [".         ","..        ",".S.       ",".A..      ","XM..X     ","XXSAMX    ","MM..A.S   ","AS.AS...  ","S.A.A.SA. ","...MM.....","..SX.SAMX","...X....","XMASAMX","M.....","ASAMX","S..M","AMA","MS","X"]

-- >>> (map (xmas "SAMX") . parse') "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- [[["SAMX"],["SAMX"]],[["SAMX"],["SAMX"]],[["SAMX"],["SAMX"],["SAMX"],["SAMX"]],[["SAMX"],["SAMX"],["SAMX"],["SAMX"]]]

-- >>> (parse'') "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX"
-- [["....XXMAS.",".SAMXMS...","...S..A...","..A.A.MS.X","XMASAMX.MM","X.....XA.A","S.S.S.S.SS",".A.A.A.A.A","..M.M.M.MM",".X.X.XMASX"],["...SXX....","X.A..M..S.",".M.S.AA.A.","X.A..S.SM.",".M.S.AA.XX","X.A..M..MX","MM.SXXMASM","A.A.A.S..A","SM.S.M...S","XMASAMX..."],[".         ","..        ",".S.       ",".A..      ","XM..X     ","XXSAMX    ","MM..A.S   ","AS.AS...  ","S.A.A.SA. ","...MM.....","..SX.SAMX","...X....","XMASAMX","M.....","ASAMX","S..M","AMA","MS","X"],[".         ",".X        ","...       ","SAMX      ","X....     ","X.SAMX    ",".M....M   ","..A.SAMA  ","..AS....S ",".S..A.SAMX",".ASAMX..M",".M..XASA",".X.M..S","XMASMA","XS..M","M..X","A..","S.","."]]
