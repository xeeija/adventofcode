import GHC.Base (liftA2)
import Data.List.Split (splitOn)
import Data.Function (on)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve2

solve :: String -> Integer
solve = sum . map validSolutions . parse
  where
    validSolutions (x, eq) | x `elem` solutions eq (ops (length eq - 1)) = x | otherwise = 0


parse :: String -> [(Integer, [Integer])]
parse = map ((\(x:xs:_) -> (read x, map read (words xs))) . splitOn ":") . lines

-- possibly non exhaustive pattern here with wrong input
calc' :: Num a => [a] -> String -> a
calc' [a, b] [o] = operator' o a b
calc' (a:xs) (o:p) = operator' o a (calc' xs p)

operator' :: Num a => Char -> a -> a -> a
operator' o | o == '+' = (+) | o == '*' = (*)
-- operator' o | o == '+' = (\a b -> show a ++ "+" ++ show b) | o == '*' = (\a b -> show a ++ "*" ++ show b)

solutions :: Num b => [b] -> [String] -> [b]
solutions eq = map (calc' (reverse eq))
-- solutions eq opList = map ((flip calc') eq) opList

-- valid :: (Int, [Int]) -> Bool
-- valid (x, xs) = sum xs <= x && product xs >= x


binary :: (Show t, Integral t) => t -> String
binary n | n == 0 = "" | otherwise = show (n `mod` 2) ++ binary (n `div` 2)

op :: Char -> String
op x | x == '0' = "+" | x == '1' = "*"
-- op '0' = "+"
-- op '1' = "*"

opsBinary :: Integral p => p -> [[String]]
opsBinary n = take (n2 `div` 2) $ map (map op . binary) [n2-1,n2-2..0] where n2 = 2 ^ n
-- opsBinary n = filter ((== n) . length) $ map (map op . binary) [1..(2 ^ n)-1]

ops :: Integral p => p -> [String]
ops x = concatMap (map concat) [opsBinary x, map (map swapOp) (opsBinary x)]

swapOp :: String -> String
swapOp x | x == "+" = "*" | x == "*" = "+"


-- Part Two

solve2 :: String -> Integer
solve2 = sum . map validSolutions2 . parse
  where
    validSolutions2 (x, eq) 
      -- | otherwise = (x, eq, solutions2 eq (ops3 (length eq - 1))) 
      | x `elem` solutions2 eq (ops3 (length eq - 1)) = x 
      | x `elem` solutions2 (reverse eq) (ops3 (length eq - 1)) = x 
      -- | x `elem` solutions2 eq (ops3 (length eq - 1)) || x `elem` solutions2 (reverse eq) (ops3 (length eq - 1)) = x 
      | otherwise = 0

-- valid3 (x, xs) = sum xs <= x && product xs >= x && x <= read (concatMap show xs)


solutions2 :: (Num b, Read b, Show b) => [b] -> [String] -> [b]
solutions2 eq = map (calc'' (reverse eq))


calc'' :: (Num a, Read a, Show a) => [a] -> String -> a
calc'' [a, b] [o] = operator'' o a b
calc'' (a:xs) (o:p) = operator'' o a (calc'' xs p)

operator'' :: (Num a, Read a, Show a) => Char -> a -> a -> a
operator'' o | o == '+' = (+) | o == '*' = (*) 
  | o == '|' = \a b -> read (show b ++ show a)
  -- equivalent to this in pointfree style
  -- | o == '|' = read .: (flip (++) `on` show)

-- function composition with 2 arguments
-- (.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
-- (.:) = (.) . (.)

opsTri :: Integral p => p -> [[String]]
opsTri n = take (n3 `div` 3) $ map (map op3 . ternary) [n3-1,n3-2..0] where n3 = 3 ^ n
-- opsTri n = map (map op3 . tri) [n3-1,n3-2..0] where n3 = 3 ^ n

ternary :: (Show t, Integral t) => t -> String
ternary n | n == 0 = "" | otherwise = show (n `mod` 3) ++ ternary (n `div` 3)

op3 :: Char -> String
op3 '0' = "+"
op3 '1' = "*"
op3 '2' = "|"
-- op3 x | x == '0' = "+" | x == '0' = "+" | x == '1' = "*" | x == '2' = "|"

swapOp3 :: String -> String
swapOp3 x | x == "+" = "*" | x == "*" = "|" | x == "|" = "+"

ops3 :: Integral p => p -> [String]
ops3 x = concatMap (map concat) [opsTri x, swap1 x, swap2 x]
  where
    swap1 = swapFn swapOp3
    swap2 = swapFn (swapOp3 . swapOp3)
    swapFn fn x = map (map fn) (opsTri x)


-- Unused

-- -- equation :: [a] -> [(a, a)]
-- equations = zip <*> drop 1
-- -- equation = (\eq -> calc eq (ops (length eq))) . zip <*> drop 1

-- -- calculate :: [(Int, Int)] -> [((Int, Int), [[String]])]
-- -- calculate = zip <*> equation -- (ops (length eq))
-- -- calculate eq = (\xs -> (xs, ops (length xs))) $ equation eq -- (ops (length eq))
-- calculate = concatMap (\(a,b) -> [(a, "+", b), (a, "*", b)]) . equations
-- -- calculate = concatMap (\(a,b) -> [a "+" b, a * b]) . equations
-- -- calculate = map (uncurry (+)) . equations
