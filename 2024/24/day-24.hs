import GHC.Base (liftA2)
import Data.List.Split (splitOn, splitOneOf)
import Data.List (sortOn, isPrefixOf)
import Data.Maybe
import Data.Bits
import Data.Map qualified as M


main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve (const 0)

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

solve :: String -> Int
solve = toDecimal . digits . parse
  where
    digits [xs, ys] = map (fst . wires (initial xs) (wiresMap ys)) (zs ys)
    zs = filter (isPrefixOf "z") . M.keys . wiresMap

-- solve = (\(x:y:_) -> (initial x, map (wires (initial x) (wiresMap y)) (zs y))) . map lines . splitOn "\n\n"
-- solve = (\(x:y:_) -> (initial x, wiresMap y)) . map lines . splitOn "\n\n"

toDecimal :: [Int] -> Int
toDecimal = sum . zipWith (*) [2 ^ x | x <- [0..]]

-- parseWires = sortOn (Down . last) . map (filter (/= "") . splitOneOf " ->")

wiresMap :: [String] -> M.Map String [String]
wiresMap = M.fromList . map ((\(a:op:b:c:_) -> (c, [a,op,b])) . filter (/= "") . splitOneOf " ->")


initial :: [String] -> M.Map String Int
initial = M.fromList . map ((\(x:b:_) -> (x, read b)) . splitOn ": ")

wires :: M.Map String Int -> M.Map String [String] -> String -> (Int, M.Map String Int)
-- wires m (x@(a:op:b:c:_):xs) y
-- wires m xs y = (0, m)
wires mr mw y
  | isJust val = (fromJust val, M.insert y (fromJust val) mr)
  | otherwise = (res, M.union ma mb)
  where
    val = M.lookup y mr
    gate' = gate op
    res = resA `gate'` resB
    (resA, ma) = wires mr mw a
    (resB, mb) = wires mr mw b
    resW = M.lookup y mw
    [a, op, b]
      | isJust resW = fromJust resW
      | otherwise = undefined


gate :: String -> Int -> Int -> Int
gate "OR" = (.|.)
gate "XOR" = (.^.)
gate "AND" = (.&.)

