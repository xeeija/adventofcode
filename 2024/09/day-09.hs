import GHC.Base (liftA2, ord)
import Data.List.Split (chunksOf)
import Data.Bifunctor (first)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve (const 0)

type Chunk = (Integer, [Char])
type Compressed = (Integer, Integer)
-- type Compressed = (Integer, Integer)

solve = checksum . map (\(a,b,_) -> (a,b)) . compress' 0 . disk

solve' = checksum' . map (\(a,b,_) -> (a,b)) . compress' 0 . disk


type Compressed2 = (Integer, Integer, [Chunk])

compress' :: Int -> ([Chunk], [Chunk]) -> [Compressed2]
compress' depth ([], ls) = filled
  where
    (filled, remaining) = append' (-1) (reverse ls)

compress' depth (a@(id, [f]):fs, ls) =
  (read [f], id, []) : fst (append' (fromIntegral (length ls)) ls)

compress' depth ((id, [f, e]):fs, ls) =
  (read [f], id, []) :
  filled ++
  compress' (length filled) (init' fs, init' remaining)
  where
    (filled, remaining) = append' (read [e]) (take (length ls + 1) ls)

compress' depth (a, b) = []


append' :: Integer -> [Chunk] -> ([Compressed2], [Chunk])
append' e [] = ([], [])
append' e [(id, c:c2)] = ([(read [c], id, []) | e < 0], [])

append' e ((id, c:ls):cs)
  | e > read [c] || e < 0 = first ((read [c], id, []) :) rest
  | e < lr = ([(e, id, [])], (id, show (lr - e) ++ ls) : cs)
  | e == lr = ([(e, id, [])], cs)
    where
      lr = read [c]
      rest = append' (e - lr) cs


init' [] = []
init' xs = init xs



-- disk :: String -> [[Chunk]]
disk :: String -> ([Chunk], [Chunk])
disk = (\(a,b) -> (a,b)) . liftA2 (,) (zip [0..] . chunksOf 2) (reverse . zip [0..] . reverse . chunksOf 2 . reverse)

-- disk = (\(a,b) -> (a,b)) . liftA2 (,) (zip [0..] . chunksOf 2) (zip [0..] . chunksOf 2 . reverse)
-- disk = (\(a,b) -> (a,b)) . liftA2 (,) (zip [0..] . chunksOf 2) ((reverse . zip [0..]) . chunksOf 2 . reverse)

checksum :: Foldable f => f Compressed -> (Integer, Integer)
checksum = foldl (\(i, acc) (n, x) -> (i + n, acc + sum (take (fromInteger n) (map (* x) [i..])))) (0, 0)

checksum' :: Foldable f => f Compressed -> (Integer, [(Integer, Integer)])
checksum' = foldl (\(i, acc) (n, x) -> (i + n, acc ++ take (fromInteger n) (map (, x) [i..]))) (0, [])


-- Unused

-- compress :: ([Chunk], [Chunk]) -> [Compressed]
-- -- compress ((id, [f, e]):_, (_, [l, le]):_) = replicate (read [f]) [f] ++ replicate (read [e]) [l]
-- -- compress ((id, [f, e]):fs, (idl, l:_):ls) = [(read [f], f, id), (read [e], l, idl)] ++ compress (fs, ls)
-- -- compress ((id, [f, e]):fs, ls) = (read [f], f, id) : filled ++ compress (fs, remaining)
-- -- init fs but add 
-- -- compress ((id, [f, e]):fs, ls) = (read [f], id) : filled ++ compress (init' fs, init' remaining)
-- compress ([], ls) = (96, 96) : fst (append (fromIntegral (length ls)) ls)
-- compress ((id, [f]):fs, ls) = (97, 97) : (read [f], id) : fst (append (fromIntegral (length ls)) ls)
-- compress ((id, [f, e]):fs, ls) = (95, 95) : (read [f], id) : filled ++ compress (init fs, init remaining)
--   where
--     (filled, remaining) = append (read [e]) ls

-- -- compress ([(id, [f])], ls) = (97, id) : fst (append (fromIntegral (length ls)) ls)

-- -- compress ((id, [f]):fs, ls) = (read [f], id) : fst (append (fromIntegral (length ls)) ls)
-- -- compress ((id, fs):_, (idl, ls):_) = []

-- append :: Integer -> [Chunk] -> ([Compressed], [Chunk])
-- append e [] = ([(98, 98)], [(98, [])])
-- append e [(id, c)] = ([(99, 99)], [(99, [])])
-- append e ((id, c:ls):cs)
--   -- | e > lr = ((lr, c, id) : fst rest, snd rest)
--   | e > read [c] = first ((read [c], id) :) rest
--   | e < lr = ([(e, id)], (id, show (lr - e) ++ ls) : cs)
--   | e == lr = ([(e, id)], cs)
--     where
--       lr = read [c]
--       rest = append (e - lr) cs

