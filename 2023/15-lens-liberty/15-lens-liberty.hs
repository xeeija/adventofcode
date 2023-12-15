import Control.Applicative (liftA2)
import Data.List.Split (splitOn)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve

solve = sum . map hash . splitOn ","

hash = foldl (\acc x -> (acc + fromEnum x) * 17 `mod` 256) 0

-- Part Two



-- >>> hash "HASH"
-- 52

-- >>> (fromEnum 'H' * 17) `rem` 256
-- 200

-- >>> solve "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
-- 1320
 
