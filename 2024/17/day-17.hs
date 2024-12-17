import GHC.Base (liftA2)
import Data.List.Split (splitOn)
import Data.Bits (Bits(xor))
import Data.List (isPrefixOf)
import Data.Char (isNumber)

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) id id

-- (A, B, C, Instruction Pointer ("Program Counter" or PC), Instructions, Output)
type Computer = (Int, Int, Int, Int, [Int], [Int])

-- (A, B, C, Instruction Pointer ("Program Counter" or PC), Instructions) -> Output
-- type Computer = Int -> Int -> Int -> Int -> [Instruction] -> [Int]
-- type Computer = [Int] -> (Int, Int, Int, Int) -> [Int]

computer :: Computer -> Computer
computer state@(a, b, c, pc, memory, out)
  | pc >= length memory = (a, b, c, pc, memory, reverse out) -- state
  | otherwise = computer $ (ops !! (memory !! pc)) state


ops :: [Computer -> Computer]
ops = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

-- parse :: String -> Computer
-- parse x = 

instructions :: String -> [Int]
instructions = map read . splitOn ","

-- instructions' :: String -> [(Int, Int)]
instructions' :: String -> [(Int, Int)]
instructions' = inst . splitOn ","
  where
    inst [] = []
    inst [_] = []
    inst (op:arg:ops) = (read op, read arg) : inst ops


parse :: String -> Computer
parse s = parse' (lines s) (0, 0, 0, 0, [], [])
  where
    parse' :: [String] -> Computer -> Computer
    parse' [] state = state
    parse' (x:xs) state@(a, b, c, pc, memory, out)
      | "Register A" `isPrefixOf` s = parse' xs (reg, b, c, pc, memory, out)
      | "Register B" `isPrefixOf` s = parse' xs (a, reg, c, pc, memory, out)
      | "Register C" `isPrefixOf` s = parse' xs (a, b, reg, pc, memory, out)
      | "Program" `isPrefixOf` s = parse' xs (a, b, c, pc, instructions (dropWhile (not . isNumber) x), out)
      | otherwise = parse' xs state
      where
        -- reg :: Int
        reg = read (dropWhile (not . isNumber) x) :: Int

-- maybe use Data.Tuple.Ops for updating elements in tuple 
-- https://hackage.haskell.org/package/tuple-sop-0.3.1.0/docs/Data-Tuple-Ops.html


adv :: Computer -> Computer
adv (a, b, c, pc, memory, out) = (adv', b, c, pc + 2, memory, out)
  where
    -- adv' arg = a `div` (2 ^ combo arg a b c)
    adv' = a `div` 2 ^ combo (memory !!+ pc) a b c

bxl :: Computer -> Computer
bxl (a, b, c, pc, memory, out) = (a, b `xor` (memory !!+ pc), c, pc + 2, memory, out)
  -- where
  --   mod 8 == bitwise "and" with 0b111 (lowest 3 bits)
  --   bxl' = xor b

bst :: Computer -> Computer
bst (a, b, c, pc, memory, out) = (a, bst', c, pc + 2, memory, out)
  where
    -- mod 8 == bitwise 'and' 0b111 (lowest 3 bits)
    -- bst' arg = combo arg a b c `mod` 8
    bst' = combo (memory !!+ pc) a b c `mod` 8

jnz :: Computer -> Computer
jnz (a, b, c, pc, memory, out) = (a, b, c, jnz' (memory !!+ pc), memory, out)
  where
    -- mod 8 == bitwise 'and' 0b111 (lowest 3 bits)
    -- jnz' arg | a == 0 = pc | otherwise = arg
    jnz' arg = if a == 0 then pc + 2 else arg

bxc :: Computer -> Computer
bxc (a, b, c, pc, memory, out) = (a, b `xor` c, c, pc + 2, memory, out)

out :: Computer -> Computer
out (a, b, c, pc, memory, output) = (a, b `xor` c, c, pc + 2, memory, out')
  where
    -- out' arg = combo arg a b c `mod` 8
    out' = combo (memory !!+ pc) a b c `mod` 8 : output
    -- out' = output ++ [combo (memory !!+ pc) a b c `mod` 8]

bdv :: Computer -> Computer
bdv (a, b, c, pc, memory, out) = (a, bdv', c, pc + 2, memory, out)
  where
    bdv' = a `div` 2 ^ combo (memory !!+ pc) a b c

cdv :: Computer -> Computer
cdv (a, b, c, pc, memory, out) = (a, b, cdv', pc + 2, memory, out)
  where
    cdv' = a `div` 2 ^ combo (memory !!+ pc) a b c


(!!+) :: [a] -> Int -> a
(!!+) xs x = xs !! (x + 1)


combo :: Int -> Int -> Int -> Int -> Int
combo 4 a _ _ = a
combo 5 _ b _ = b
combo 6 _ _ c = c
combo 7 _ _ _ = undefined
combo op _ _ _ = op

