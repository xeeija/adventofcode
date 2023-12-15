import Control.Applicative (liftA2)
import Data.List
import Data.List.Split
import Data.Ord (Down(Down))

main :: IO ()
main = interact $ (++ "\n") . show . liftA2 (,) solve solve3 . lines

solve = sum . weight . tiltNorth

weight = zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

tiltNorth = transpose . map (intercalate "#" . map (sortOn Down) . linesBy (== '#')) . transpose

-- Part Two

solve2 = sum . weight . (!! 4) . iterate spin

solve3 = zip [500..] . map (sum . weight) . take 50 . drop 500 . iterate spin

tilt =  map (foldr1 (++) . map sort . split (oneOf "#"))

turn = transpose . reverse

spin = (!! 4) . iterate (tilt . turn)

spin' x y = take y . drop x . iterate (tilt . turn)

spin'' x = (!! x) . iterate spin

-- TODO: determine pattern and stop, if we are at the pattern
-- or rather: skip (desired cycle div. pattern length) and jump straight to last
-- 1. drop the first bit (like 500) -> maybe not needed with find pattern
-- 2. determine pattern length (14 in my case)
-- -> pattern https://stackoverflow.com/questions/70140102/find-longest-repeating-sequence-in-list
-- 3. get the 1.000.000.000 mod pattern length's element (in my case (1 mrd - 500) mod 14 = 10)


-- [104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647,104666,104667,104663,104650,104649,104640,104651,104662,104671,104659,104654,104645,104644,104647]

-- >>> solve $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- 136

-- >>> solve2 $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- 69

-- >>> solve3 $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- [104,87,69,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63,68,69,69,65,64,65,63]

-- >>> intercalate "\n" . spin'' 401 $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".....#....\n....#...O#\n.....##...\n..O#......\n.....OOO#.\n.O#...O#.#\n....O#....\n......OOOO\n#...O###.O\n#.OOO#...O"


-- >>> intercalate "\n" . reverse . transpose . lines $ "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".#.O.#O...\n....#.....\n....O#.O#.\n..#...O.#.\n#.#..O#.##\n.#.O......\n.O.#......\n.O...#O..O\n...OO....O\nOO.O.O..##"


-- Tilt

-- >>> intercalate "\n" $ tilt $ lines ".#.O.#O...\n....#.....\n....O#.O#.\n..#...O.#.\n#.#..O#.##\n.#.O......\n.O.#......\n.O...#O..O\n...OO....O\nOO.O.O..##"
-- ".#..O#...O\n....#.....\n....O#.O#.\n..#....O#.\n#.#..O#.##\n.#.......O\n..O#......\n....O#..OO\n.......OOO\n....OOOO##"



-- >>> intercalate "\n" $ turn $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- "##..O.O.OO\nO....OO...\nO..O#...O.\n......#.O.\n......O.#.\n##.#O..#.#\n.#.O...#..\n.#O.#O....\n.....#....\n...O#.O.#."

-- >>> intercalate "\n" $ tilt $ turn $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- "##....OOOO\n.......OOO\n..OO#....O\n......#..O\n.......O#.\n##.#..O#.#\n.#....O#..\n.#.O#....O\n.....#....\n...O#..O#."

-- >>> intercalate "\n" $ tilt $ turn $ tilt $ turn $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- "....#....#\n..###....#\n.........O\n..OO#....O\n#.#....#..\n.#........\n....OO#..O\n..O##..OOO\n#....#..OO\n...O#.OOOO"

-- >>> intercalate "\n" $ tilt $ turn $ tilt $ turn $ tilt $ turn $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".#...#....\n....#.....\n....O#.O#.\n.O#....O#.\n#.#..O#.##\n.#.......O\n..O#......\n...OO#....\n.......OOO\n..OOOOOO##"

-- >>> intercalate "\n" $ tilt $ turn $ tilt $ turn $ tilt $ turn $ tilt $ turn $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".....#....\n....#...O#\n...OO##...\n.OO#......\n.....OOO#.\n.O#...O#.#\n....O#....\n......OOOO\n#...O###..\n#..OO#...."

-- >>> intercalate "\n" $ spin 12 $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".....#....\n....#...O#\n.....##...\n..O#......\n.....OOO#.\n.O#...O#.#\n....O#...O\n.......OOO\n#...O###.O\n#.OOO#...O"

-- >>> intercalate "\n" $ spin 12 $ lines "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- ".....#....\n....#...O#\n.....##...\n..O#......\n.....OOO#.\n.O#...O#.#\n....O#...O\n.......OOO\n#...O###.O\n#.OOO#...O"

