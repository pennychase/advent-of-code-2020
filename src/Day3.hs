module Day3 where

move :: Int -> Int -> Int -> Int -> [String] -> Int
move row col right down grid 
    | newRow > length grid - 1 = 0
    | otherwise = count ((grid !! newRow) !! newCol) + move newRow newCol right down grid
    where
        count pos = if pos == '#' then 1 else 0
        newRow = row + down
        newCol = col + right

part1 :: [String] -> Int
part1 grid = move 0 0 3 1 grid

part2 :: [String] -> Int
part2 grid = 
    move 0 0 1 1 grid *
    move 0 0 3 1 grid *
    move 0 0 5 1 grid *
    move 0 0 7 1 grid *
    move 0 0 1 2 grid 


main :: IO ()
main = do
    contents <- readFile "./test/data/day-3-input.txt"
    let grid = map cycle $ lines contents
    print $ part1 grid
    print $ part2 grid