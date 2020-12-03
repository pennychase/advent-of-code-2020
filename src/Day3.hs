module Day3 where

-- Move through the grid using a slope given by Right and Down, counting the points
-- that have '#'
move :: Int -> Int -> Int -> Int -> [String] -> Int
move row col right down grid 
    | newRow > length grid - 1 = 0
    | otherwise = count ((grid !! newRow) !! newCol) + move newRow newCol right down grid
    where
        count pos = if pos == '#' then 1 else 0
        newRow = row + down
        newCol = col + right

-- Part 1: Moving through the grid with a single slope
singleSlope :: (Int, Int) -> [String] -> Int
singleSlope (r, d) = move 0 0 r d 

-- Part 2: Moving through the grid with a multiple slopes
multipleSlopes :: [(Int, Int)] -> [String] -> Int
multipleSlopes slopes grid = 
    foldr (\s x -> singleSlope s grid * x) 1 slopes

main :: IO ()
main = do
    contents <- readFile "./test/data/day-3-input.txt"
    let grid = map cycle $ lines contents
    print $ singleSlope (3, 1) grid
    print $ multipleSlopes [(1,1), (3,1), (5,1), (7,1), (1, 2)] grid