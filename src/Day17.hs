module Day17 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List ( delete )
import Control.Monad ( replicateM )


-- Initialize the Map 
-- The input is a 2 dimensional slice of the n-dimensional grid (so the coordinates of the other dimensions 
-- of the slice are 0)
initialize :: Int -> [String] -> Map [Int] Char
initialize ndims lines =
    M.fromList $ zip indices cells
    where
        n = length lines
        indices = map (++ replicate (ndims-2) 0) (replicateM 2 [0 .. n-1])
        cells = (concat lines) ++ (repeat '.')

-- Create the neighbors of the given cell, removing the cell itself
neighbors :: [Int] -> [[Int]]
neighbors cell = delete cell $ mapM (\x -> map (x +) [-1,0,1]) cell

-- Get the cells to be examined: these are the cells in the map plus the bordering cells (empty but candidates
-- to be filled in the current step)
getCells :: Map [Int] Char -> [[Int]]
getCells m =  
    sequence $ mkLists low high  
    where
        low = minimum $ M.keys m
        high = maximum $ M.keys m
        mkLists [] _ = []
        mkLists _ [] = []
        mkLists (l:ls) (h:hs) = [l-1 .. h+1] : mkLists ls hs

newCell :: [Int] -> Map [Int] Char -> ([Int], Char)
newCell cell m = (cell, new)
    where 
        new  = case M.findWithDefault '.' cell m of
            '#' -> if numActive == 2 || numActive == 3 then '#' else '.'
            '.' -> if numActive == 3 then '#' else '.'
        numActive = length $ filter (=='#') $ map (\n -> M.findWithDefault '.' n m) (neighbors cell)


step :: Map [Int] Char -> Map [Int] Char
step m = M.fromList (go cells [])
    where
        cells = getCells m
        go [] new = new
        go (c:cs) new = go cs (newCell c m : new)

part1 :: String -> Int
part1 input = length $ filter (=='#') (M.elems m)
    where
        m = iterate step (initialize 3 (lines input)) !! 6

part2 :: String -> Int
part2 input = length $ filter (=='#') (M.elems m)
    where
        m = iterate step (initialize 4 (lines input)) !! 6

main :: IO ()
main = do
    contents <- readFile "./test/data/day-17-input.txt"
    print $ part1 contents
    print $ part2 contents
    
