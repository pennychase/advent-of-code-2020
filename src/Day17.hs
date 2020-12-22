module Day17 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Control.Monad


initialize :: [String] -> Map [Int] Char
initialize lines =
    M.fromList $ zip indices (cycle cells)
    where
        n = length lines
        indices = [[x,y,0] | x <- [0 .. n-1], y <- [0 .. n-1]]
        cells = concat lines

neighbors :: [Int] -> [[Int]]
neighbors [x,y,z] = delete [x,y,z] (map (\[i,j,k] -> [x+i, y+j, z+k]) $ replicateM 3 [-1,0,1])

getCells :: Map [Int] Char -> [[Int]]
getCells m = [[x,y,z] | x <- [x1-1 .. x2+1], y <- [y1-1 .. y2+1], z <- [z1-1 .. z2+1]]
    where
        [x1,y1,z1] = minimum $ M.keys m
        [x2,y2,z2] = maximum $ M.keys m

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
        m = iterate step (initialize (lines input)) !! 6

main :: IO ()
main = do
    contents <- readFile "./test/data/day-17-input.txt"
    print $ part1 contents
    
