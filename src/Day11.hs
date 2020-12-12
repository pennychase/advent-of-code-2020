module Day11 where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )

-- Solution using Map to represent the floor

type Floor = Map (Int, Int) Char

mkFloor :: [String] -> Floor
mkFloor input = 
    M.fromList  $ zip indices (concat input)
    where
        rowMax = length input - 1
        colMax = length (head input) - 1
        indices = concatMap (\ x -> zip (repeat x) [0 .. colMax]) [0 .. rowMax]

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (n,m) = [ (n + i, m + j) | i <- [-1 .. 1], j <- [-1 .. 1], (i,j) /= (0,0)]

occupySeat :: (Int, Int) -> Floor -> Char
occupySeat i floor =
    if '#' `elem` places
        then 'L'
        else '#'
    where
        places = mapMaybe (`M.lookup` floor) (adjacent i) 

emptySeat :: (Int, Int) -> Floor -> Char
emptySeat i floor =
    if length (filter (== '#') places) >= 4
        then 'L'
        else '#'
    where
        places = mapMaybe (`M.lookup` floor) (adjacent i)

newValue :: (Int, Int) -> Floor -> Char
newValue i floor = 
    case M.lookup i floor of
        Just '.' -> '.'
        Just 'L' -> occupySeat i floor
        Just '#' -> emptySeat i floor
   
newFloor :: Floor -> Floor
newFloor floor =
    M.fromList $ zip indices newValues
    where
        indices = M.keys floor
        newValues = map (`newValue` floor) indices

step :: Floor -> Floor
step floor 
    | floor == floor' = floor
    | otherwise = step floor'
    where
        floor' = newFloor floor

part1 :: [String] -> Int
part1 input =
    M.size . M.filter (== '#') $ step (mkFloor input)

main :: IO ()
main = do
    content <- readFile "./test/data/day-11-input.txt"
    let input = lines content
    print $ part1 input