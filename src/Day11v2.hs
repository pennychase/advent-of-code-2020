module Day11v2 where

import Data.Vector (Vector, (//), (!?), (!))
import qualified Data.Vector as V 
import Data.Maybe ( mapMaybe )

type Floor = Vector (Vector Char)

mkFloor :: [String] -> Floor
mkFloor rows =
    floor // rows'
    where
        nrows = length rows
        floor = V.replicate nrows V.empty
        rows' = zip [0 .. (nrows - 1)] (map V.fromList rows)

fLookup :: (Int, Int) -> Floor -> Maybe Char
fLookup (i, j) floor = floor !? i >>= (!? j)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (n,m) = [ (n + i, m + j) | i <- [-1 .. 1], j <- [-1 .. 1], (i,j) /= (0,0)]

occupySeat :: (Int, Int) -> Floor -> Char
occupySeat i floor =
    if '#' `elem` places
        then 'L'
        else '#'
    where
        places = mapMaybe (`fLookup` floor) (adjacent i) 

emptySeat :: (Int, Int) -> Floor -> Char
emptySeat i floor =
    if length (filter (== '#') places) >= 4
        then 'L'
        else '#'
    where
        places = mapMaybe (`fLookup` floor) (adjacent i)

newValue :: (Int, Int) -> Floor -> Char
newValue i floor = 
    case fLookup i floor of
        Just '.' -> '.'
        Just 'L' -> occupySeat i floor
        Just '#' -> emptySeat i floor

newRow :: Int -> Floor -> String
newRow i floor = 
    map (\j -> newValue (i, j) floor) cols
    where
        cLen = V.length (floor ! i) - 1
        cols = [0 .. cLen]

newFloor :: Floor -> Floor
newFloor floor =
    mkFloor $ map (`newRow` floor) rows
    where
        rows = [0 .. V.length floor - 1]
        
step :: Floor -> Floor
step floor 
    | floor' == floor = floor
    | otherwise = step floor'
    where
        floor' = newFloor floor

part1 :: [String] -> Int
part1 input =
    length (filter (== '#') (concatMap V.toList (V.toList stable)))
    where
        stable = step (mkFloor input)
  
main :: IO ()
main = do
    content <- readFile "./test/data/day-11-input.txt"
    let input = lines content
    print $ part1 input