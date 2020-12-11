module Day10 where

import qualified Data.MultiSet as S
import Data.MultiSet(MultiSet) 
import Data.List ( sort ) 

readInt :: String -> Int
readInt = read

findChain :: [Int] -> MultiSet Int
findChain adapters = findChain' 0 (sort adapters)  S.empty
    where
        findChain' _ [] used = S.insert 3 used
        findChain' source (a:as) used =
            if diff > 3
                then S.empty
                else findChain' a as (S.insert diff used)
            where diff = a - source


part1 :: [Int] -> String
part1 input = 
    if null diffs then "No solution" else show $ (S.occur 1 diffs) * (S.occur 3 diffs) 
    where
        diffs = findChain input
        
main :: IO ()
main = do
    contents <- readFile "./test/data/day-10-input.txt"
    let input = readInt <$> lines contents
    print $ part1 input