module Day6 where

import qualified Data.Map as M
import Lib ( splitList )

-- For both parts we'll create a Map from the input with the questions as the keys

-- Part 1
-- Since we want to count the number of questions answered by any person, all we're interested in 
-- is the number of keys in the map (each represents an answer answered by one or more people)
mkCounter :: String -> M.Map Char Int
mkCounter xs = M.fromList $ zip xs (repeat 0)

countsAny :: M.Map Char Int -> Int
countsAny m  = length $ M.keys m

part1 :: [[String]]-> Int
part1 xs = sum $ map (countsAny . mkCounter) $ map concat xs

-- Part 2
-- Since we want to count the number of questions everyone in the group answered, we need to
-- record the count of the number of responses to each question in the Map values and then compare 
-- with the number of people in the group (the length of the list of strings in mkCounter2)
mkCounter2 :: [String] -> (M.Map Char Int, Int)
mkCounter2 xs = ( M.fromListWith (+) $ zip (concat xs) (repeat 1), length xs )

countsAll :: (M.Map Char Int, Int)-> Int
countsAll (m, n) = length . filter (== n) $ M.elems m

part2 :: [[String]] -> Int
part2 xs = sum  $ map (countsAll . mkCounter2) xs

main :: IO ()
main = do
    contents <- readFile "./test/data/day-6-input.txt"
    let input = splitList (=="") (lines contents)
    print $ part1 input
    print $ part2 input