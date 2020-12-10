module Day9 where

import Data.List ( tails, union )
import Data.Maybe ( isNothing, fromJust )

readInt :: String -> Int
readInt = read

sumOfPairs :: [Int] -> [Int]
sumOfPairs nums =
    foldr union [] (zipWith (\n m -> map (+ n) m) xs xss)
    where
        xs = init nums
        xss = tail (init (tails nums))

findNumber :: Int -> [Int] -> Maybe Int
findNumber n lis =
    if length lis > n then findNumber' lis else Nothing
    where
        findNumber' :: [Int] -> Maybe Int
        findNumber' [] = Nothing
        findNumber' lis =
            if num `elem` sumOfPairs testList
                then findNumber' $ tail testList ++ rest
                else Just num
            where 
                (testList, rest) = splitAt n lis
                num = head rest

findNumSumList :: Int -> [Int] -> Maybe Int
findNumSumList _ [] = Nothing
findNumSumList val (x:xs) =
    case n of
        Nothing -> Nothing
        Just m -> if head m /= x
                    then Just (minimum m + maximum m)
                    else findNumSumList val xs
    where
        n = findNumSumList' x [x] val xs

findNumSumList' :: Int -> [Int] -> Int -> [Int] -> Maybe [Int]
findNumSumList' _ _ _ [] = Nothing
findNumSumList' first acc val (x:xs) = 
    case compare (sum acc') val of
        LT -> findNumSumList' first acc' val xs
        EQ -> Just acc'
        GT -> Just [first]
    where
        acc' = x:acc


part1 :: [Int ]-> Maybe Int
part1 input = findNumber 25 input

part2 :: Maybe Int -> [Int] ->  Maybe Int
part2 n input = do
    if isNothing n
        then Nothing
        else findNumSumList (fromJust n) input

main :: IO ()
main = do
    contents <- readFile "./test/data/day-9-input.txt"
    let nums = readInt <$> lines contents
    let n = part1 nums
    print $ "Part 1: " ++ show n
    print $ "Part 2: " ++ show (part2 n nums)
    