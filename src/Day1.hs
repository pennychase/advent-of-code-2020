module Day1 where

import Data.List (subsequences, sort)

readInt :: String -> Int
readInt = read

combinationsOfLength :: Int -> [Int] -> [[Int]]
combinationsOfLength n xs = filter (\y -> length y == n) $ subsequences (sort xs)

-- find n numbers in xs (a list of numbers) whose sum is x
-- Construct combinations of length n and then find the combination that sums to x
findSumEq :: Int -> Int -> [Int] -> Maybe [Int]
findSumEq n x xs =
    findSumEq' $ combinationsOfLength n xs
    where
        findSumEq' [] = Nothing
        findSumEq' (m:ms)
            | sum m == x = Just m
            | otherwise = findSumEq' ms

main :: IO ()
main = do
    contents <- readFile "./test/data/day-1-input.txt"
    let nums = map readInt $ lines contents
    -- Part 1
    let result = findSumEq 2 2020 nums
    case result of
        Nothing     -> putStrLn "No pair sums to 2020"
        Just n  -> putStrLn $ "The product of " ++ show n ++ " is " ++ show (product n)
    -- Part 2
    let result' = findSumEq 3 2020 nums
    case result' of
        Nothing     -> putStrLn "No triple sums to 2020"
        Just n  -> putStrLn $ "The product of " ++ show n ++ " is " ++ show (product n)

    