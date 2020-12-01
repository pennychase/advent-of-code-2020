module Day1 where

import Data.List (subsequences, sort)

readInt :: String -> Int
readInt = read

findSumEq :: Int -> [Int] -> Maybe (Int, Int)
findSumEq x xs =
    findSumEq' combinationsOfTwo
    where
        combinationsOfTwo = filter (\y -> length y == 2) $ subsequences (sort xs)
        findSumEq' [] = Nothing
        findSumEq' ([n, m]:ns)
            | n + m == x = Just (n, m)
            | otherwise = findSumEq' ns

main :: IO ()
main = do
    contents <- readFile "day-1-input.txt"
    let nums = map readInt $ lines contents
    let result = findSumEq 2020 nums
    case result of
        Nothing     -> putStrLn "No pair sums to 2020"
        Just (n,m)  -> putStrLn $ "The product of " ++ show (n,m) ++ " is " ++ show (n*m)

    