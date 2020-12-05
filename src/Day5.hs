module Day5 where

import Data.List ( sort )

-- pred determines which half is searched: if predicate is true, use the lower half
binarySearch :: (Char -> Bool) -> Int -> Int -> [Char] -> Int
binarySearch pred low high [] = low
binarySearch pred low high (x:xs) =
    if pred x
        then binarySearch pred low mid xs
        else binarySearch pred  (mid + 1) high xs
    where mid = (low + high) `div` 2

-- Use binarySearch with appropriate predicates to find the seat row and column
-- For rows, input is F for lower hald and B for upper half, so test =='F'
-- For columns, input is L for lower half and R for upper half, so test =='L'
findSeat :: String -> Int
findSeat xs =
    binarySearch (=='F') 0 127 rows * 8 + binarySearch (=='L') 0 7 cols
    where
        rows = takeWhile (`elem` "FB") xs
        cols = dropWhile (`elem` "FB") xs

part1 :: [String] -> Int
part1 seats = maximum $ map findSeat seats

-- Find the one empty seat by sorting the list of seats and then doing a linear search
part2 :: [String] -> Int
part2 seats = findGap (head seats') (tail seats') 
    where
        seats' = sort $ map findSeat seats
        findGap cur [] = cur
        findGap cur (x:xs) =
            if x - cur > 1 then cur + 1 else findGap x xs

main :: IO ()
main = do
    contents <- readFile "./test/data/day-5-input.txt"
    let seats = lines contents
    print $ part1 seats
    print $ part2 seats