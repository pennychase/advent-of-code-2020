module Day2 where

import Text.Regex.TDFA

meetsRule :: Char -> Int -> Int -> String -> Bool
meetsRule c low high pwd =
    between . length $ filter (== c) pwd
    where
        between n = low <= n && n <= high

main :: IO ()
main = do
    contents <- readFile "day-2-input.txt"
    putStrLn "Done"