{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List

readInt :: Text -> Int
readInt = read . T.unpack

parseInput :: [Text] -> (Int, [Int])
parseInput input = (timestamp, buses)
    where
        timestamp = readInt (input !! 0)
        buses = map readInt $ filter (/= "x") $ T.splitOn "," (input !! 1)

findEarliest :: Int -> [Int] -> (Int, Int)
findEarliest timestamp buses =
    minimum $ zip (map findEachBus buses) buses
    where
        findEachBus bus = head (dropWhile (< timestamp) [bus, 2*bus ..])
 
part1 :: [Text] -> Int
part1 input = (time - timestamp) * bus
    where
        (timestamp, buses) = parseInput input
        (time, bus) = findEarliest timestamp buses

main :: IO ()
main = do
    content <- readFile "./test/data/day-13-input.txt"
    let input = T.lines $ T.pack content 
    print $ part1 input
