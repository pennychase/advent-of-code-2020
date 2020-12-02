module Day2 where

import Text.Regex.TDFA

readInt :: String -> Int
readInt = read

-- Break up each rule/pwd into components: low number, high number, letter, password
pwdReqRegex :: String -> (String, String, String, [String])
pwdReqRegex str = str =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" 

type RuleFunc = (Char -> Int -> Int -> String -> Bool)

-- Part 1 test: character c appears in pwd between low and high times
meetsRule :: RuleFunc
meetsRule c low high pwd =
    between . length $ filter (== c) pwd
    where
        between n = low <= n && n <= high

-- Part 2 test: character c appears at position low or position high, but not both
-- Positions start at 1
meetsRule2 :: RuleFunc
meetsRule2 c low high pwd = (l == c || h == c) && (l /= h)
    where
        l = pwd !! (low - 1)
        h = pwd !! (high - 1)

-- Applies meetsRule to regex output
matchOne :: RuleFunc -> (String, String, String, [String]) -> Bool
matchOne f (_,_,_,[l,h,c,s]) = f (head c) (readInt l) (readInt h) s

main :: IO ()
main = do
    contents <- readFile "./test/data/day-2-input.txt"
    let regexes = map pwdReqRegex $ lines contents
    let matches1 = filter (matchOne meetsRule) regexes
    let matches2 = filter (matchOne meetsRule2) regexes
    print $ length matches1
    print $ length matches2