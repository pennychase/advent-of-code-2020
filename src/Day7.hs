{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Text (Text) 
import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ( union )

-- Normalize bag/bags to bag. If there is more than one bag it's a plural in the input, so make it singular.
parseItem :: Text -> (Text, Int)
parseItem xs = (item', count)
    where
        item = T.strip (T.dropWhile isDigit xs)
        count = read (T.unpack $ T.takeWhile isDigit xs) :: Int
        item' = if count == 1 then item else T.init item

parseList :: Text -> [(Text, Int)]
parseList xs = map parseItem (T.splitOn ", " xs)

-- Line has a period at the end, so remove with T.init
-- Normalize bag/bags to bag. The key (the bag before "contains") is plural so make it singular
parseLine :: Text -> (Text, [(Text, Int)])
parseLine line = 
    case T.splitOn " contain " (T.init line) of
        [key, "no other bags"] -> (T.init key, [])
        [key, list]            -> (T.init key, parseList list)

mkGraphs :: [(Text, [(Text, Int)])] -> (Map Text [(Text, Int)], Map Text [Text])
mkGraphs input = (contains, containedIn)
    where
        contains = M.fromList input
        containedIn = M.unionsWith (++) (map reverseEdge input)
        reverseEdge :: (Text, [(Text, Int)]) -> Map Text [Text]
        reverseEdge (key, items) = M.fromList $ zip (fst (unzip items)) (repeat [key])

searchContainedIn :: Map Text [Text] -> Text-> [Text]
searchContainedIn m x =
    case M.lookup x m  of
        Nothing -> []
        Just xs -> xs `union` concatMap (searchContainedIn m) xs

searchContains :: Map Text [(Text, Int)] -> Text -> Int
searchContains m x = 
    case M.lookup x m of
        Nothing -> 0
        Just xs -> sum $ map num xs
        where
            num (i, c) = c + c * searchContains m i

part1 :: Text -> [Text] -> Int
part1 e input = length $ searchContainedIn g e
    where
        (_, g) = mkGraphs $ map parseLine input

part2 :: Text -> [Text] -> Int
part2 e input = searchContains g e
    where 
        (g, _) = mkGraphs $ map parseLine input

main :: IO ()
main = do
    contents <- readFile "./test/data/day-7-input.txt"
    let input = T.lines $ T.pack contents
    print $ part1 "shiny gold bag" input
    print $ part2 "shiny gold bag" input
