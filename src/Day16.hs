{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Void ( Void )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Rule = Rule
    { name :: String
    , lower :: Set Int
    , upper :: Set Int
    }
    deriving (Show)

type MParser = Parsec Void String

-- Parser

parseInput :: MParser ([Rule], Set Int, [Set Int])
parseInput = do
    rules <- M.many parseRule
    M.many M.eol
    M.string "your ticket:"
    M.eol
    myTicket <- parseTicket
    M.many M.eol
    M.string "nearby tickets:"
    M.eol
    nearbyTickets <- parseRest
    -- nearbyTickets <- M.many parseTicket
    M.eol <|> (M.eof >> return "")
    return (rules, myTicket, mkTickets nearbyTickets)

parseRule :: MParser Rule
parseRule = do
    name <- M.some (M.letterChar <|> M.char ' ')
    M.string ": "
    ranges <- parseRange `M.sepBy` " or "
    M.eol <|> (M.eof >> return "")
    return $ Rule name (head ranges) (last ranges)

parseRange :: MParser (Set Int)
parseRange = do
    bounds <- parseNumber `M.sepBy` M.char '-'
    return $ S.fromList [ head bounds .. last bounds]

parseTicket :: MParser (Set Int)
parseTicket = do
    nums <- parseNumber `M.sepBy` M.char ','
    M.eol <|> (M.eof >> return "")
    return $ S.fromList nums

parseRest :: MParser String
parseRest = do
    rest <- M.many (M.digitChar <|> M.char ',' <|> M.char '\n')
    M.eol <|> (M.eof >> return "")
    return rest

parseNumber :: MParser Int
parseNumber = read <$> M.some M.digitChar

mkTickets :: String -> [Set Int]
mkTickets str =
    map S.fromList tickets
    where
        tickets = map textToNums $ T.lines $ T.pack str
        textToNums txt =  map (read . T.unpack) $ T.splitOn "," txt

-- Part 1
-- Find all invalid fields in scanned tickets

invalidFields :: [Rule] -> [Set Int] -> [Int]
invalidFields rules tickets =
    mconcat $ map (S.toList . (`S.difference` validFields)) tickets
    where
        validFields = (S.unions $ map upper rules) `S.union` (S.unions $ map lower rules)
        
part1 :: ([Rule], Set Int, [Set Int]) -> Int
part1 (rules, _, tickets) = sum $ invalidFields rules tickets



main :: IO ()
main = do
    content <- readFile "./test/data/day-16-input.txt"
    case M.runParser parseInput "" content of
        Left s -> error (show s)
        Right input -> print $ part1 input


