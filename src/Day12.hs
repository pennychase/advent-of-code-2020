{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Heading = North | East | South | West | Rght | Lft | Forward
    deriving (Show, Eq, Enum)

data Direction = N | E | S | W
    deriving (Show, Eq, Ord, Enum)

data Move = Move Heading Int
    deriving (Show, Eq)

data Position = Position Direction (Int, Int)
    deriving (Show, Eq)

toDir :: Heading -> Direction
toDir h = toEnum (fromEnum h) :: Direction

-- Parser

type MParser = Parsec Void Text

parseMoves :: MParser [Move]
parseMoves = M.many moveParser

moveParser :: MParser Move
moveParser = do
    heading <- headingParser
    value <- numberParser
    M.eol <|> (M.eof >> return "")
    return $ Move heading value

headingParser :: MParser Heading
headingParser = M.choice
    [ North <$ M.char 'N'
    , South <$ M.char 'S'
    , East <$ M.char 'E'
    , West <$ M.char 'W'
    , Rght <$ M.char 'R'
    , Lft <$ M.char 'L'
    , Forward <$ M.char 'F'
    ]

numberParser :: MParser Int
numberParser = read <$> M.try (M.some M.digitChar)

-- Part 1
allMoves :: Position -> [Move] -> Position
allMoves position [] = position
allMoves current (m:ms) = allMoves (nextPosition current m) ms

nextPosition :: Position -> Move -> Position
nextPosition (Position dir (x,y)) (Move Rght n) = Position (changeDir dir n) (x, y)
nextPosition (Position dir (x,y)) (Move Lft n) = Position (changeDir dir (-n)) (x, y)
nextPosition (Position dir (x,y)) (Move Forward n) = Position dir (changeCoords dir n (x, y))
nextPosition (Position dir (x,y)) (Move d n) = Position dir (changeCoords (toDir d) n (x, y))

changeDir :: Direction -> Int -> Direction
changeDir d x = toEnum $ cycle [start, start + offset .. ] !! (abs x `div` 90) `mod` 4
    where
        start = fromEnum d
        offset = if x < 0 then -1 else 1

changeCoords :: Direction -> Int -> (Int, Int) -> (Int, Int)
changeCoords N n (x, y) = (x, y + n)
changeCoords S n (x, y) = (x, y - n)
changeCoords E n (x, y) = (x + n, y)
changeCoords W n (x, y) = (x - n, y)

distance :: Int -> Int -> Int
distance x y = abs x + abs y

part1 :: Text -> Int
part1 input =
    distance x y 
    where
        (Position _ (x, y)) = case moves of
            Left s -> error (show s)
            Right ms -> allMoves (Position E (0,0)) ms
        moves = M.runParser parseMoves "" input

main :: IO ()
main = do
    content <- readFile "./test/data/day-12-input.txt"
    print $ part1 (T.pack content)
