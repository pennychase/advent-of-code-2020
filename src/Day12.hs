{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Heading = N | E | S | W | R | L | F
    deriving (Show, Eq, Enum)

data Direction = North | East | South | West
    deriving (Show, Eq, Ord, Enum)

data Move = Move Heading Int
    deriving (Show, Eq)

data Position = Position Direction (Int, Int)
    deriving (Show, Eq)

data Ship = Ship {  wayPoint :: (Int, Int)
                  , position :: (Int, Int)
                  }

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
    [ N <$ M.char 'N'
    , S <$ M.char 'S'
    , E <$ M.char 'E'
    , W <$ M.char 'W'
    , R <$ M.char 'R'
    , L <$ M.char 'L'
    , F <$ M.char 'F'
    ]

numberParser :: MParser Int
numberParser = read <$> M.try (M.some M.digitChar)

getMoves :: Text -> [Move]
getMoves input =
    case M.runParser parseMoves "" input of
        Left s -> error (show s)
        Right moves -> moves

-- Part 1
allMoves :: Position -> [Move] -> Position
allMoves position moves = foldl nextPosition position moves

nextPosition :: Position -> Move -> Position
nextPosition (Position dir (x,y)) (Move R n) = Position (changeDir dir n) (x, y)
nextPosition (Position dir (x,y)) (Move L n) = Position (changeDir dir (-n)) (x, y)
nextPosition (Position dir (x,y)) (Move F n) = Position dir (changeCoords dir n (x, y))
nextPosition (Position dir (x,y)) (Move d n) = Position dir (changeCoords (toDir d) n (x, y))

changeDir :: Direction -> Int -> Direction
changeDir d x = toEnum $ cycle [start, start + offset .. ] !! (abs x `div` 90) `mod` 4
    where
        start = fromEnum d
        offset = if x < 0 then -1 else 1

changeCoords :: Direction -> Int -> (Int, Int) -> (Int, Int)
changeCoords North n (x, y) = (x, y + n)
changeCoords South n (x, y) = (x, y - n)
changeCoords East n (x, y) = (x + n, y)
changeCoords West n (x, y) = (x - n, y)

-- Part 2

allNavigates :: Ship -> [Move] -> Ship
allNavigates ship moves = foldl navigate ship moves

navigate :: Ship -> Move -> Ship
navigate ship (Move F n) = moveShip ship n
navigate ship (Move N n) = shiftWayPoint ship (0, n)
navigate ship (Move S n) = shiftWayPoint ship (0, -n)
navigate ship (Move E n) = shiftWayPoint ship (n, 0)
navigate ship (Move W n) = shiftWayPoint ship (-n, 0)
navigate ship (Move R n) = rotateWayPoint ship n
navigate ship (Move L n) = rotateWayPoint ship (360 - n)

moveShip :: Ship -> Int -> Ship
moveShip ship n = ship { position = (x + i * n, y + j * n)} 
    where
        (i, j) = wayPoint ship
        (x, y) = position ship

shiftWayPoint :: Ship -> (Int, Int) -> Ship
shiftWayPoint ship (i, j) = ship { wayPoint = (x + i, y + j)}
    where
        (x, y) = wayPoint ship

rotateWayPoint :: Ship -> Int -> Ship
rotateWayPoint ship deg
    | deg == 90 = ship { wayPoint = (y, -x) }
    | deg == 180 = ship { wayPoint = (-x, -y) }
    | deg == 270 = ship { wayPoint = (-y, x) }
    where
        (x, y) = wayPoint ship

distance :: Int -> Int -> Int
distance x y = abs x + abs y

part1 :: [Move] -> Int
part1 moves = uncurry distance pos
    where
        Position _ pos =  allMoves (Position East (0,0)) moves

part2 :: [Move] -> Int
part2 moves = uncurry distance (position ship)
    where
        ship = allNavigates Ship { wayPoint = (10, 1), position = (0, 0)} moves

main :: IO ()
main = do
    content <- readFile "./test/data/day-12-input.txt"
    let moves = getMoves $ T.pack content
    print $ part1 moves
    print $ part2 moves
