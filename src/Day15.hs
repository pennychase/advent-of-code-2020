module Day15 where

import Data.Map.Strict ( Map )
import qualified Data.Map as M

type Game = (Int, Int, Map Int Int)

fst3 :: Game -> Int
fst3 (num, _, _) = num

snd3 :: Game -> Int
snd3 (_, pos, _) = pos

initialize :: [Int] -> Game
initialize xs = (0, length xs + 1, positions)
    where
        positions = M.fromList $ zip xs [1 .. length xs]

next :: Game -> Game
next (num, cur, positions) = 
    case M.lookup num positions of
        Nothing -> (0, cur + 1, M.insert num cur positions)
        Just old -> (cur - old, cur + 1, M.insert num cur positions)

part1 :: [Int] -> Int -> Int
part1 xs stop = fst3 $ go (initialize xs)
    where
        go :: Game -> Game
        go game =
            if snd3 game == stop
                then game
                else go (next game)




