{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq, (|>), Seq ((:<|)))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as M

type Deck = Seq Int

type History = Map (Deck, Deck) Int

data Round = Round { history :: History
                   , player1 :: Deck
                   , player2 :: Deck
                   }
        deriving (Show)

data Winner = Player1 | Player2
        deriving (Show)

parseDeck :: Text -> Deck
parseDeck str =
    Seq.fromList $ read . T.unpack <$> tail (T.lines str)

parseFile :: String -> (Deck, Deck)
parseFile input = (parseDeck (head decks), parseDeck (last decks))
    where
        decks = T.splitOn "\n\n" $ T.pack input

-- Part 1: Combat
playRound :: (Deck, Deck) -> (Deck, Deck)
playRound (c1:<|c1s, c2:<|c2s) =
    if c1 > c2
        then (c1s |> c1 |> c2, c2s)
        else (c1s, c2s |> c2 |> c1)

playGame :: (Deck, Deck) -> Int
playGame (player1, player2) = loop (player1, player2)
    where
        loop (p1, p2)
            | Seq.null p1 = sumDeck p2
            | Seq.null p2 = sumDeck p1
            | otherwise = loop (playRound (p1, p2))
        sumDeck deck = 
            let n = Seq.length deck
            in sum $ Seq.zipWith (*) deck (Seq.fromList [n, n-1 .. 1])

-- Part 2: Recursive Combat
playRound2 :: Round -> Round
playRound2 rnd 
    | handSeen rnd                         = rnd { player2 = Seq.empty }
    | length c1s >= c1 && length c2s >= c2 = updateRound (winner, rnd)
    | c1 > c2                              = updateRound (Player1, rnd)
    | otherwise                            = updateRound (Player2, rnd)
    where
        c1:<|c1s = player1 rnd
        c2:<|c2s = player2 rnd
        (winner, _) = gameLoop rnd { history = M.empty, player1 = Seq.take c1 c1s, player2 = Seq.take c2 c2s }
 
playGame2 :: Round -> Int
playGame2 round = 
    case gameLoop round of
        (Player1, rnd) -> sumDeck (player1 rnd)
        (Player2, rnd) -> sumDeck (player2 rnd)
    where
        sumDeck deck = 
            let n = Seq.length deck
            in sum $ Seq.zipWith (*) deck (Seq.fromList [n, n-1 .. 1])

gameLoop :: Round -> (Winner, Round)
gameLoop round = loop round
    where
        loop rnd
            | Seq.null (player1 rnd) = (Player2, rnd)
            | Seq.null (player2 rnd) = (Player1, rnd)
            | otherwise = loop (playRound2 rnd)


handSeen :: Round -> Bool
handSeen rnd = 
    case M.lookup (player1 rnd, player2 rnd) (history rnd) of
        Just _ -> True
        Nothing -> False

updateRound :: (Winner, Round) -> Round
updateRound (winner, rnd) =
    case winner of
        Player1 -> newRnd { player1 = c1s |> c1 |> c2
                          , player2 = c2s
                          }
        Player2 -> newRnd { player1 = c1s
                          , player2 = c2s |> c2 |> c1
                          }
    where
        newRnd = rnd { history = M.insert (player1 rnd, player2 rnd) 0 (history rnd) }
        c1:<|c1s = player1 rnd
        c2:<|c2s = player2 rnd

part2 :: String -> Int
part2 input =
    playGame2 $ Round { history = M.empty, player1 = deck1, player2 = deck2 }
    where
        (deck1, deck2) = parseFile input
        
main :: IO ()
main = do
    content <- readFile "test/data/day-22-input.txt"
    print $ playGame (parseFile content)
    print $ part2 content