{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq, (|>), Seq ((:<|)))
import qualified Data.Sequence as Seq

parseDeck :: Text -> Seq Int
parseDeck str =
    Seq.fromList $ read . T.unpack <$> tail (T.lines str)

parseFile :: String -> (Seq Int, Seq Int)
parseFile input = (parseDeck (head decks), parseDeck (last decks))
    where
        decks = T.splitOn "\n\n" $ T.pack input

playRound :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
playRound (c1:<|c1s, c2:<|c2s) =
    if c1 > c2
        then (c1s |> c1 |> c2, c2s)
        else (c1s, c2s |> c2 |> c1)

playGame :: (Seq Int, Seq Int) -> Int
playGame (player1, player2) = loop (player1, player2)
    where
        loop (p1, p2)
            | Seq.null p1 = sumDeck p2
            | Seq.null p2 = sumDeck p1
            | otherwise = loop (playRound (p1, p2))
        sumDeck deck = 
            let n = Seq.length deck
            in sum $ Seq.zipWith (*) deck (Seq.fromList [n, n-1 .. 1])

main :: IO ()
main = do
    content <- readFile "test/data/day-22-input.txt"
    print $ playGame (parseFile content)