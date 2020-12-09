{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Map as Map 
import Data.Map ( Map )
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M


data OpCode = Nop | Jmp | Acc
    deriving (Show, Eq)

data Instruction =
    Instruction { opCode :: OpCode
                , arg :: Int
                , visited :: Bool
                }
                deriving Show

defInstruction :: Instruction
defInstruction = Instruction Nop 0 False

mkInstruction :: OpCode -> Int -> Instruction
mkInstruction op a = defInstruction { opCode = op, arg = a}

data Computer =
    Computer { pc :: Int
             , acc :: Int 
             , memory :: Map Int Instruction
             }
        deriving Show

type MParser = Parsec Void Text

parseInstructions :: MParser [Instruction]
parseInstructions = M.many instructionParser

instructionParser :: MParser Instruction
instructionParser = do
    opcode <- opcodeParser
    M.space
    arg <- numberParser
    M.eol <|> (M.eof >> return "")
    return $ mkInstruction opcode arg

opcodeParser :: MParser OpCode
opcodeParser = M.choice
    [ Nop <$ M.string "nop"
    , Jmp <$ M.string "jmp"
    , Acc <$ M.string "acc"
    ]

numberParser :: MParser Int
numberParser = read <$>
    (negativeParser <|> positiveParser)
    where
        integerParser :: MParser String
        integerParser = M.try (M.some M.digitChar)

        negativeParser :: MParser String
        negativeParser = M.try $ do
            M.char '-'
            num <- integerParser
            return $ '-'  : num
            
        positiveParser :: MParser String
        positiveParser= M.try $ do
            M.char '+'
            integerParser
 
mkComputer :: Text -> Computer
mkComputer input =
    Computer 0 0 (Map.fromList $ zip (cycle [0..]) instrs)
    where
        instrs = case M.runParser parseInstructions "" input of
            Left s -> error (show s)
            Right instrs -> instrs


changeInstr :: Computer -> Int -> Computer
changeInstr comp index = 
    case Map.lookup index (memory comp) of
        Nothing -> error ("Illegal memory address: " ++ show index)
        (Just instr) -> case opCode instr of
            Jmp -> comp { memory = Map.insert index (instr { opCode = Nop }) (memory comp)  }
            Nop -> comp { memory = Map.insert index (instr { opCode = Jmp}) (memory comp)  }
            _   -> comp


step :: Computer -> (String, Computer)
step comp =
    case Map.lookup (pc comp) (memory comp) of
        Nothing -> 
            if pc comp == length (memory comp)
                then ("terminates", comp)
                else error ("Illegal memory address: " ++ show (pc comp))
        (Just instr) -> if visited instr 
                            then ("loops", comp)
                            else step (updateReg (updateMem comp instr) instr)
    where
        updateReg c i = case opCode i of
            Nop -> c { pc = pc c + 1 }
            Jmp -> c { pc = pc c + arg i }
            Acc -> c { pc = pc c + 1, acc = acc c + arg i }
        updateMem c i  = c { memory = Map.insert (pc c) i { visited = True } (memory comp) }
            

part1 :: Text -> (String, Computer)
part1 input = step (mkComputer input)

part2 :: Text -> (String, Computer)
part2 input =
    go (Map.keys (memory comp))
    where
        comp = mkComputer input
        go [] = ("doesn't terminate", comp)
        go (i:is) =
            case step (changeInstr comp i) of
                ("loops", _) -> go is
                ("terminates", c) -> ("terminates", c)

main :: IO ()
main = do
    content <- readFile "./test/data/Day-8-input.txt"
    let (status, computer) = part1 (T.pack content)
    print $ "Program " ++ status ++ " with acc = " ++ show (acc computer)
    let (status, computer) = part2 (T.pack content)
    print $ "Program " ++ status ++ " with acc = " ++ show (acc computer)

