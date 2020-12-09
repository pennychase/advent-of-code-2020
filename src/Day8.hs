{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Map as Map 
import Data.Map ( Map )
import Data.Void
import Data.Text(Text)
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
 
mkComputer :: [Instruction] -> Computer
mkComputer instrs =
    Computer 0 0 (Map.fromList $ zip (cycle [0..]) instrs)

step :: Computer -> Computer
step comp =
    case Map.lookup (pc comp) (memory comp) of
        Nothing -> 
            if pc comp == length (memory comp)
                then comp
                else error $ "Illegal memory address: " ++ show (pc comp)
        (Just instr) -> if visited instr then comp else step (updateReg (updateMem comp instr) instr)
    where
        updateReg c i = case opCode i of
            Nop -> c { pc = pc c + 1 }
            Jmp -> c { pc = pc c + arg i }
            Acc -> c { pc = pc c + 1, acc = acc c + arg i }
        updateMem c i  = c { memory = Map.insert (pc c) i { visited = True } (memory comp) }
            

part1 :: Text -> Int
part1 input = acc $ step comp
    where
        instrs = case M.runParser parseInstructions "" input of
            Left s -> error (show s)
            Right instrs -> instrs
        comp = mkComputer instrs

main :: IO ()
main = do
    content <- readFile "./test/data/Day-8-input.txt"
    print $ part1 (T.pack content)
