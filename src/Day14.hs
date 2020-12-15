module Day14 where

import Data.Char
import Data.List
import Numeric ()
import Data.Bits ( Bits(setBit, clearBit) )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Addr = Int
type Value = Int

data BitOp = ClearBit Int| SetBit Int
    deriving (Show)

data Instr = Move Addr Value | Mask [BitOp]
    deriving (Show)

type Memory = Map Addr Value

type MParser = Parsec Void String

-- Parser

parseInstrs:: MParser [Instr]
parseInstrs = M.many instrParser

instrParser :: MParser Instr
instrParser = 
    maskParser <|>
    moveParser

maskParser :: MParser Instr
maskParser = do
    M.string "mask = "
    maskStr <- M.try (M.some M.alphaNumChar)
    M.eol <|> (M.eof >> return "")
    return $ makeMask maskStr

moveParser :: MParser Instr
moveParser = do
    M.string "mem["
    addr <- M.try (M.some M.digitChar)
    M.string "] = "
    value <- M.try (M.some M.digitChar)
    let addr' = read addr
    let value' = read value
    M.eol <|> (M.eof >> return "")
    return $ Move addr' value'

makeMask :: String -> Instr
makeMask str = Mask ops
    where
        rev = reverse str
        ops = (SetBit <$> elemIndices '1' rev) ++ (ClearBit <$> elemIndices '0' rev)

-- Execute instructions

-- Apply mask to address
applyMask :: [BitOp] -> Value -> Value
applyMask ops = foldr ((.) . bitop) id ops
    where
        bitop :: BitOp -> Value -> Value
        bitop (ClearBit bit) val = clearBit val bit
        bitop (SetBit bit) val = setBit val bit

-- Execute instruction in the context of the current mask and memory
-- mask will change the mask and not affect memory
-- move will update memory after applying mask to the value that is being stored
execInstr :: Instr -> ([BitOp], Memory) -> ([BitOp], Memory)
execInstr (Mask ops) (_, mem) = (ops, mem)
execInstr (Move addr val) (ops, mem) = (ops, Map.insert addr (applyMask ops val) mem)

execInstrs :: [Instr] -> Memory
execInstrs instrs =
    go instrs ([], Map.empty)
    where 
        go [] (_, mem) = mem
        go (i:is) (m, mem) = go is (execInstr i (m, mem))

part1 :: String -> Int
part1 input = sum $ Map.elems mem
    where
        mem= case M.runParser parseInstrs "" input of
            Left s -> error (show s)
            Right instrs -> execInstrs instrs

main :: IO ()
main = do
    content <- readFile "./test/data/day-14-input.txt"
    print $ part1 content 
