module Day14 where

import Data.Char ()
import Data.List ( elemIndices )
import Numeric ()
import Data.Bits ( Bits(setBit, clearBit) )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Void ( Void )
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Addr = Int
type Value = Int

data BitOp = ClearBit Int| SetBit Int
    deriving (Show)

data Instr = Move Addr Value | VMask [BitOp] | AMask [[BitOp]]
    deriving (Show)

type Memory = Map Addr Value

type MParser = Parsec Void String

-- Parser

parseInstrs1 :: MParser [Instr]
parseInstrs1 = M.many instrParser1

instrParser1 :: MParser Instr
instrParser1 = 
    valMaskParser <|>
    moveParser

parseInstrs2 :: MParser [Instr]
parseInstrs2 = M.many instrParser2

instrParser2 :: MParser Instr
instrParser2 = 
    addrMaskParser <|>
    moveParser

valMaskParser :: MParser Instr
valMaskParser = do
    M.string "mask = "
    maskStr <- M.try (M.some M.alphaNumChar)
    M.eol <|> (M.eof >> return "")
    return $ makeMask maskStr

addrMaskParser :: MParser Instr
addrMaskParser = do
    M.string "mask = "
    maskStr <- M.try (M.some M.alphaNumChar)
    M.eol <|> (M.eof >> return "")
    return $ makeAddrMask maskStr

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

-- Common

-- Apply mask to value or address
applyMask :: [BitOp] -> Int -> Int 
applyMask ops = foldr ((.) . bitop) id ops
    where
        bitop :: BitOp -> Value -> Value
        bitop (ClearBit bit) val = clearBit val bit
        bitop (SetBit bit) val = setBit val bit

-- Part 1

makeMask :: String -> Instr
makeMask str = VMask ops
    where
        rev = reverse str
        ops = (SetBit <$> elemIndices '1' rev) ++ (ClearBit <$> elemIndices '0' rev)

-- Execute instruction in the context of the current mask and memory
-- mask will change the value and not affect memory
-- move will update memory after applying mask to the value that is being stored
execInstr :: Instr -> ([BitOp], Memory) -> ([BitOp], Memory)
execInstr (VMask ops) (_, mem) = (ops, mem)
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
        mem= case M.runParser parseInstrs1 "" input of
            Left s -> error (show s)
            Right instrs -> execInstrs instrs

-- Part 2

-- Create a set of masks that will be used to geneate multiple addresses when applied to an address
-- If the mask has a 1, that bit is turned on. If the mask has an X that bit  needs to be cleared or
-- set. Use List Monad to generate all combinations of SetBit and ClearBit fot the given bits.
makeAddrMask :: String -> Instr
makeAddrMask str = AMask ops
    where
        rev = reverse str
        ops = map (\x -> (SetBit <$> elemIndices '1' rev) ++ x) $ mapM (\b -> [SetBit b, ClearBit b]) (elemIndices 'X' rev)

-- Execute instruction in the context of the current mask and memory
-- Use the list of masks to generate all the addressed that will be used in the Move operation
execInstr2 :: Instr -> ([[BitOp]], Memory) -> ([[BitOp]], Memory)
execInstr2 (AMask ops) (_, mem) = (ops, mem)
execInstr2 (Move addr val) (ops, mem) = (ops, decodeAddr ops mem)
    where
        decodeAddr [] m = m
        decodeAddr (o:os) m = decodeAddr os (Map.insert (applyMask o addr) val m)

execInstrs2:: [Instr] -> Memory
execInstrs2 instrs =
    go instrs ([], Map.empty)
    where 
        go [] (_, mem) = mem
        go (i:is) (m, mem) = go is (execInstr2 i (m, mem))

part2 :: String -> Int
part2 input = sum $ Map.elems mem
    where
        mem = case M.runParser parseInstrs2 "" input of
            Left s -> error (show s)
            Right instrs -> execInstrs2 instrs

main :: IO ()
main = do
    content <- readFile "./test/data/day-14-input.txt"
    print $ part1 content 
    print $ part2 content 
