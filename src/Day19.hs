module Day19 where

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Void ( Void )
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec ( Parsec, (<|>) )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Rule = Rule { rid :: Int
                 , body :: Body
                 }
    deriving Show

data Seq = Seq [Int]
    deriving Show

data Body = Const [Char] | Subrules Seq | OrSubrules Seq Seq
    deriving Show

instance Eq Rule where
    rule1 == rule2 = rid rule1 == rid rule2

instance Ord Rule where
    compare rule1 rule2 = compare (rid rule1) (rid rule2)

type MParser = Parsec Void String

parseFile :: MParser ([Rule], [String])
parseFile = do
    rules <- M.many parseRule
    M.string "\n"
    strs <- M.many parseString
    return (rules, strs)

parseRule :: MParser Rule
parseRule = do
    ruleId <- parseNumber
    M.string ": "
    rule <- (parseConst <|> parseSubrules)
    return $ Rule ruleId rule

parseConst :: MParser Body
parseConst = do
    M.char '\"'
    c <- M.some M.letterChar
    M.char '\"'
    M.eol <|> (M.eof >> return "")
    return $ Const c

parseSeq:: MParser Seq
parseSeq = do
    rules <- M.many parseNumber
    return $ Seq rules
    
parseSubrules :: MParser Body
parseSubrules = do
    rules <- parseSeq `M.sepBy` M.string "|"
    M.eol <|> (M.eof >> return "")
    if length rules == 1
        then return $ Subrules (head rules)
        else return $ OrSubrules (head rules) (last rules)

parseNumber :: MParser Int
parseNumber = do
    M.many (M.char ' ')
    digits <- M.some M.digitChar
    M.many (M.char ' ')
    return $ read digits

parseString :: MParser String
parseString = do
    str <- M.some M.letterChar
    M.eol <|> (M.eof >> return "")
    return str

rulesToMap :: [Rule] -> HashMap Int Body
rulesToMap rules =
    HMap.fromList $ zip (map rid rules') (map body rules')
    where
        rules' = sort rules

compileRule :: HashMap Int Body -> Int -> [String]
compileRule rules n =
    case HMap.lookup n rules of
        Nothing -> error $ "Undefined rule: " ++ show n
        Just (Const str) -> [str]
        Just (Subrules (Seq subs)) -> map concat (mapM (compileRule rules) subs)
        Just (OrSubrules (Seq subs1) (Seq subs2)) -> 
             map concat (mapM (compileRule rules) subs1) ++ 
             map concat (mapM (compileRule rules) subs2)

matches :: [Rule] -> [String] -> Int -> Int
matches rules strs n =
    length $ filter(`HMap.member` ms) (filter (\s -> length s <= longest) strs)
    where
        rs = compileRule (rulesToMap rules) n
        ms = HMap.fromList $ zip rs (repeat 0)
        longest = maximum $ map length rs
 
part1 :: String -> Int
part1 input =
    case M.runParser parseFile "" input of
        Left s -> error (show s)
        Right (rs, ss) -> matches rs ss 0

main :: IO ()
main = do
    -- content <- readFile "test/data/day-19-input.txt"
    content <- readFile "src/test19.txt"
    print $ part1 content

    