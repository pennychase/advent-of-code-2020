module Day18 where

import Data.Void (Void)
import Control.Monad (void)
import Control.Applicative (empty)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr = Const Integer | BinExpr Op Expr Expr
    deriving (Show)

data Op = Add | Mul
    deriving (Show)

type Parser = Parsec Void String

-- Lexemes

-- Consume whitespace after lexeme
-- L.space takes three args: parsers for whitespace, line comment, block comment
-- Since we don't have comments, use empty
sc :: Parser ()
sc = L.space (void spaceChar) empty empty

-- Lexeme wrapper wraps lexemes with our space consumer
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol lexeme (a string)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parens parsers
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Integer parser
integer :: Parser Integer
integer = lexeme L.decimal

-- Parsers for Part 1

-- Parse Expression
-- Use Megaparsec expression parser builder
expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (BinExpr Mul <$ symbol "*")
    , InfixL (BinExpr Add <$ symbol "+") ]
  ]

-- Parse Term
term :: Parser Expr
term = parens expr
    <|> Const <$> integer

-- Parse Exprs
parseExprs :: Parser [Expr]
parseExprs = many expr

-- Parsers for Part 2

-- Parse Expression
-- Use Megaparsec expression parser builder
expr2 :: Parser Expr
expr2 = makeExprParser term2 operators2

operators2 :: [[Operator Parser Expr]]
operators2 =
  [ [ InfixL (BinExpr Add <$ symbol "+") ]
  , [ InfixL (BinExpr Mul <$ symbol "*") ]
  ]

-- Parse Term
term2 :: Parser Expr
term2 = parens expr2
    <|> Const <$> integer

-- Parse Exprs
parseExprs2 :: Parser [Expr]
parseExprs2 = many expr2
--
-- Evaluate Exprs
--

eval :: Expr -> Integer
eval (Const i) = i
eval (BinExpr Add arg1 arg2) = eval arg1 + eval arg2
eval (BinExpr Mul arg1 arg2) = eval arg1 * eval arg2

evalExprs :: [Expr] -> Integer
evalExprs = foldr (\x y -> eval x + y) 0 

-- Part 1

part1 :: String -> Integer
part1 input = evalExprs exprs
    where 
        exprs = case runParser parseExprs "" input of
            Left s -> error (show s)
            Right exprs -> exprs

-- Part 2
part2 :: String -> Integer
part2 input = evalExprs exprs
    where 
        exprs = case runParser parseExprs2 "" input of
            Left s -> error (show s)
            Right exprs -> exprs

main :: IO ()
main = do
    content <- readFile "./test/data/day-18-input.txt"
    print $ part1 content
    print $ part2 content
    





