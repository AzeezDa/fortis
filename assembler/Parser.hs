module Parser where

import Lexer
import Control.Applicative


data ParsingResult a = Error Int | Result (a, String, Int) deriving (Show)

data Statement = Nullary Token
               | NextValue Token Integer 
               | JumpBranch Token String
               | Label String
               deriving (Show)


newtype Parser a = Parser (String -> Int -> ParsingResult a)

applyParser :: Parser a -> String -> Int -> ParsingResult a
applyParser (Parser p) = p

instance Functor Parser where
    fmap f p = Parser (\s n -> case applyParser p s n of
                        Error k -> Error k
                        Result (r, s', k) -> Result (f r, s', k)
                      )

instance Applicative Parser where
    pure x = Parser (\s n -> Result (x, s, n))
    p <*> q = Parser (\s n -> case applyParser p s n of 
                        Error k -> Error k
                        Result (r, s', k) -> applyParser (fmap r q) s' k
                     )

instance Monad Parser where
    p >>= f = Parser (\s n -> case applyParser p s n of
                        Error k -> Error k
                        Result (r, s', k) -> applyParser (f r) s' k
                     )

instance Alternative Parser where
    empty = Parser (\_ n -> Error n)
    p <|> q = Parser (\s n -> case applyParser p s n of
                        Error _           -> applyParser q s n
                        Result (r, s', k) -> Result (r, s', k)
                     )

token :: Parser Token
token = Parser (\s n -> case lexIt s of
                (Illegal, _, k) -> Error (n + k)
                (t, s', k)      -> Result(t, s', n + k)
               )

peek :: Parser Token
peek = Parser (\s n -> case lexIt s of
                 (Illegal, _, k) -> Error (n + k)
                 (t, _, _)       -> Result(t, s, n)
               )

nullaryInst :: Parser Statement
nullaryInst = do
    t <- token
    if t `elem` [COP, ADD, SUB, MUL, DIV, PUS, POP, RIN, OUT, HLT]
        then return $ Nullary t
    else empty

value :: Parser Integer 
value = do
    t <- token
    case t of
        Hex n -> return n
        Num n -> return n
        _     -> empty

nextValueInst :: Parser Statement
nextValueInst = do
    t <- token
    v <- value
    if t `elem` [SXV, AXV, SON] 
        then return $ NextValue t v  
    else empty

identifier :: Parser (Token, String)
identifier = do
    t <- token
    case t of
        Ident s -> return (t, s)
        _       -> empty

label :: Parser Statement
label = do
    (_, s) <- identifier
    col <- token
    case col of
        Colon -> return $ Label s 
        _     -> empty

jumpBranchInst :: Parser Statement
jumpBranchInst = do
    t <- token
    (_, str) <- identifier
    if t `elem` [JRX, BRZ, BRN, BRP]
        then return $ JumpBranch t str 
    else
        empty

statement :: Parser Statement
statement = do
    label <|> nullaryInst <|> nextValueInst <|> jumpBranchInst 

program :: Parser [Statement]
program = do
    p <- peek
    case p of
        EOF -> return []
        _   -> do
                s <- statement
                prog <- program
                return $ s:prog

parse :: String -> ParsingResult [Statement]
parse s = applyParser program s 0