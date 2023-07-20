-- Inspired by Graham Hutton's "Functional Parsing": https://youtu.be/dDtZLm7HIJs
module Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Lexer (Token (..), lexIt)

-- Result is (a, Remaining String, Current Line Number)
data ParsingResult a = Error Int | Result (a, String, Int) deriving (Show)

-- See the Assemble Language Grammar in README.md
data Statement
  = Nullary Token
  | NextValue Token Integer
  | JumpBranch Token String
  | Label String
  deriving (Show)

-- Parser function for the Monadic Parser
newtype Parser a = Parser (String -> Int -> ParsingResult a)

-- Takes a Parser function and applies it on a given string and line number to give a ParsingResult
applyParser :: Parser a -> String -> Int -> ParsingResult a
applyParser (Parser p) = p

{- =====================================================
Make Parser into a Monad by:
1. Making it a Functor
2. Making it an Applicative
3. Making it a Monad (Since it requires the above 2)
4. Making it also an Alternative to allow for "Either"

Each definition basically is applying the parser and
then if an error results then we return it with the
line number where the error occurred. If no errors
occurred then we apply the next parser on the result
===================================================== -}
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =
    Parser
      ( \s n -> case applyParser p s n of
          Error k -> Error k
          Result (r, s', k) -> Result (f r, s', k)
      )

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s n -> Result (x, s, n))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q =
    Parser
      ( \s n -> case applyParser p s n of
          Error k -> Error k
          Result (r, s', k) -> applyParser (fmap r q) s' k
      )

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      ( \s n -> case applyParser p s n of
          Error k -> Error k
          Result (r, s', k) -> applyParser (f r) s' k
      )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ n -> Error n)
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    Parser
      ( \s n -> case applyParser p s n of
          Error _ -> applyParser q s n
          Result (r, s', k) -> Result (r, s', k)
      )

-- ========= END OF MONAD DEFINITIONS =========

-- Consumes the next token
token :: Parser Token
token =
  Parser
    ( \s n -> case lexIt s of
        (Illegal, _, k) -> Error (n + k)
        (t, s', k) -> Result (t, s', n + k)
    )

-- Peeks at the next token without consuming it
peek :: Parser Token
peek =
  Parser
    ( \s n -> case lexIt s of
        (Illegal, _, k) -> Error (n + k)
        (t, _, _) -> Result (t, s, n)
    )

-- Consumes (Expects) a Nullary Instruction
nullaryInst :: Parser Statement
nullaryInst = do
  t <- token
  if t `elem` [COP, ADD, SUB, MUL, DIV, PUS, POP, RIN, OUT, HLT]
    then return $ Nullary t
    else empty

-- Consumes (Expects) a Hex or Num
value :: Parser Integer
value = do
  t <- token
  case t of
    Hex n -> return n
    Num n -> return n
    _ -> empty

-- Consumes (Expects) a Next-Value Instruction
nextValueInst :: Parser Statement
nextValueInst = do
  t <- token
  v <- value
  if t `elem` [SXV, AXV, SON]
    then return $ NextValue t v
    else empty

-- Consumes (Expects) an Identifier
identifier :: Parser (Token, String)
identifier = do
  t <- token
  case t of
    Ident s -> return (t, s)
    _ -> empty

-- Consumes (Expects) a Label (Identifier followed by a Colon)
label :: Parser Statement
label = do
  (_, s) <- identifier
  col <- token
  case col of
    Colon -> return $ Label s
    _ -> empty

-- Consumes (Expects) a Jump-Branch Instruction
jumpBranchInst :: Parser Statement
jumpBranchInst = do
  t <- token
  (_, str) <- identifier
  if t `elem` [JRX, BRZ, BRN, BRP]
    then return $ JumpBranch t str
    else empty

-- Consumes (Expects) a Statement
-- Notice how <|> acts as an "or" in the grammar
statement :: Parser Statement
statement = do
  label <|> nullaryInst <|> nextValueInst <|> jumpBranchInst

-- Consumes (Expects) an entire program
-- which is a list of statements
program :: Parser [Statement]
program = do
  p <- peek
  case p of
    EOF -> return []
    _ -> do
      s <- statement
      prog <- program
      return $ s : prog

-- The Parse Function: Takes a string a tries to return
-- a list of statements
parse :: String -> ParsingResult [Statement]
parse s = applyParser program s 0