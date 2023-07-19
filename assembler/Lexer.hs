module Lexer where

import Data.Char
  ( isAlpha,
    isAlphaNum,
    isDigit,
    isHexDigit,
    isSpace,
    toLower,
  )
import Numeric (readHex)
import Text.Read (readMaybe)

-- Allowed tokens in the language (See Assembly Language Grammar in README.md) for more information
data Token
  = SXV
  | AXV
  | SON
  | COP
  | ADD
  | SUB
  | MUL
  | DIV
  | JRX
  | BRZ
  | BRN
  | BRP
  | PUS
  | POP
  | RIN
  | OUT
  | HLT
  | Num Integer
  | Hex Integer
  | Ident String
  | Colon
  | EOF
  | Illegal
  deriving (Show, Eq)

-- (Consumed Token, Remaining String, Number of New Lines)
type LexResult = (Token, String, Int)

-- The Lexer Function: Lexes a token
lexIt :: String -> LexResult
lexIt s = lexIt' s 0
  where
    -- Accumulates number of new lines to return when done
    lexIt' :: String -> Int -> LexResult
    lexIt' "" n = (EOF, "", n)
    lexIt' (':' : t) n = (Colon, t, n)
    lexIt' (' ' : t) n = let s = dropWhile (== ' ') t in lexIt' s n
    lexIt' ('\t' : t) n = let s = dropWhile (== '\t') t in lexIt' s n
    lexIt' ('\n' : t) n = lexIt' t (n + 1)
    lexIt' ('\r' : t) n = lexIt' t n
    lexIt' ('#' : t) n = let s = dropWhile (/= '\n') t in lexIt' s n
    lexIt' s@(c : cs) n
      | isAlpha c || c == '_' = let (eaten, rest) = span isAlphaNum s in (checkToken eaten, rest, n)
      | isDigit c || c == '-' =
          let (eaten, rest) = break isSpace s
           in (getNumber eaten, rest, n)
    lexIt' s n = (Illegal, s, n)

    -- Checks if consumed string is a keyword or an identifier
    checkToken :: String -> Token
    checkToken t =
      let t_lower = map toLower t
       in case t_lower of
            "sxv" -> SXV
            "axv" -> AXV
            "son" -> SON
            "cop" -> COP
            "add" -> ADD
            "sub" -> SUB
            "mul" -> MUL
            "div" -> DIV
            "jrx" -> JRX
            "brz" -> BRZ
            "brn" -> BRN
            "brp" -> BRP
            "pus" -> PUS
            "pop" -> POP
            "rin" -> RIN
            "out" -> OUT
            "hlt" -> HLT
            _ -> Ident t
    -- Try to convert a string to a Hex or Num token
    getNumber :: String -> Token
    getNumber ['0', 'x', t]
      | isHexDigit t = case readHex [t] of
          ((h, _) : _) -> Hex h
          [] -> Illegal
      | otherwise = Illegal
    getNumber t = case readMaybe t :: Maybe Integer of
      Just v -> if -8 <= v && v <= 7 then Num v else Illegal
      Nothing -> Illegal
