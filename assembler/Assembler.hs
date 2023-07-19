module Assembler where

import Data.Binary (Word16, Word8)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString, pack)
import Data.List (intersperse, sort)
import Data.Map (Map, elems, empty, insert, lookup, member)
import Lexer (Token (..))
import Numeric (showHex)
import Parser (ParsingResult (..), Statement (..), parse)

-- Holds the state of the currently assembled Assembler (no pun intended)
data Assembler = Assembler
  { program :: [Word8], -- The Instructions as 2 instruction per byte [BBBB|AAAA]
    labelMap :: Map String (Word8, Word16), -- Table from Label to (ROM Index, Address to Jump To)
    programCounter :: Word16, -- Used when analysing labels
    labelCount :: Word8, -- Used for checking that number of labels < 16
    currentInstruction :: Word8
  }
  deriving (Show)

data AssemblerResult a = Result a | Error String

-- Initial Assembler
initAsm :: Assembler
initAsm =
  Assembler
    { program = [],
      labelMap = empty,
      programCounter = 0,
      labelCount = 0,
      currentInstruction = 0
    }

-- Does one pass through the statements adding labels to the label map
-- together with their indices and where they are located in terms of 
-- program position
analyseLabels :: Assembler -> [Statement] -> AssemblerResult Assembler
analyseLabels a [] = Assembler.Result a {programCounter = 0}
analyseLabels a (Label str : ss) =
  let currMap = labelMap a
   in let alreadyExists = member str currMap
       in if alreadyExists
            then Assembler.Error ("Label " ++ str ++ " already exists.")
            else
              let currPC = programCounter a
               in let currLabelCnt = labelCount a
                   in let newAsm =
                            a
                              { labelMap = insert str (currLabelCnt, currPC) currMap,
                                labelCount = currLabelCnt + 1
                              }
                       in analyseLabels newAsm ss
analyseLabels a (Nullary t : ss) =
  let currPC = programCounter a
   in let toAdd = if t == HLT then 3 else 1 -- Halt is 0x0 0x0 0x0
       in let newAsm = a {programCounter = currPC + toAdd}
           in analyseLabels newAsm ss
-- Other than nullary and labels are Jump-Branch and Next-Value
-- Both need two instructions hence the +2
analyseLabels a (_ : ss) =
  let currPC = programCounter a
   in let newAsm = a {programCounter = currPC + 2}
       in analyseLabels newAsm ss

-- Adds a new instruction (as bytes) to the assembler by packing
-- them into a u8. See Machine Code in README.md for more 
-- information
newInstruction :: Word8 -> Assembler -> Assembler
newInstruction i a =
  let pc = programCounter a
   in let evenPC = even pc
       in if evenPC
            then
              let newInst = i
               in a {programCounter = pc + 1, currentInstruction = newInst}
            else
              let currInst = currentInstruction a
               in let newInst = currInst .|. (i `shiftL` 4)
                   in let prog = program a
                       in a
                            { program = newInst : prog,
                              currentInstruction = 0,
                              programCounter = pc + 1
                            }

-- Instruction to Value (as a nibble)
inst2val :: Token -> Word8
inst2val t = case t of
  SXV -> 0
  AXV -> 1
  SON -> 2
  COP -> 3
  ADD -> 4
  SUB -> 5
  MUL -> 6
  DIV -> 7
  JRX -> 8
  BRZ -> 9
  BRN -> 10
  BRP -> 11
  PUS -> 12
  POP -> 13
  RIN -> 14
  OUT -> 15
  Num a -> fromIntegral a
  Hex a -> fromIntegral a
  _ -> error "Should never happen"

-- Statements to Assembler: Recursively go through the statements
-- and assemble the Assembler
stmts2asm :: Assembler -> [Statement] -> AssemblerResult Assembler
stmts2asm a [] =
  let currInst = currentInstruction a
   in let prog = currInst : program a
       in Assembler.Result a {program = reverse prog}
stmts2asm a ((Label _) : ss) = stmts2asm a ss
stmts2asm a ((Nullary HLT) : ss) =
  let newAsm = iterate (newInstruction 0) a !! 3 -- Halt is 0x0 0x0 0x0
   in stmts2asm newAsm ss
stmts2asm a ((Nullary t) : ss) =
  let newAsm = newInstruction (inst2val t) a
   in stmts2asm newAsm ss
stmts2asm a ((NextValue t n) : ss) =
  let asmWithInst = newInstruction (inst2val t) a
   in let val = fromInteger n .&. 0xf
       in let newAsm = newInstruction val asmWithInst
           in stmts2asm newAsm ss
stmts2asm a ((JumpBranch t jmp) : ss) =
  let targetPC = Data.Map.lookup jmp (labelMap a)
   in case targetPC of
        Nothing -> Assembler.Error $ "Label " ++ jmp ++ " does not exist."
        Just (n, _) ->
          let asmWithInst = newInstruction (inst2val t) a
           in let newAsm = newInstruction n asmWithInst
               in stmts2asm newAsm ss

-- Takes a list of statements and try to return an assembler

-- holding the ROM and the Program
assembleStatements :: [Statement] -> AssemblerResult Assembler
assembleStatements ss =
  let newAsm = initAsm
   in let labeledAsm = analyseLabels newAsm ss
       in case labeledAsm of
            Assembler.Error s -> labeledAsm
            Assembler.Result a -> stmts2asm a ss

-- Convert the ROM Address from u16 into 12-bit address by 
-- packing then into a byte and a half resulting a total 
-- of 24 bytes for the ROM
romList2Bytes :: [Word16] -> [Word8]
romList2Bytes [] = []
romList2Bytes (l1 : l2 : r) =
  let b1 :: Word8 = fromIntegral $ l1 .&. 0xff
   in let b2 :: Word8 = fromIntegral $ (l1 `shiftR` 8) .|. ((l2 .&. 0xf) `shiftL` 4)
       in let b3 :: Word8 = fromIntegral $ (l2 `shiftR` 4) .&. 0xff
           in b1 : b2 : b3 : romList2Bytes r

-- The Assembler Function: Takes a String written in the
-- Fortis Assembly Language and tries to return the
-- corresponding machine code (with ROM). This function
-- basically Lexes, Parses and Assembles the input string
assemble :: String -> AssemblerResult ByteString
assemble s = case parse s of
  Parser.Error n -> Assembler.Error $ "Parse error at line" ++ show n
  Parser.Result (ss, _, _) -> case assembleStatements ss of
    Assembler.Error s -> Assembler.Error s
    Assembler.Result a ->
      let labelAddresses = map snd (sort $ elems $ labelMap a)
       in let lcount = fromIntegral $ labelCount a
           in if lcount > 16
                then Assembler.Error "Too many labels"
                else
                  let romList = labelAddresses ++ replicate (16 - lcount) 0
                   in Assembler.Result $ pack $ romList2Bytes romList ++ program a
