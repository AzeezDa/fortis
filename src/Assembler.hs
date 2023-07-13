module Assembler where

import Data.Binary (Word16, Word8)
import Data.Bits (shiftL, (.|.), shiftR, (.&.))
import Data.Map (Map, empty, insert, lookup, member, elems)
import Lexer (Token (..))
import Parser (ParsingResult (..), Statement (..), parse)
import Data.ByteString (ByteString, pack)
import Data.List (sort)

data Assembler = Assembler
  { program :: [Word8],
    labelMap :: Map String (Word8, Word16),
    programCounter :: Word16,
    labelCount :: Word8,
    currentInstruction :: Word8
  }
  deriving (Show)

data AssemblerResult a = Result a | Error String

initAsm :: Assembler
initAsm =
  Assembler
    { program = [],
      labelMap = empty,
      programCounter = 0,
      labelCount = 0,
      currentInstruction = 0
    }

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
analyseLabels a (Nullary _ : ss) =
  let currPC = programCounter a
   in let newAsm = a {programCounter = currPC + 1}
       in analyseLabels newAsm ss
analyseLabels a (_ : ss) =
  let currPC = programCounter a
   in let newAsm = a {programCounter = currPC + 2}
       in analyseLabels newAsm ss

newInstruction :: Word8 -> Assembler -> Assembler
newInstruction i a =
  let pc = programCounter a
   in let evenPC = even pc
       in if evenPC
            then
              let newInst = i `shiftL` 4
               in a {programCounter = pc + 1, currentInstruction = newInst}
            else
              let currInst = currentInstruction a
               in let newInst = currInst .|. i
                   in let prog = program a
                       in a
                            { program = newInst : prog,
                              currentInstruction = 0,
                              programCounter = pc + 1
                            }

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
  _ -> error "Should not happen"

stmts2asm :: Assembler -> [Statement] -> AssemblerResult Assembler
stmts2asm a [] =
  let currInst = currentInstruction a in
  let prog = currInst:program a in
  Assembler.Result a {program = reverse prog}
stmts2asm a ((Label _) : ss) = stmts2asm a ss
stmts2asm a ((Nullary t) : ss) =
  let newAsm = newInstruction (inst2val t) a
   in stmts2asm newAsm ss
stmts2asm a ((NextValue t n) : ss) =
  let asmWithInst = newInstruction (inst2val t) a
   in let newAsm = newInstruction (fromInteger n) asmWithInst
       in stmts2asm newAsm ss
stmts2asm a ((JumpBranch t jmp) : ss) =
  let targetPC = Data.Map.lookup jmp (labelMap a)
   in case targetPC of
        Nothing -> Assembler.Error $ "Label " ++ jmp ++ " does not exist."
        Just (n, _) ->
          let asmWithInst = newInstruction (inst2val t) a
           in let newAsm = newInstruction n asmWithInst
               in stmts2asm newAsm ss

assembleStatements :: [Statement] -> AssemblerResult Assembler
assembleStatements ss =
  let newAsm = initAsm
   in let labeledAsm = analyseLabels newAsm ss
       in case labeledAsm of
            Assembler.Error s -> labeledAsm
            Assembler.Result a -> stmts2asm a ss

assemble :: String -> AssemblerResult ByteString
assemble s = case parse s of
    Parser.Error n -> Assembler.Error $ "Parse error at line" ++ show n
    Parser.Result (ss, _, _) -> case assembleStatements ss of
        Assembler.Error s -> Assembler.Error s
        Assembler.Result a ->
            let labelAddresses = map snd (sort $ elems $ labelMap a) in
            let lcount = fromIntegral $ labelCount a in 
            if lcount > 16 then Assembler.Error "Too many labels"
            else let romList = labelAddresses ++ replicate (16-lcount) 0 in
                 let romBytes = foldr (\e l -> let lo = fromIntegral $ e .&. 0xff in
                                               let hi = fromIntegral $ e `shiftR` 8 in
                                               hi:lo:l 
                          ) [] romList in
                 Assembler.Result $ pack $ romBytes ++ program a
