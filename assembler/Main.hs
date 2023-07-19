import Assembler (AssemblerResult (..), assemble)
import Data.ByteString (ByteString, writeFile)
import System.Environment (getArgs)

-- Reads command line args where the first argument is
-- the file path to the input file and the second
-- argument is the path to the output (machine code) file
main :: IO ()
main = do
  args <- getArgs
  let argc = length args
  if argc == 0
    then print "No file path provided"
    else do
      str <- readFile (head args)
      case assemble str of
        Error str -> print str
        Result byteStr -> let out = args !! 1 in Data.ByteString.writeFile out byteStr