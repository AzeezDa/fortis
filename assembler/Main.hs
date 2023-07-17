import Assembler (AssemblerResult (..), assemble)
import Data.ByteString (ByteString, writeFile)
import System.Environment (getArgs)
main = do
  args <- getArgs
  let l = length args
  if l == 0 then
      print "No file path provided"
    else do
        str <- readFile (head args)
        case assemble str of
            Error s -> print s
            Result bs -> do
                let out = if l == 1 then "out" else args !! 1
                Data.ByteString.writeFile out bs
                
