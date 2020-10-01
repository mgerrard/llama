\begin{code}
module Main where

import Lib
import System.Environment

main :: IO ()
main = do

  -- Setup
  a <- getArgs  
  if (length a) < 2
    then error "Usage: llama <foo.c> <function-name> [stub.c] [-debug]"
    else return ()
  {- Indices 0 and 1 exist following the above check -}
  let fileName = a !! 0
      funcName = a !! 1
  mStubAst <- getMaybeStubAst a
  debugFlag <- getDebugArg a

  fileName' <- replaceAssert fileName funcName
  p' <- runPreprocessor fileName' [] -- no preprocessor args for now

  -- Main logic  
  ast <- parseFile p'
  ast' <- prune funcName ast mStubAst debugFlag
  ast'' <- modularize funcName ast' debugFlag

  -- Display
  printAST ast''
  return ()

\end{code}
