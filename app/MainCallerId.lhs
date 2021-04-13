\begin{code}
module MainCallerId where

import Lib
import System.Environment
import System.Directory
import Language.C
import Language.C.Data.Ident
import Data.List (isInfixOf)
import Control.Monad

main :: IO ()
main = do

  -- Setup
  a <- getArgs  
  if (length a) < 2
    then error "Usage: caller-id <foo.c> <function-name>"
    else return ()
  {- Indices 0 and 1 exist following the above check -}
  let fileName = a !! 0
      fName = a !! 1

  -- Main logic  
  p' <- runPreprocessor fileName [] -- no preprocessor args for now
  ast <- parseFile p'
  let fs = collectFunctions ast
      fs' = map (\(CFDefExt f)->f) fs
  callers <- filterM (calls fName) fs'
  let callerIds = map getFuncId callers      

  -- Print caller ids
  mapM_ (\caller->putStrLn $ fileName++" "++caller) callerIds
  removeTmpIfExists fileName
  removeCppIfExists fileName

  return ()

removeTmpIfExists :: FilePath -> IO ()
removeTmpIfExists f = do
  let tmpFile = f++".tmp"
  fExists <- doesFileExist tmpFile
  if fExists
    then removeFile tmpFile
    else return ()

removeCppIfExists :: FilePath -> IO ()
removeCppIfExists f = do
  let cppFile = f++".cpp.c"
  fExists <- doesFileExist cppFile
  if fExists
    then removeFile cppFile
    else return ()

declrName :: CDeclr -> String
declrName declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in n

getFuncId :: CFunDef -> String
getFuncId (CFunDef _ d _ _ _) = declrName d

calls :: String -> CFunDef -> IO Bool
calls s (CFunDef _ _ _ stmt _) = do
  let fBody = show $ pretty stmt
--  putStrLn fBody
  let funcCall = s++"("
  return $ funcCall `isInfixOf` fBody

\end{code}
