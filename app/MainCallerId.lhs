\begin{code}
module MainCallerId where

import Lib
import System.Environment
import Language.C
import Language.C.Data.Ident
import Data.List (isInfixOf)

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
  ast <- parseFile fileName
  let fs = collectFunctions ast
      fs' = map (\(CFDefExt f)->f) fs
      callers = filter (calls fName) fs'
      callerIds = map getFuncId callers      

  -- Print caller ids
  mapM_ (\caller->putStrLn $ fileName++" "++caller) callerIds

  return ()

declrName :: CDeclr -> String
declrName declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in n

getFuncId :: CFunDef -> String
getFuncId (CFunDef _ d _ _ _) = declrName d

calls :: String -> CFunDef -> Bool
calls s (CFunDef _ _ _ stmt _) =
  let fBody = show $ pretty stmt
      funcCall = s++"("
  in funcCall `isInfixOf` fBody

\end{code}
