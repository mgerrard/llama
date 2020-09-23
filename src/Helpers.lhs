The following two functions are somewhat
inverses of each other.
The first---{\tt parseFile}---transforms
a C file into an AST (called a
{\tt CTranslUnit} within {\tt language-c}),
and the second---{\tt printAST}---outputs the
AST in pretty-printed form to {\tt stdout}.
These functions are taken almost verbatim from
{\url github.com/visq/language-c}, within
{\tt examples/BasicUsage.hs}.

\begin{code}

module Helpers where

import System.Process
import System.Directory
import Language.C
import Language.C.System.GCC

type AST = CTranslUnit

parseFile :: FilePath -> IO AST
parseFile input_file = do
  parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
  case parse_result of
    Left parse_err -> error (show parse_err)
    Right ast      -> return ast

astToString :: AST -> String
astToString ast = show $ pretty ast

printAST :: AST -> IO ()
printAST ast = (print . pretty) ast

removeStubbedLibraries :: [String] -> FilePath -> IO FilePath
removeStubbedLibraries libs p = do
  let libExprs = (concat $ map (\l->"/"++l++"/d;") libs)
  (_,out,_) <- readProcessWithExitCode "/bin/sed" [libExprs, p] ""
  let p' = p++".tmp"
  writeFile p' out
  return p'

runPreprocessor :: FilePath -> String -> IO FilePath
runPreprocessor p flags = do
  let libs = ["assert.h"]
  p' <- removeStubbedLibraries libs p
  let p'' = p ++ ".cpp.c"
  if null flags
    then do
      _ <- readProcessWithExitCode "cpp" [p', p''] ""
      return p''
    else do
      _ <- readProcessWithExitCode "cpp" [flags, p', p''] ""
      return p''

replaceAssert :: FilePath -> String -> IO FilePath
replaceAssert fileName funcName = do
  let fileName' = fileName++"."++funcName++".c"
  copyFile fileName fileName'
  let sedArgs = ["-E","-i","s/assert\\((.*);/if\\(\\1{;}else{__VERIFIER_error\\(\\);}/",fileName']
  _ <- readProcess "sed" sedArgs []
  return fileName'

getDebugArg :: [String] -> IO Bool
getDebugArg [_,_,d] = return (d == "-debug")
getDebugArg [_,_,_,d] =
  if (d == "-debug")
    then return True
    else error $ "oops, i don't recognize the argument: "++d
getDebugArg _ = return False

getMaybeStubAst :: [String] -> IO (Maybe AST)
getMaybeStubAst [_,_,s] = do
  if s == "-debug"
    then return Nothing
    else do
      sAst <- parseFile s
      return (Just sAst)
getMaybeStubAst [_,_,s,_] = do
  sAst <- parseFile s
  return (Just sAst)
getMaybeStubAst _ = return Nothing

\end{code}
