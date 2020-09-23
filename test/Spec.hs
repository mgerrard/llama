import Lib

main :: IO ()
main = do
  check "test1.c" "oracle1.c"
  check "declInitTest.c" "declInitOracle.c"
  check "structTest.c" "structOracle.c"
  return ()

check :: String -> String -> IO ()
check testFile oracleFile = do
  let basePrefix = "test/"
  testAst <- parseFile (basePrefix++testFile)
  prunedAst <- prune "main" testAst
  
  oracleAst <- parseFile (basePrefix++oracleFile)
  
  if sameAst prunedAst oracleAst
    then putStrLn $ " success: "++testFile++" and "++oracleFile++" have matching ASTs after pruning."
    else error $ " FAIL: "++testFile++"'s AST does not match "++oracleFile++"'s AST after pruning."
  
sameAst :: AST -> AST -> Bool
sameAst a1 a2 = (astToString a1)==(astToString a2)
