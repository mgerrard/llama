The purpose of this module is to remove external
declarations that are guaranteed to be unused,
starting from some entry function. The reason we
do this is because, after preprocessing files in
the {\tt chrony} codebase, their header files
include a ton of junk that makes the tools in
{\sc alpaca}'s portfolio choke.

To prune the unneeded external declarations,
we first traverse the AST starting from the given
entry function and collect the names of
functions, global variables, structs, and
enums encountered.
We then retain any external declarations whose
name is found in our collection of names.

We will have to place everything inside an
{\tt IO} monad (unfortunately), in order to be
able to emit warning messages, e.g., if we
run into any external assembly string literals or
assembly statements, we let the user know we're
throwing them away---the analysis tools in
ALPACA cannot deal with them in general.
So there will be various ``monadic'' versions
of more common Haskell functions, e.g.,
{\tt mapM}---the monadic {\tt map}, and
{\tt filterM}---the monadic {\tt filter}.

\begin{code}

module Prune where

import Helpers
import Control.Monad (mapM,filterM)
import System.IO (stderr,hPutStrLn)
import Data.List (nub,(\\))
import Data.Maybe (catMaybes)
import Language.C
import Language.C.Data.Ident

prune :: String -> AST -> [Maybe AST] -> Bool -> IO AST
prune entryFunction ast mAst debug = do
  let (CTranslUnit es _) = possiblySmoosh ast mAst
  let es' = filter (not . externOrAttr) es
      ast' = CTranslUnit es' undefNode
  (names, _) <- collectNames ([],[entryFunction]) ast'
  let names' = filter (not . null) names
  es'' <- (matchingExtDecls names' ast')
  let ast'' = CTranslUnit es'' undefNode

  if debug
    then do
      putStrLn "** DEBUG *******************"
      putStrLn "* AST after pruning ******"
      printAST ast''
      putStrLn "** DEBUG *******************\n"
    else return ()

  return ast''

possiblySmoosh :: AST -> [Maybe AST] -> AST
possiblySmoosh a [Nothing] = a
possiblySmoosh (CTranslUnit origAst a) s = do
  let (Just (CTranslUnit stubAst _)) = smooshStubs s
  (CTranslUnit (stubAst++origAst) a)

smooshStubs :: [Maybe AST] -> Maybe AST
smooshStubs [] = Nothing
smooshStubs mAsts = 
  let asts = catMaybes mAsts
      extDecls = concat $ map (\(CTranslUnit e _)->e) asts
    in (Just (CTranslUnit extDecls undefNode))

externOrAttr :: CExtDecl -> Bool
externOrAttr (CDeclExt (CDecl ((CStorageSpec (CExtern _)):_) _ _)) = True
externOrAttr (CDeclExt (CDecl ((CTypeQual (CAttrQual _)):_) _ _)) = True
externOrAttr _ = False

collectNames :: ([String],[String]) -> AST -> IO ([String],[String])
collectNames (seen,[]) _ = return (seen,[]) -- termination condition
collectNames (seen,unseen) ast = do
  es <- matchingExtDecls unseen ast
  refdNames <- mapM processExtDecl es
  -- nub removes duplicates; concat "flattens" a list
  let refdNames' = concat $ nub refdNames
      seen' = seen++unseen
      unseen' = refdNames' \\ seen'
  collectNames (seen',unseen') ast

matchingExtDecls :: [String] -> AST -> IO [CExtDecl]
matchingExtDecls names (CTranslUnit es _) = do
  filterM (extDeclInList names) es

extDeclInList :: [String] -> CExtDecl -> IO Bool
extDeclInList names (CDeclExt (CDecl [CStorageSpec (CTypedef _),CTypeSpec (CEnumType e _)] _ _)) = do
  eNames <- enumNames e
  return $ any (\eName->eName `elem` names) eNames
-- this case is a huge hack
extDeclInList names (CDeclExt (CDecl [CTypeSpec (CEnumType e _)] _ _)) = do
  eNames <- enumNames e
  return $ any (\eName->eName `elem` names) eNames
extDeclInList names d@(CDeclExt (CDecl ((CStorageSpec (CTypedef dName)):_) _ _)) = do
  let dName = show $ pretty d
      dName' = last $ words dName
      dName'' = init dName' -- strip semicolon
  return $ dName'' `elem` names
extDeclInList names d = do
  dName <- extDeclName d
  if (not $ null dName) && (dName `elem` names)
    then return True
    else return False

extDeclName :: CExtDecl -> IO String
extDeclName (CFDefExt fDef) = funDefName fDef
extDeclName (CDeclExt decl) = do
  declName <- processDecl decl
  if length declName == 0
    then return ""
    else return $ declName!!0
extDeclName a@(CAsmExt _ _) = do
  let aStr = show $ pretty a
  hPutStrLn stderr $ "ALPACA cannot handle assembly; throwing away: "++aStr
  return ""

processExtDecl :: CExtDecl -> IO [String]
processExtDecl (CFDefExt fDef) = processFunDef fDef
processExtDecl (CDeclExt decl) = processDecl decl
processExtDecl a@(CAsmExt _ _) = do
  let aStr = show $ pretty a
  hPutStrLn stderr $ "ALPACA cannot handle assembly; throwing away: "++aStr
  return [] 
--processExtDecl _ = error "Need to implement other cases of 'processExtDecl'"

processFunDef :: CFunDef -> IO [String]
processFunDef fDef@(CFunDef declSpecs declr decls body _) = do
  n <- funDefName fDef
  pNames <- processParams fDef
  bNames <- processStmt body
  return $ [n]++pNames++bNames

processParams :: CFunDef -> IO [String]
processParams f = do
  let decls = grabParams' f
  declNames <- mapM processDecl decls
  let declNames' = concat declNames
  return declNames'

grabParams' :: CFunDef -> [CDecl]
grabParams' f =
  let (CFunDef _ (CDeclr _ paramList _ _ _) _ _ _) = f
      ps = head paramList
      (CFunDeclr (Right (params, _)) _ _) = ps
  in params

liftSelectList :: [(Maybe (CDecl), CExpr)] -> IO [String]
liftSelectList [] = return []
liftSelectList ((Nothing, expr):xs) = do
  exprNames <- processExpr expr
  listNames <- liftSelectList xs
  return $ exprNames++listNames
liftSelectList (((Just decl), expr):xs) = do
  declNames <- processDecl decl
  exprNames <- processExpr expr
  listNames <- liftSelectList xs
  return $ declNames++exprNames++listNames

\end{code}

C can have many kinds of statements---some of which may contain
names that we care about (like those containing nested statements,
or expressions), so we explore these; others definitely do not
contain names, such as a break statement, in which case we just
return an empty array.

\begin{code}

processStmt :: CStat -> IO [String]
-- we don't care about a label's name (or attributes),
-- but we do want to jump into its statement
processStmt (CLabel _ s _ _) = processStmt s
processStmt (CCase e s _) = do
  eNames <- processExpr e
  sNames <- processStmt s
  return $ eNames++sNames
processStmt (CCases e1 e2 s _) = do
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  sNames <- processStmt s
  return $ e1Names++e2Names++sNames
processStmt (CDefault s _) = processStmt s
processStmt (CExpr Nothing _) = return []
processStmt (CExpr (Just e) _) = processExpr e
processStmt (CCompound _ bItems _) = do
  bItemNames <- mapM processBlockItem bItems
  return $ concat bItemNames
-- below is an 'if' statement with no 'else' body
processStmt (CIf cond thenBody Nothing _) = do
  condNames <- processExpr cond
  thenNames <- processStmt thenBody
  return $ condNames++thenNames
-- ...and one *with* an 'else' body
processStmt (CIf cond thenBody (Just elseBody) _) = do
  condNames <- processExpr cond
  thenNames <- processStmt thenBody
  elseNames <- processStmt elseBody
  return $ condNames++thenNames++elseNames
processStmt (CSwitch e s _) = do
  exprNames <- processExpr e
  stmtNames <- processStmt s
  return $ exprNames++stmtNames
processStmt (CWhile cond body _ _) = do
  condNames <- processExpr cond
  bodyNames <- processStmt body
  return $ condNames++bodyNames
-- the combinatorics of all the ways to write a 'for' statement
-- in C make handling this one tedious: there are 12 cases...
processStmt (CFor (Left (Just e1)) (Just e2) (Just e3) s _) = do
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  e3Names <- processExpr e3
  sNames <- processStmt s
  return $ e1Names++e2Names++e3Names++sNames
processStmt (CFor (Left (Just e1)) Nothing (Just e2) s _) = do
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  sNames <- processStmt s
  return $ e1Names++e2Names++sNames
processStmt (CFor (Left (Just e1)) (Just e2) Nothing s _) = do
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  sNames <- processStmt s
  return $ e1Names++e2Names++sNames
processStmt (CFor (Left (Just e1)) Nothing Nothing s _) = do
  e1Names <- processExpr e1
  sNames <- processStmt s
  return $ e1Names++sNames
processStmt (CFor (Left Nothing) (Just e1) (Just e2) s _) = do
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  sNames <- processStmt s
  return $ e1Names++e2Names++sNames
processStmt (CFor (Left Nothing) Nothing (Just e1) s _) = do
  e1Names <- processExpr e1
  sNames <- processStmt s
  return $ e1Names++sNames
processStmt (CFor (Left Nothing) (Just e1) Nothing s _) = do
  e1Names <- processExpr e1
  sNames <- processStmt s
  return $ e1Names++sNames
processStmt (CFor (Left Nothing) Nothing Nothing s _) = do
  sNames <- processStmt s
  return sNames
processStmt (CFor (Right d) (Just e1) (Just e2) s _) = do
  dNames <- processDecl d
  e1Names <- processExpr e1
  e2Names <- processExpr e2
  sNames <- processStmt s
  return $ dNames++e1Names++e2Names++sNames
processStmt (CFor (Right d) Nothing (Just e1) s _) = do
  dNames <- processDecl d
  e1Names <- processExpr e1
  sNames <- processStmt s
  return $ dNames++e1Names++sNames
processStmt (CFor (Right d) (Just e1) Nothing s _) = do
  dNames <- processDecl d
  e1Names <- processExpr e1
  sNames <- processStmt s
  return $ dNames++e1Names++sNames
processStmt (CFor (Right d) Nothing Nothing s _) = do
  dNames <- processDecl d
  sNames <- processStmt s
  return $ dNames++sNames
-- the goto just points to labels, which we don't care about
processStmt (CGoto _ _) = return []
processStmt (CGotoPtr e _) = processExpr e
processStmt (CCont _) = return []
processStmt (CBreak _) = return []
processStmt (CReturn (Just e) _) = processExpr e
processStmt (CReturn Nothing _) = return []
processStmt (CAsm a _) = do
  let aStr = show $ pretty a
  hPutStrLn stderr $ "ALPACA cannot handle assembly; throwing away: "++aStr
  return []

processExpr :: CExpr -> IO [String]
processExpr (CComma exprs _) = do
  exprNames <- mapM processExpr exprs
  let exprNames' = concat exprNames
  return exprNames'
processExpr (CAssign _ expr1 expr2 _) = do
  expr1Names <- processExpr expr1
  expr2Names <- processExpr expr2
  return $ expr1Names++expr2Names
processExpr (CCond cond (Just ifTrue) ifFalse _) = do
  condNames <- processExpr cond
  ifTrueName <- processExpr ifTrue
  ifFalseName <- processExpr ifFalse
  return $ condNames++ifTrueName++ifFalseName
processExpr (CCond cond Nothing ifFalse _) = do
  condNames <- processExpr cond
  ifFalseName <- processExpr ifFalse
  return $ condNames++ifFalseName
processExpr (CBinary _ leftExpr rightExpr _) = do
  leftNames <- processExpr leftExpr
  rightNames <- processExpr rightExpr
  return $ leftNames++rightNames
processExpr (CCast type1 expr _) = do
  typeNames <- processDecl type1
  exprNames <- processExpr expr
  return $ typeNames++exprNames
processExpr (CUnary _ expr _) = do
  exprNames <- processExpr expr
  return exprNames
processExpr (CSizeofExpr expr _) = do
  exprNames <- processExpr expr
  return exprNames
processExpr (CSizeofType decl _) = do
  declNames <- processDecl decl
  return declNames
processExpr (CAlignofExpr expr _) = do
  exprNames <- processExpr expr
  return exprNames
processExpr (CAlignofType decl _) = do
  declNames <- processDecl decl
  return declNames
processExpr (CComplexReal expr _) = do
  exprNames <- processExpr expr
  return exprNames
processExpr (CComplexImag expr _) = do
  exprNames <- processExpr expr
  return exprNames
processExpr (CIndex indexee indexer _) = do
  indexeeNames <- processExpr indexee
  indexerNames <- processExpr indexer
  return $ indexeeNames++indexerNames
processExpr (CCall e1 es _) = do
  e1Names <- processExpr e1
  esNames <- mapM processExpr es
  let esNames' = concat esNames
  return $ e1Names++esNames'
-- Can we ignore bool? If yes, Done
processExpr (CMember expr ident _ _) = do
  processExpr expr
processExpr (CVar ident _) = return [(identToString ident)]
processExpr (CConst _) = return []
-- I think we don't have to worry about initializer lists?
processExpr (CCompoundLit decl _ _) = processDecl decl 
-- Assuming helper function works, Done
processExpr (CGenericSelection expr selectList _) = do
  exprNames <- processExpr expr
  listNames <- liftSelectList selectList
  return $ exprNames++listNames
processExpr (CStatExpr statement _) = processStmt statement
-- I think we don't have to do anything for this case. We already should know the label beforehand
processExpr (CLabAddrExpr _ _) = return []
-- These are baked into C, shouldn't have to worry about collecting them
processExpr (CBuiltinExpr _) = return []
-- No longer needed
-- processExpr _ = error "need to implement remaining cases of 'processExpr'"

processBlockItem :: CBlockItem -> IO [String]
processBlockItem (CBlockStmt s) = processStmt s
processBlockItem (CBlockDecl d) = processDecl d
processBlockItem (CNestedFunDef f) = processFunDef f

\end{code}

The full explanation of the various cases of C declarations is at:
hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Syntax-AST.html#t:CDeclaration.

\begin{code}

processDecl :: CDecl -> IO [String]
processDecl (CDecl declSpecs ds _) = do
  dSpecNames <- mapM processDeclSpec declSpecs
  let dSpecNames' = concat dSpecNames
  dNames <- mapM processInitDeclr ds
  let dNames' = concat dNames
  return $ dSpecNames'++dNames'
processDecl (CStaticAssert e _ _) = processExpr e

type InitDeclr = ((Maybe CDeclr),(Maybe CInit),(Maybe CExpr))

processInitDeclr :: InitDeclr -> IO [String]
processInitDeclr ((Just aDecl), (Just aInit), (Just expr)) = do
  declNames <- declrName aDecl
  initNames <- processInit aInit
  exprNames <- processExpr expr
  return $ [declNames]++initNames++exprNames
processInitDeclr (Nothing, (Just aInit), (Just expr)) = do
  initNames <- processInit aInit
  exprNames <- processExpr expr
  return $ initNames++exprNames
processInitDeclr ((Just aDecl), Nothing, (Just expr)) = do
  declNames <- declrName aDecl
  exprNames <- processExpr expr
  return $ [declNames]++exprNames
processInitDeclr ((Just aDecl), (Just aInit), Nothing) = do
  declNames <- declrName aDecl
  initNames <- processInit aInit
  return $ [declNames]++initNames
processInitDeclr (Nothing, Nothing, (Just expr)) = do
  exprNames <- processExpr expr
  return exprNames
processInitDeclr (Nothing, (Just aInit), Nothing) = do
  initNames <- processInit aInit
  return initNames
processInitDeclr ((Just aDecl), Nothing, Nothing) = do
  declNames <- declrName aDecl
  return [declNames]
--processInitDeclr _ = error "need to implement 'processInitDeclr'"

processDeclSpec :: CDeclSpec -> IO [String]
processDeclSpec (CStorageSpec _) = return []
processDeclSpec (CTypeSpec aType) = do
  typeNames <- typeName aType 
  return typeNames
processDeclSpec (CTypeQual _) = return []
processDeclSpec (CFunSpec _) = return []
processDeclSpec (CAlignSpec (CAlignAsType decl _)) = processDecl decl
processDeclSpec (CAlignSpec (CAlignAsExpr expr _)) = processExpr expr
--processDeclSpec s = error "need to implement 'processDeclSpec'"

processInit :: CInit -> IO [String]
processInit (CInitExpr expr _) = processExpr expr 
processInit (CInitList ls _) = do
  initsNames <- mapM (\(_,i)->processInit i) ls
  let initsNames' = concat initsNames
  return initsNames'

funDefName :: CFunDef -> IO String
funDefName f@(CFunDef _ declr _ _ _) = declrName declr

declrName :: CDeclr -> IO String
declrName (CDeclr (Just (Ident n _ _)) _ _ _ _) = return n
declrName _ = return "" 
declrName s = error $ "need to implement declrName for this case: "++(show $ pretty s)

typeName :: CTypeSpec -> IO [String]
typeName (CSUType name _) = csuName name
typeName (CTypeDef (Ident n _ _) _) = return [n]
typeName (CEnumType e _) = enumNames e
typeName _ = return []

enumNames :: CEnum -> IO [String]
enumNames (CEnum (Just n) (Just ns) _ _) = do
  let idents = map (\(i,_)->i) ns
      idents' = [n]++idents
      idents'' = map identToString idents'
  return idents''
enumNames (CEnum Nothing (Just ns) _ _) = do
  let idents = map (\(i,_)->i) ns
      idents' = map identToString idents
  return idents'
enumNames _ = return []

csuName :: CStructUnion -> IO [String]
--csuName (CStruct _ (Just (Ident structIdent _ _)) _ [CAttr (Ident structTypeName _ _) _ _] _) = return $ [structIdent]++[structTypeName]
csuName (CStruct _ (Just (Ident structIdent _ _)) Nothing _ _) = return $ [structIdent]
--csuName (CStruct _ Nothing _ [CAttr (Ident structTypeName _ _) _ _] _) = return [structTypeName]
csuName (CStruct _ Nothing (Just decls) _ _) = do
  declNames <- mapM processDecl decls
  let declNames' = concat declNames
  return declNames'
csuName (CStruct _ (Just (Ident structIdent _ _)) (Just decls) _ _) = do
  declNames <- mapM processDecl decls
  let declNames' = concat declNames
  return $ [structIdent]++declNames'
csuName s = error $ "need to implement csuName for this case: "++(show $ pretty s)
csuName (CStruct _ Nothing _ [] _) = return []

\end{code}
