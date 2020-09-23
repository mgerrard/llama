\begin{code}
module Modularize where

import Helpers
import Language.C
import Language.C.Analysis
import Language.C.Data.Ident
import System.Process
import System.Directory
import System.Environment
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Exception.Base (assert)
import Data.String.Utils (startswith)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Strict as Map
import Data.List (find, intersperse,isInfixOf)

\end{code}

This code takes in a function name and a 
C abstract syntax tree, and returns a new
abstract syntax tree that allows a program
analyzer to reason about reachability properties
from the callsite of the function-of-interest.
Because this kind of modular analysis rips the
function out of its runtime context, we have to
assume that the global variables as well
as the function's parameters can take on any value.

To create this new AST, we will declare the
parameters of the function-of-interest as globals
(giving them unique names, to use in the issued call)
just before {\tt main}, collect all globals,
make each of the globals symbolic at the entry point
of {\tt main}, and finally issue a call to the
function-of-interest. The resulting AST will
be (fingers crossed) amenable to a modular
analysis when pretty-printed. The {\tt modularize}
function is placed in an {\tt IO} monad just for
debugging purposes.

We take advantage of an AST analysis that comes
from the {\tt language-c} codebase to collect
the global definitions and typedef/struct
definitions. This is where the functions
{\tt analyseAST}, {\tt runTrav_}, and
{\tt checkResult} come from. Chaining these
functions together involves some higher-level
Haskell trickery that's too complicated for
me (arrows and applicative stuff);
I copy and pasted the magical invocations after
reading through and trying the code from
github.com/visq/language-c/blob/master/examples/SearchDef.hs.

The function {\tt analyseAST} comes from
hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Analysis-AstAnalysis.html.

\begin{code}

modularize :: String -> AST -> Maybe AST -> Bool -> IO AST
modularize funcName ast mAst debug = do
  let ast' = possiblySmoosh ast mAst
      ast'' = enforceNamedStructs ast'
      (ast''',pNames) = makeParamsGlobal ast'' funcName
  (globals,_) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast'''
  let globals' = filterGlobalDecls symbolicEvent globals

  setup <- symbolicSetup globals' debug
  let m = modularMain setup funcName pNames
      ast'''' = appendMain ast''' m
  return ast''''

possiblySmoosh :: AST -> Maybe AST -> AST
possiblySmoosh a Nothing = a
possiblySmoosh (CTranslUnit origAst a) (Just (CTranslUnit stubAst _)) =
  (CTranslUnit (stubAst++origAst) a)

enforceNamedStructs :: AST -> AST
enforceNamedStructs (CTranslUnit es _) =
  let esIds = zip es [0..]
      es' = map nameStruct esIds
  in CTranslUnit es' undefNode

nameStruct :: (CExtDecl,Int) -> CExtDecl
nameStruct ((CDeclExt (CDecl [td@(CStorageSpec (CTypedef _)),(CTypeSpec (CSUType csu p1))] p2 p3)),i) =
  let csu' = enforceName csu i
  in CDeclExt (CDecl [td,(CTypeSpec (CSUType csu' p1))] p2 p3)
nameStruct (e,_) = e

enforceName :: CStructUnion -> Int -> CStructUnion
enforceName (CStruct p1 Nothing p3 p4 p5) i =
  let n = makeIdent ("__llama_struct_id_"++(show i))
  in CStruct p1 (Just n) p3 p4 p5
enforceName c _ = c

\end{code}

The following three functions extract
the parameter declarations from the 
function-of-interest's signature, gives
these parameter names a unique prefix
(``__alpaca_param''), and then makes
the parameters global declarations
appended to the AST so that they'll
be made symbolic along with the rest
of the relevant globals.

\begin{code}

makeParamsGlobal :: AST -> String -> (AST,[String])
makeParamsGlobal ast fName =
  let f = grabFunction ast fName
      ps = grabParams f
      declNamePairs = map makeGlobal ps
      ds = map fst declNamePairs
      pNames = map snd declNamePairs
      ast' = appendDecls ast ds
  in (ast',pNames)
             
makeGlobal :: CDecl -> (CExtDecl,String)
makeGlobal d =
  let v = extractDeclVarStr d
      v' = "__alpaca_param_"++v
      d' = renameDecl d v'
  in ((CDeclExt d'), v')

appendDecls :: AST -> [CExtDecl] -> AST
appendDecls ast@(CTranslUnit es _) ds = (CTranslUnit (es++ds) undefNode)

\end{code}

The following two functions---{\tt symbolicSetup} and
{\tt makeSymbolic} are essentially drivers for the
meat and potatoes of this module: {\tt makeVarSymbolic}.
The first takes in a map of the (pruned) global definitions
and makes each of the ``object definitions'', i.e.,
global variables, symbolic. The second function just
unpacks the name and type of each object definition
and throws an error if we get to something that's
{\it not} an object definition (this case should be
unreachable).

The data type {\tt GlobalDecls} comes from
hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Analysis-SemRep.html.

\begin{code}
      
symbolicSetup :: GlobalDecls -> Bool -> IO [CBlockItem]
symbolicSetup gs debug = do
  if debug
    then do
      putStrLn "** DEBUG *******************"
      putStrLn "* map used in making variables symbolic:"
      print $ pretty gs
      putStrLn "** DEBUG *******************\n"
    else return ()

  let os = Map.toList (gObjs gs)
  bs <- mapM (makeSymbolic gs debug) os
  return $ concat bs

makeSymbolic :: GlobalDecls -> Bool -> (Ident, IdentDecl) -> IO [CBlockItem]
makeSymbolic gs debug ((Ident i _ _),(ObjectDef (ObjDef (VarDecl _ _ ty) _ _))) = do
  makeVarSymbolic i ty gs debug
makeSymbolic _ debug ((Ident i _ _),_) = do
  if debug
    then do
      putStrLn "** DEBUG *******************"
      putStrLn $ "found: "++i++". eventually we should only be handling ObjectDefs in makeSymbolic"
      putStrLn "** DEBUG *******************\n"      
    else return ()
  return []

\end{code}

The following function is what actually generates the
symbolic variables. The data types used here come from
hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Analysis-SemRep.html,
which offers a more semantic representation of the program as opposed
to the purely syntactic representation given by the AST.
Especially of interest is the {\tt Type} data type, which
is what we're pattern-matching on in this function.

Making a direct type, e.g., an integer variable, is straighforward:
we construct an initialization statement with the the variable's
identifier on the lefthand size and ``__VERIFIER_nondet_{type}()''
on the righthand side.
Making a variable whose type is defined by a typedef just involves
pattern matching on the base type (this was recovered during the call
to {\tt analyseAST}) and then calling {\tt makeVarSymbolic} again.

The more involved cases are the pointer variables and the struct
variables. In the case of a pointer, we first have to declare an
auxillary variable that has a unique name and make this auxillary
variable symbolic; now the pointer can actually point to
something in memory; we can then assign the address of this
auxillary variable to the pointer.
In the case of a struct, we do something similar, where
we first make each of the fields of the struct hold symbolic
values in memory by declaring respective auxillary variables,
and then we can define the struct using these auxillary variables
as the initializers.

\begin{code}

makeVarSymbolic :: String -> Type -> GlobalDecls -> Bool -> IO [CBlockItem]
makeVarSymbolic i (DirectType (TyIntegral TyInt) _ _) _ _ = symInit i "int"
makeVarSymbolic i (DirectType (TyIntegral TyUInt) _ _) _ _ = symInit i "uint"
makeVarSymbolic i (DirectType (TyIntegral TyChar) _ _) _ _ = symInit i "char"
makeVarSymbolic i (DirectType (TyIntegral TySChar) _ _) _ _ = symInit i "char"
makeVarSymbolic i (DirectType (TyIntegral TyUChar) _ _) _ _ = symInit i "uchar"
makeVarSymbolic i (DirectType (TyIntegral TyShort) _ _) _ _ = symInit i "short"
makeVarSymbolic i (DirectType (TyIntegral TyUShort) _ _) _ _ = symInit i "ushort"
makeVarSymbolic i (DirectType (TyIntegral TyLong) _ _) _ _ = symInit i "long"
makeVarSymbolic i (DirectType (TyIntegral TyULong) _ _) _ _ = symInit i "ulong"
makeVarSymbolic i (DirectType (TyIntegral TyBool) _ _) _ _ = symInit i "bool"
makeVarSymbolic i (DirectType (TyFloating TyFloat) _ _) _ _ = symInit i "float"
makeVarSymbolic i (DirectType (TyFloating TyDouble) _ _) _ _ = symInit i "double"
makeVarSymbolic i (PtrType (DirectType TyVoid _ _) _ _) gs _ = symInit i "pointer"
makeVarSymbolic i (PtrType (TypeDefType (TypeDefRef _ ty@(DirectType tName _ _) _) _ _) _ _) gs debug = do
  -- declare direct type under a unique name
  let uniqueName = "__alpaca_ref_"++i
  auxVarDecl <- declareAuxVar (uniqueName,ty)
  -- make this direct type symbolic
  symVar <- makeVarSymbolic uniqueName ty gs debug
  -- assign the address of this direct type to the pointer
  let d = makeExpr (i++"= &"++uniqueName)
      d' = CBlockStmt (CExpr (Just d) undefNode)
  return $ [auxVarDecl]++symVar++[d']
makeVarSymbolic i (TypeDefType (TypeDefRef _ ty _) _ _) gs debug = makeVarSymbolic i ty gs debug
makeVarSymbolic i (PtrType ty@(DirectType tName _ _) _ _) gs debug = do
  -- declare direct type under a unique name
  let uniqueName = "__alpaca_ref_"++i
  auxVarDecl <- declareAuxVar (uniqueName,ty)
  -- make this direct type symbolic
  symVar <- makeVarSymbolic uniqueName ty gs debug
  -- assign the address of this direct type to the pointer
  let d = makeExpr (i++"= &"++uniqueName)
      d' = CBlockStmt (CExpr (Just d) undefNode)
  return $ [auxVarDecl]++symVar++[d']
makeVarSymbolic i (DirectType (TyComp ct@(CompTypeRef sRef StructTag _)) _ _) gs debug = do
  auxNameTypes <- extractAuxPairs sRef gs
  if debug
    then do
      putStrLn "** DEBUG *******************"
      putStrLn $ "struct "++i++"'s auxNameTypes: "
      mapM_ (\(n,ty)->do putStrLn $ n++","++(show $ pretty ty)) auxNameTypes
      putStrLn "** DEBUG *******************\n"
    else return ()
  auxDecls <- mapM declareAuxVar auxNameTypes
  
  auxSetup <- mapM (\(n,ty)->do makeVarSymbolic n ty gs debug) auxNameTypes
  let auxSetup' = concat auxSetup
      auxNames = map fst auxNameTypes
      init = structInit ct i auxNames
  return $ auxDecls++auxSetup'++[init]
makeVarSymbolic i (DirectType (TyEnum _) _ _) _ _ = do
  error "should not be trying to make a type enum symbolic"
makeVarSymbolic i (DirectType (TyBuiltin _) _ _) _ _ = do
  error "should not be trying to make a builtin type symbolic"
makeVarSymbolic i (DirectType (TyComp (CompTypeRef sRef UnionTag _)) _ _) gs _ = do
  error "need to implement makeVarSymbolic for union types"
makeVarSymbolic i d _ _ = do
  putStrLn $ "found this def of "++(i)++": "++(show $ pretty d)
  error "need to implement other cases of makeVarSymbolic"

\end{code}

The functions {\tt modularMain} and {\tt appendMain} finish
up the modularizing logic after all the hard work has been
done. The first creates a function call statement with the
appropriate name and uniquely-named parameters that have
been made symbolic; and then glues together the symbolic
setup and this function call into a new function definition
that we call ``main''. The second function just appends this
new definition of ``main'' onto the pruned AST's list of
external declarations.

\begin{code}

modularMain :: [CBlockItem] -> String -> [String] -> CExtDecl
modularMain initializations funcName funcArgs =
  let callStmt = makeCallStmt funcName funcArgs
      body = (CCompound [] (initializations++[(CBlockStmt callStmt)]) undefNode)
      newMain = (CFDefExt (CFunDef [intType] (makeDeclarator "main()") [] body undefNode))
  in newMain

appendMain :: AST -> CExtDecl -> AST
appendMain (CTranslUnit es _) newMain = (CTranslUnit (es++[newMain]) undefNode)

\end{code}

The following block of functions are helpers used within
the different cases of {\tt makeVarSymbolic}.

\begin{code}

symInit :: String -> String -> IO [CBlockItem]
symInit lhs tyStr = do
  let init = varInit lhs ("__VERIFIER_nondet_"++tyStr++"()")
  return $ [CBlockDecl init]
  
extractMemberNameType :: MemberDecl -> (String,Type)
extractMemberNameType (MemberDecl (VarDecl (VarName (Ident n _ _) _) _ ty) _ _) = (n,ty)
extractMemberNameType _ = error "cannot yet handle AnonBitField"  

extractAuxPairs :: SUERef -> GlobalDecls -> IO [(String,Type)]
extractAuxPairs sRef gs = do
  let mTagDef = Map.lookup sRef (gTags gs)
  if isJust mTagDef
    then do
      let (Just (CompDef (CompType _ _ structMembers _ _))) = mTagDef
          pairs = map extractMemberNameType structMembers
          -- let's give these a unique name
          pairs' = map (\(n,ty)->("__alpaca_struct_mem_"++n,ty)) pairs
      return pairs'
    else error $ "SUERef not found in tag map: "++(show $ pretty sRef)

declareAuxVar :: (String,Type) -> IO CBlockItem
declareAuxVar (n,ty@(DirectType _ _ _)) = do
  -- though this should be a CBlockDecl, I just want to manipulate
  -- at the string level, so the following is a hack
  let d = makeExpr ((show $ pretty ty)++" "++n)
  return $ CBlockStmt (CExpr (Just d) undefNode)
declareAuxVar (n,ty@(PtrType (DirectType TyVoid _ _) _ _)) = do
  let d = makeExpr ("void *"++n)
  return $ CBlockStmt (CExpr (Just d) undefNode)
declareAuxVar (n,(TypeDefType (TypeDefRef _ ty _) _ _)) = declareAuxVar (n,ty)
declareAuxVar (n,ty) = do
  error $ "need to implement declareAuxVar for type: "++(show $ pretty ty)

structInit :: CompTypeRef -> String -> [String] -> CBlockItem
structInit ct lhs auxNames =
  let initializers = concat $ intersperse "," auxNames
      typeCast = "("++(show $ pretty ct)++")"
      init = varInit lhs (typeCast++"{"++initializers++"}")
  in CBlockDecl init

symbolicEvent :: DeclEvent -> Bool  
symbolicEvent (TagEvent _) = True
symbolicEvent (DeclEvent (Declaration (Decl (VarDecl _ _ (FunctionType _ _)) _))) = False
symbolicEvent (DeclEvent (ObjectDef _)) = True
symbolicEvent (DeclEvent (Declaration (Decl (VarDecl (VarName (Ident n _ _) _) _ _) _))) =
  not $ containsLangCString n
symbolicEvent (TypeDefEvent (TypeDef (Ident n _ _) _ _ _)) = not $ containsLangCString n
symbolicEvent _ = False

containsLangCString :: String -> Bool
containsLangCString n =
  ("__builtin_" `isInfixOf` n) ||
  ("__FUNCTION__" `isInfixOf` n) ||
  ("__PRETTY_FUNCTION__" `isInfixOf` n) ||
  ("__func__" `isInfixOf` n)

\end{code}

The following function is used to check the result of collecting
the global declarations. It is taken from
github.com/visq/language-c/blob/master/examples/SearchDef.hs.

\begin{code}

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

\end{code}

The remaining functions are helper functions that were
mostly taken from previous code. Are these functions
well-written? No. Am I going to rewrite them now?
Also no.

\begin{code}

grabFunction :: CTranslUnit -> String -> CFunDef
grabFunction ast f =
  let maybeF = find (functionIs f) (collectFunctions ast)
  in
    if isJust maybeF
      then
        let (Just (CFDefExt func)) = maybeF
        in func
      else error $ "function '"++(f)++"' not found in AST"

collectFunctions :: CTranslUnit -> [CExtDecl]
collectFunctions (CTranslUnit es _) = filter isFunction es

isFunction :: CExtDecl -> Bool
isFunction (CFDefExt (CFunDef _ _ _ _ _)) = True
isFunction _ = False

functionNameIs :: String -> CDeclr -> Bool
functionNameIs s declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in s == n

functionIs :: String -> CExtDecl -> Bool
functionIs s (CFDefExt (CFunDef _ declr _ _ _)) = functionNameIs s declr
functionIs _ _ = False

functionNameStartsWith :: String -> CDeclr -> Bool
functionNameStartsWith s declr =
  let (CDeclr (Just (Ident n _ _)) _ _ _ _) = declr
  in startswith s n

grabParams :: CFunDef -> [CDecl]
grabParams f =
  let (CFunDef _ (CDeclr _ paramList _ _ _) _ _ _) = f
      ps = head paramList
      (CFunDeclr (Right (params, _)) _ _) = ps
  in params

renameDecl :: CDecl -> String -> CDecl
renameDecl decl newName = 
  let (CDecl p1 exprs p2) = decl
      singleExpr = assert (not (null exprs)) $ head exprs
      (Just declr, p3, p4) = singleExpr
      (CDeclr _ p5 p6 p7 p8) = declr
      declr' = (CDeclr (Just (makeIdent newName)) p5 p6 p7 p8)
      exprs' = [(Just declr', p3, p4)]
   in (CDecl p1 exprs' p2)

makeIdent :: String -> Ident
makeIdent s = Ident s 0 undefNode

extractDeclVarStr :: CDecl -> String
extractDeclVarStr decl =
  let (CDecl _ exprs _) = decl
      singleExpr = assert (not (null exprs)) $ head exprs
      (Just declr, _, _) = singleExpr
      (CDeclr (Just iden) _ _ _ _) = declr
      (Ident n _ _) = iden
   in n

makeExpr :: String -> CExpr
makeExpr s =
  let iden = makeIdent s
  in CVar iden undefNode

makeDeclaratorRhs :: String -> CInit
makeDeclaratorRhs rhs = CInitExpr (makeVar rhs) undefNode

varInit :: String -> String -> CDecl
varInit lhs rhs =
  let declr = makeDeclarator lhs
      vInit = makeDeclaratorRhs rhs
  in CDecl [] [(Just declr, Just vInit, Nothing)] undefNode

makeDeclarator :: String -> CDeclr
makeDeclarator s =
  let iden = makeIdent s
  in CDeclr (Just iden) [] Nothing [] undefNode

makeVar :: String -> CExpr
makeVar s =
  let iden = makeIdent s
  in CVar iden undefNode
  
intType :: CDeclSpec
intType = CTypeSpec (CIntType undefNode)

makeCallStmt :: String -> [String] -> CStat
makeCallStmt funcName funcArgs = 
  let paramVars = map makeVar funcArgs
      funcCall = CCall (makeVar funcName) paramVars undefNode
      stmt = CExpr (Just funcCall) undefNode
  in stmt

\end{code}
