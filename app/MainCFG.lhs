This is based on:
https://www.cs.cornell.edu/courses/cs412/2008sp/lectures/lec24.pdf

\begin{code}
module MainCFG where

import Lib
import Data.Maybe
import Data.List
import System.Environment
import Language.C
import Language.C.Data.Node
import Data.Graph.Inductive.Graph (mkGraph, size)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.Dominators (dom)
import Data.Graph.Inductive.Query.TransClos
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.Basic (grev)

main :: IO ()
main = do

  -- Setup
  a <- getArgs  
  if (length a) < 1
    then error "Usage: llama <foo.c>"
    else return ()
  {- Index 0 exists following the above check -}
  let fileName = a !! 0
  p' <- runPreprocessor fileName [] -- no preprocessor args for now

  -- Main logic  
  ast <- parseFile p'
  let funcs = extractFuncs ast
  let interprocCfgs = map makeInterCfg funcs
  let intraprocCfg = linkCfgs interprocCfgs

  -- Try out fgl
  let g = genGraph intraprocCfg
  let d = dom g 1
  putStrLn "---"
  putStrLn "the dominators of 1:"
  putStrLn $ show d
  let pd = dom (grev g) (-1)
  putStrLn "the postdominators of -1:"
  putStrLn $ show pd

  let pps = pinchPoints g 1 (-1)
  putStrLn $ show pps
  
  -- Display
  displayCfg intraprocCfg
  return ()

pinchPoints :: Gr MyNode Int -> Int -> Int -> [MyNode]
pinchPoints g startPoint stopPoint =
  let shortie = sp startPoint stopPoint g
      dominators = dom g startPoint
      postdominators = dom (grev g) stopPoint
  in error "need to implement pinchPoints"

makeBasic :: CFG -> CFG
makeBasic (CFG n1 n2 ns es) =
  let es' = foldl (\acc e-> contractEdges e acc) es es
  in CFG n1 n2 ns es'

-- There should be no pair of basic blocks (B1,B2) such that:
-- – B2 is a successor of B1
-- – B1 has one outgoing edge
-- – B2 has one incoming edge
contractEdges :: (MyNode,MyNode) -> [(MyNode,MyNode)] -> [(MyNode,MyNode)]
contractEdges e@(s,v) es =
  if (shouldContract e es)
    then
      let es' = filter (\e1 -> e1/=e) es
          ss = succs v es
          newEdges = map (\n->(s,n)) ss
      in es'++newEdges
    else es

shouldContract :: (MyNode,MyNode) -> [(MyNode,MyNode)] -> Bool
shouldContract (n1,n2) es = ((inDegree n2 es)==1) && ((outDegree n1 es)==1)

succs :: MyNode -> [(MyNode,MyNode)] -> [MyNode]
succs n es =
  let ss = filter (\(n1,_)->n1==n) es
  in map (\(_,n2)->n2) ss

isSucc :: MyNode -> MyNode -> [(MyNode,MyNode)] -> Bool
isSucc n1 n2 es = (n1,n2) `elem` es

inDegree :: MyNode -> [(MyNode,MyNode)] -> Int
inDegree n es =
  let ss = filter (\(_,s)->s==n) es
  in length ss

outDegree :: MyNode -> [(MyNode,MyNode)] -> Int
outDegree n es =
  let ss = filter (\(s,_)->s==n) es
  in length ss

data CFG = CFG {
  entryN :: MyNode,
  exitN :: MyNode,
  nodes :: [MyNode],
  edges :: [(MyNode,MyNode)]
}

genGraph :: CFG -> Gr MyNode Int
genGraph (CFG entryNode exitNode ns es) = mkGraph gNodes gEdges
  where
    gNodes = map (\n@(MyNode _ i)->(i,n)) ns
    gEdges = map (\e@((MyNode _ e1),(MyNode _ e2))->(e1,e2,1)) es

data MyNode = MyNode {
  statement :: CStat,
  nodeLabel :: Int
}

instance Eq MyNode where
  (MyNode _ l1) == (MyNode _ l2) = l1==l2

makeInterCfg :: CFunDef -> CFG
makeInterCfg f@(CFunDef _ _ _ s _) =
  let cfg1 = cfg s
      cfg2 = makeEntryAndExit f cfg1
      cfg3 = resolveReturns cfg2
      cfg4 = resolveGotos cfg3
      cfg5 = removeSelfEdges cfg4
      cfg6 = makeBasic cfg5
      cfg7 = reachableCfg cfg6
  in cfg7

reachableCfg :: CFG -> CFG
reachableCfg (CFG n1 n2 _ es) =
  let es' = reachable n1 es
      ns = nub $ concat $ map (\(na,nb)->[na,nb]) es'
  in CFG n1 n2 ns es'

reachable :: MyNode -> [(MyNode,MyNode)] -> [(MyNode,MyNode)]
reachable n es =
  let (es',rest) = partition (\(s,_)->s==n) es
      neighbs = map (\(_,nb)->nb) es'
  in es'++(concat $ map (\x->reachable x rest) neighbs)

removeSelfEdges :: CFG -> CFG
removeSelfEdges (CFG n1 n2 ns es) =
  let es' = filter (\(a,b)-> a /= b) es
  in CFG n1 n2 ns es'

makeEntryAndExit :: CFunDef -> CFG -> CFG
makeEntryAndExit f (CFG entryNode exitNode ns es) =
  let funcEntry = MyNode (CExpr Nothing undefNode) (fEntryLabel f)
      funcExit = MyNode (CExpr Nothing undefNode) (fExitLabel f)
      ns' = [funcEntry,funcExit]++ns
      es' = [(funcEntry,entryNode),(exitNode,funcExit)]++es
  in CFG funcEntry funcExit ns' es'

fEntryLabel :: CFunDef -> Int
fEntryLabel f = posRow $ posOf $ nodeInfo f

fExitLabel :: CFunDef -> Int
fExitLabel f = -(fEntryLabel f)

resolveReturns :: CFG -> CFG
resolveReturns (CFG entryNode exitNode ns es) =
  let es' = filter (not . sourceIsReturn) es
      rets = filter isReturn ns
      backedges = map (\r->(r,exitNode)) rets
      es'' = es' ++ backedges
  in CFG entryNode exitNode ns es''

resolveGotos :: CFG -> CFG
resolveGotos (CFG e1 e2 ns es) =
  let es' = filter (not . sourceIsGoto) es
      gotos = filter isGoto ns
      gotoEdges = map (makeGotoEdge ns) gotos
      es'' = es' ++ gotoEdges
  in CFG e1 e2 ns es''

makeGotoEdge :: [MyNode] -> MyNode -> (MyNode,MyNode)
makeGotoEdge ns n@(MyNode s@(CGoto l _) _) =
  let mLabel = find (matchingLabel l) ns
  in
    if isJust mLabel
      then
        let (Just lab) = mLabel
        in (n,lab)
      else error $ "could not find label to: "++(show $ pretty s)

matchingLabel :: Ident -> MyNode -> Bool
matchingLabel l1 (MyNode (CLabel l2 _ _ _) _) = l1==l2
matchingLabel _ _ = False

isGoto :: MyNode -> Bool
isGoto (MyNode (CGoto _ _) _) = True
isGoto _ = False

sourceIsGoto :: (MyNode,MyNode) -> Bool
sourceIsGoto (n,_) = isGoto n

isReturn :: MyNode -> Bool
isReturn (MyNode (CReturn _ _) _) = True
isReturn _ = False

sourceIsReturn :: (MyNode,MyNode) -> Bool
sourceIsReturn (n,_) = isReturn n

getStmts :: [CBlockItem] -> [CStat]
getStmts bs =
  let bs' = filter isStmt bs
  in map (\(CBlockStmt s)->s) bs'

isStmt :: CBlockItem -> Bool
isStmt (CBlockStmt _) = True
isStmt _ = False

cfg :: CStat -> CFG
-- CFG for Block Statment
cfg (CCompound _ bItems _) =
  let ss = getStmts bItems
      stmtCfgs = map cfg ss
  in concatCfgs stmtCfgs
-- CFG for If-Else Statement
cfg s@(CIf _ s1 (Just s2) _) = 
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      tBlock = cfg s1
      fBlock = cfg s2
      ns = (nodes tBlock)++(nodes fBlock)++[entryNode,exitNode]
      e1 = (entryNode, entryN tBlock)
      e2 = (entryNode, entryN fBlock)
      e3 = (exitN tBlock, exitNode)
      e4 = (exitN fBlock, exitNode)
      es = (edges tBlock)++(edges fBlock)++[e1,e2,e3,e4]
  in CFG entryNode exitNode ns es
-- CFG for If Statement (no Else block)
cfg s@(CIf _ s1 Nothing _) = 
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      tBlock = cfg s1
      ns = (nodes tBlock)++[entryNode,exitNode]
      e1 = (entryNode, entryN tBlock)
      e2 = (exitN tBlock, exitNode)
      es = (edges tBlock)++[e1,e2]
  in CFG entryNode exitNode ns es
cfg s@(CFor _ _ _ s1 _) = 
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      tBlock = cfg s1
      ns = (nodes tBlock)++[entryNode,exitNode]
      e1 = (entryNode, entryN tBlock)
      e2 = (exitN tBlock, exitNode)
      es = (edges tBlock)++[e1,e2]
  in CFG entryNode exitNode ns es
cfg s@(CSwitch _ ss _) = 
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      (ns,es) = processSwitchBody ss entryNode exitNode
  in CFG entryNode exitNode ns es
cfg s@(CWhile _ b _ _) =
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      tBlock = cfg b
      ns = (nodes tBlock)++[entryNode,exitNode]
      e1 = (entryNode, entryN tBlock)
      e2 = (exitN tBlock, entryNode)
      e3 = (entryNode, exitNode)
      es = (edges tBlock)++[e1,e2,e3]
  in CFG entryNode exitNode ns es
cfg s@(CCase _ b _) =
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      c = cfg b
      ns = [entryNode,exitNode]++(nodes c)
      e1 = (entryNode, entryN c)
      e2 = (exitN c, exitNode)
      es = [e1,e2]++(edges c)
  in CFG entryNode exitNode ns es
cfg s@(CCases _ _ b _) =
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      c = cfg b
      ns = [entryNode,exitNode]++(nodes c)
      e1 = (entryNode, entryN c)
      e2 = (exitN c, exitNode)
      es = [e1,e2]++(edges c)
  in CFG entryNode exitNode ns es
cfg s@(CDefault b _) =
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      c = cfg b
      ns = [entryNode,exitNode]++(nodes c)
      e1 = (entryNode, entryN c)
      e2 = (exitN c, exitNode)
      es = [e1,e2]++(edges c)
  in CFG entryNode exitNode ns es
cfg s@(CLabel _ b _ _) =
  let entryNode = makeNode s
      exitNode = exitOf entryNode
      c = cfg b
      ns = [entryNode,exitNode]++(nodes c)
      e1 = (entryNode, entryN c)
      e2 = (exitN c, exitNode)
      es = [e1,e2]++(edges c)
  in CFG entryNode exitNode ns es
cfg s = basicCfg s

basicCfg :: CStat -> CFG
basicCfg s = 
  let singleNode = makeNode s
  in CFG singleNode singleNode [singleNode] []

processSwitchBody :: CStat -> MyNode -> MyNode -> ([MyNode],[(MyNode,MyNode)])
processSwitchBody (CCompound _ blockItems _) entryNode exitNode =
  let mBlockStmts = map extractBlockStmt blockItems
      blockStmts = catMaybes mBlockStmts
      blockCfgs = map cfg blockStmts
      mEs1 = map (edgeToCaseAndDefault entryNode) blockCfgs
      es1 = catMaybes mEs1
      blockCfg = concatCfgs blockCfgs
      toExit = (exitN blockCfg, exitNode)
      es2 = redirectBreaks exitNode blockCfg
      ns = [entryNode,exitNode]++(nodes blockCfg)
  in (ns, (es1++es2++[toExit]))
processSwitchBody s entryNode exitNode =
  let c = cfg s
      e1 = (entryNode, entryN c)
      e2 = (exitN c, exitNode)
      es = [e1,e2]++(edges c)
      ns = [entryNode,exitNode]++(nodes c)
  in (ns,es)

edgeToCaseAndDefault :: MyNode -> CFG -> Maybe (MyNode,MyNode)
edgeToCaseAndDefault entryNode (CFG n@(MyNode (CCase _ _ _) _) _ _ _) = Just (entryNode, n)
edgeToCaseAndDefault entryNode (CFG n@(MyNode (CCases _ _ _ _) _) _ _ _) = Just (entryNode, n)
edgeToCaseAndDefault entryNode (CFG n@(MyNode (CDefault _ _) _) _ _ _) = Just (entryNode, n)
edgeToCaseAndDefault _ _ = Nothing
  
redirectBreaks :: MyNode -> CFG -> [(MyNode,MyNode)]
redirectBreaks exitNode (CFG _ _ ns es) =
  let bs = filter isBreak ns
      es' = filter (\(n1,_)-> not $ (n1 `elem` bs)) es
      es1 = map (\b-> (b,exitNode)) bs
  in es'++es1

isBreak :: MyNode -> Bool
isBreak (MyNode (CBreak _) _) = True
isBreak _ = False

extractBlockStmt :: CBlockItem -> Maybe CStat
extractBlockStmt (CBlockStmt s) = Just s
extractBlockStmt _ = Nothing

makeNode :: CStat -> MyNode
makeNode s =
  let lineN = posRow $ posOf $ nodeInfo s
  in MyNode s lineN

exitOf :: MyNode -> MyNode
exitOf n = MyNode (CExpr Nothing undefNode) (-(nodeLabel n))

concatCfgs :: [CFG] -> CFG
concatCfgs [] = error "trying to concat 0 CFGS"
concatCfgs [c] = c
concatCfgs cs = foldl (\acc c ->
                        CFG
                          (entryN acc)
                          (exitN c)
                          ((nodes acc)++(nodes c))
                          ((edges acc)++(edges c)++[(exitN acc,entryN c)])
                      ) (head cs) (tail cs)

linkCfgs :: [CFG] -> CFG
linkCfgs [c] = c
linkCfgs _ = error "need to implement linkCfgs for more than one"

instance Show MyNode where
  show (MyNode _ l) = show l

displayCfg :: CFG -> IO ()
displayCfg (CFG en ex ns es) = do
  putStrLn "CFG:"
  putStrLn $ " entry node: "++(show en)
  putStrLn $ " exit node: "++(show ex)
  putStrLn " ---"
  let ns' = nub ns
  let es' = nub es
  putStrLn $ " nodes: "++(concat $ map (\n->(show n)++",") ns')
  putStrLn $ " edges: "
  mapM_ (\(n1,n2)->do putStrLn $ "  "++(show n1)++"->"++(show n2)) es'

\end{code}
