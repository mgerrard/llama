module Lib
    ( prune,
      parseFile,
      printAST,
      replaceAssert,
      getDebugArg,
      getMaybeStubAst,
      astToString,
      modularize,
      runPreprocessor,
      extractFuncs,
      collectFunctions,
      AST
    ) where

import Helpers
import Prune
import Modularize
