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
      AST
    ) where

import Helpers
import Prune
import Modularize
