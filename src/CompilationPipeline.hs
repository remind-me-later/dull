module CompilationPipeline
  ( CompilationResult (..),
    CompilationError (..),
    compileProgram,
  )
where

import Ast.Parser (parseProgram)
import Ast.SemanticAnalysis (analyzeProgram)
import Ast.Types (Program)
import Control.Exception (SomeException, try)
import Hir.Translate (translateProgram)
import Hir.Types (HirProgram)
import SymbolTable (SymbolTable)
import TypeSystem (BasicType)

data CompilationError
  = ParseError String
  | TypeCheckError String
  | OtherError String
  deriving (Show, Eq)

data CompilationResult = CompilationResult
  { compiledProgram :: Program BasicType,
    finalSymbolTable :: SymbolTable,
    hirProgram :: HirProgram,
    hirSymbolTable :: SymbolTable
  }
  deriving (Show, Eq)

compileProgram :: String -> String -> IO (Either CompilationError CompilationResult)
compileProgram fileName contents = do
  result <- try $ do
    -- Stage 1: Parse
    parsedProgram <- parseProgram fileName contents
    case parsedProgram of
      Left parseErr -> return $ Left (ParseError parseErr)
      Right prog -> do
        -- Stage 2: Type check and semantic analysis
        let (prog', finalState) = analyzeProgram prog
        -- Stage 3: Translate to HIR
        let (hirProg, symbTable, nextLabelIdx) = translateProgram prog' finalState
        return $
          Right $
            CompilationResult
              { compiledProgram = prog',
                finalSymbolTable = finalState,
                hirProgram = hirProg,
                hirSymbolTable = symbTable
              }

  case result of
    Left (ex :: SomeException) -> return $ Left (TypeCheckError $ show ex)
    Right compilationResult -> return compilationResult