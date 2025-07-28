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
import Ir.Translate (translateProgram)
import Ir.Types (IrProgram)
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
    irProgram :: IrProgram,
    irSymbolTable :: SymbolTable
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
        let (irProg, symbTable, nextLabelIdx) = translateProgram prog' finalState
        return $
          Right $
            CompilationResult
              { compiledProgram = prog',
                finalSymbolTable = finalState,
                irProgram = irProg,
                irSymbolTable = symbTable
              }

  case result of
    Left (ex :: SomeException) -> return $ Left (TypeCheckError $ show ex)
    Right compilationResult -> return compilationResult