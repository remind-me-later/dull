import CompilationPipeline
  ( CompilationError (..),
    CompilationResult (..),
    compileProgram,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fileName] -> do
      contents <- readFile fileName

      result <- compileProgram fileName contents
      case result of
        Left (ParseError err) -> do
          putStrLn $ "Parse error:\n" ++ err
          exitFailure
        Left (TypeCheckError err) -> do
          putStrLn $ "Type checking error:\n" ++ err
          exitFailure
        Left (OtherError err) -> do
          putStrLn $ "Compilation error:\n" ++ err
          exitFailure
        Right (CompilationResult prog symbolTable hir) -> do
          putStrLn "✓ Compilation successful!"
          putStrLn $ "Program:\n" ++ show prog
          putStrLn $ "Symbol table:\n" ++ show symbolTable
          putStrLn $ "HIR:\n" ++ show hir
    _ -> do
      putStrLn "Usage: dull <filename>"
      exitFailure
