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
          putStrLn $ "Parse error: " ++ err
          exitFailure
        Left (TypeCheckError err) -> do
          putStrLn $ "Type checking error: " ++ err
          exitFailure
        Left (OtherError err) -> do
          putStrLn $ "Compilation error: " ++ err
          exitFailure
        Right (CompilationResult prog symbolTable) -> do
          putStrLn "✓ Compilation successful!"
          putStrLn $ "Program: " ++ show prog
          putStrLn $ "Symbol table: " ++ show symbolTable
    _ -> do
      putStrLn "Usage: dull <filename>"
      exitFailure
