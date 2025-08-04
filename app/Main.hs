import CompilationPipeline
  ( CompilationError (..),
    CompilationResult (..),
    compileProgram,
  )
import Data.List (intercalate)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

-- Helper function to format bytes as hex
formatBytes :: [Word8] -> String
formatBytes bytes = intercalate "," $ map (printf "&%02X") bytes

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
        Right (CompilationResult prog symbolTable translatedBytes) -> do
          putStrLn "✓ Compilation successful!"
          putStrLn $ "Program:\n" ++ show prog
          putStrLn $ "Symbol table: " ++ show symbolTable
          putStrLn $ "Translation (" ++ show (length translatedBytes) ++ " bytes):"
          putStrLn $ formatBytes translatedBytes
    _ -> do
      putStrLn "Usage: dull <filename>"
      exitFailure
