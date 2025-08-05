import CompilationPipeline
  ( CompilationError (..),
    CompilationResult (..),
    compileProgram,
  )
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Word (Word8)
import Options.Applicative
import System.Exit (exitFailure)
import Text.Printf (printf)

-- CLI Options data type
data Options = Options
  { optInputFile :: String,
    optShowAst :: Bool,
    optShowSymbols :: Bool,
    optShowBinary :: Bool,
    optOutputFile :: Maybe String,
    optVerbose :: Bool
  }

-- Parser for CLI options
options :: Parser Options
options =
  Options
    <$> strArgument
      ( metavar "FILE"
          <> help "Input source file to compile"
      )
    <*> switch
      ( long "ast"
          <> short 'a'
          <> help "Show the parsed AST"
      )
    <*> switch
      ( long "symbols"
          <> short 's'
          <> help "Show the symbol table"
      )
    <*> switch
      ( long "binary"
          <> short 'b'
          <> help "Show the compiled binary output"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output file for binary (default: stdout)"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )

-- Program information
opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Compile BASIC-like programs to bytecode"
        <> header "dull - a BASIC compiler"
    )

-- Helper function to format bytes as hex
formatBytes :: [Word8] -> String
formatBytes bytes = intercalate "," $ map (printf "&%02X") bytes

-- Main function with enhanced CLI
main :: IO ()
main = do
  opts' <- execParser opts
  contents <- readFile (optInputFile opts')

  when (optVerbose opts') $
    putStrLn $
      "Compiling: " ++ optInputFile opts'

  let result = compileProgram (optInputFile opts') contents
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
      -- Success message
      when (optVerbose opts' || not (hasAnyOutput opts')) $
        putStrLn "✓ Compilation successful!"

      -- Show AST if requested
      when (optShowAst opts') $ do
        putStrLn "\n=== Abstract Syntax Tree ==="
        print prog

      -- Show symbol table if requested
      when (optShowSymbols opts') $ do
        putStrLn "\n=== Symbol Table ==="
        print symbolTable

      -- Show/save binary if requested
      when (optShowBinary opts') $ do
        case optOutputFile opts' of
          Nothing -> do
            putStrLn $ "\n=== Binary Output (" ++ show (length translatedBytes) ++ " bytes) ==="
            putStrLn $ formatBytes translatedBytes
          Just outFile -> do
            BS.writeFile outFile (BS.pack translatedBytes)
            when (optVerbose opts') $
              putStrLn $
                "Binary written to: " ++ outFile

      -- If no specific output was requested, show everything
      unless (hasAnyOutput opts') $ do
        putStrLn $ "\nProgram:\n" ++ show prog
        putStrLn $ "\nSymbol table: " ++ show symbolTable
        putStrLn $ "\nTranslation (" ++ show (length translatedBytes) ++ " bytes):"
        putStrLn $ formatBytes translatedBytes

-- Helper to check if any specific output option was selected
hasAnyOutput :: Options -> Bool
hasAnyOutput opts' = optShowAst opts' || optShowSymbols opts' || optShowBinary opts'
