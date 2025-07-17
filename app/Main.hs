import Ast.Parser (parseProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args

  contents <- readFile fileName
  parsedProgram <- parseProgram fileName contents
  case parsedProgram of
    Left err -> putStrLn $ "Error parsing program: " ++ err
    Right prog -> do
      putStrLn $ "Parsed program: \n" ++ show prog
