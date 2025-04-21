module Main where

import Parser qualified (program)
import ParserCombinators (Parser (parse))
import SymbolTable qualified
  ( generateSymbolTables,
    prettyPrintDataTable,
    prettyPrintStrConstTable,
    prettyPrintVarTable,
  )
import System.Environment (getArgs)
import Token qualified (tokens)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  case parse Token.tokens contents of
    Nothing -> putStrLn "Error parsing tokens"
    Just (ts, rest_tokens) -> do
      putStrLn $ "Tokens: " ++ show ts
      putStrLn $ "Remaining input: " ++ rest_tokens
      let parsedProgram = parse Parser.program ts
      case parsedProgram of
        Nothing -> putStrLn "Error parsing program: "
        Just (prog, rest_parse) -> do
          putStrLn $ "Parsed program: " ++ show prog
          putStrLn $ "Remaining input: " ++ show rest_parse
          let (varTable, strConstTable, dataStmtTable) = SymbolTable.generateSymbolTables prog
          putStrLn $ SymbolTable.prettyPrintVarTable varTable
          putStrLn $ SymbolTable.prettyPrintStrConstTable strConstTable
          putStrLn $ SymbolTable.prettyPrintDataTable dataStmtTable
