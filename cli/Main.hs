module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Hazel.Parser (parseFile)

parse :: FilePath -> IO ()
parse file = do
  result <- parseFile file
  case result of
    (Left err) -> putStrLn $ "failed parsing `" ++ file ++ "': `" ++ err ++ "'."
    (Right _) -> putStrLn $ "successfully parsed `" ++ file ++ "'."

tryParse :: String -> IO ()
tryParse file = do
  exists <- doesFileExist file
  if exists
    then parse file
    else putStrLn $ "No such file: `" ++ file ++ "'."

main :: IO ()
main = getArgs >>= mapM_ tryParse
