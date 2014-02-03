module Main (main) where

import Data.Set (toList)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Hazel.Core (TBox (..))
import Hazel.Completion ( CGraph (..)
                        , complete
                        )
import Hazel.Wrapper (extractGCIs)
import Hazel.Normalize (normalize)
import Hazel.Parser (parseFile)

parse :: FilePath -> IO ()
parse file = do
  result <- parseFile file
  case result of
    (Left err) -> putStrLn $ "failed parsing `" ++ file ++ "': `" ++ err ++ "'."
    (Right ast) -> do
      putStrLn $ "successfully parsed `" ++ file ++ "'."
      let tbox@(TBox _ names _) = normalize $ extractGCIs ast
          cg = complete tbox
      print . getNodes cg $ head . toList $ names

tryParse :: String -> IO ()
tryParse file = do
  exists <- doesFileExist file
  if exists
    then parse file
    else putStrLn $ "No such file: `" ++ file ++ "'."

main :: IO ()
main = getArgs >>= mapM_ tryParse
