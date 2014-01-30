module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Data.Set (toList)

import Hazel.Parser (parseFile)
import Hazel.Conversion (extractGCIs)
import Hazel.Core (TBox (..))
import Hazel.Normalize (normalize)
import Hazel.Completion ( complete
                        , CGraph (..)
                        )

parse :: FilePath -> IO ()
parse file = do
  result <- parseFile file
  case result of
    (Left err) -> putStrLn $ "failed parsing `" ++ file ++ "': `" ++ err ++ "'."
    (Right o) -> print $ zip ns $ map (getNodes cg) ns
      where
        ns = toList names
        t = normalize $ extractGCIs o
        TBox _ names _ = t
        cg = complete t

tryParse :: String -> IO ()
tryParse file = do
  exists <- doesFileExist file
  if exists
    then parse file
    else putStrLn $ "No such file: `" ++ file ++ "'."

main :: IO ()
main = getArgs >>= mapM_ tryParse
