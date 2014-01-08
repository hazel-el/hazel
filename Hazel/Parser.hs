{-# LANGUAGE OverloadedStrings #-}
module Hazel.Parser ( ontologyDocument
                    , parseFile
                    ) where

import Hazel.Parser.OWL.Functional ( ontologyDocument
                                   , OntologyDocument (..)
                                   )

import Control.Exception (catch)
import Data.Conduit ( ($$)
                    , (=$)
                    , runResourceT
                    )
import Data.Conduit.Attoparsec ( sinkParser
                               , ParseError (..)
                               )
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.Text ( decode
                         , utf8
                         )

parseFile :: FilePath -> IO (Either String OntologyDocument)
parseFile file = parse `catch` err
  where parse = do
          result <- runResourceT $ sourceFile file
                    $$ decode utf8
                    =$ sinkParser ontologyDocument
          return . Right $ result
        err :: ParseError -> IO (Either String OntologyDocument)
        err e = return . Left $ errorMessage e
