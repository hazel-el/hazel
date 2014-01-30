{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
Module      :  Hazel.Parser
Copyright   :  (c) 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Front-end module for the OWL2 Functional-Style parser
-}
module Hazel.Parser ( ontologyDocument
                    , parseFile
                    , module Hazel.Parser.OWL.AST
                    ) where

import Prelude hiding (catch)

import Hazel.Parser.OWL.AST
import Hazel.Parser.OWL.Functional (ontologyDocument)

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
