{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.SPARQL
Copyright   :  (c) 2013 Maximilian Marx
License     :  GPL3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser for the parts from SPARQL referenced in the OWL2 Functional-Style grammar.
-}
module Hazel.Parser.OWL.SPARQL ( blankNodeLabel
                               , pnameLN
                               , pnameNS
                               ) where

import Prelude hiding (takeWhile)
import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Data.Monoid (mappend)
import Data.Text ( Text
                 , cons
                 )
import qualified Data.Text as T
import Data.Attoparsec.Text

pnCharsBase :: Parser Char
pnCharsBase = satisfy isBaseChar

isBaseChar :: Char -> Bool
isBaseChar = inClass $ concat [ "A-Z"
                              , "a-z"
                              , "\x00C0-\x00D6"
                              , "\x00D8-\x00F6"
                              , "\x00F8-\x02FF"
                              , "\x0370-\x037D"
                              , "\x037F-\x1FFF"
                              , "\x200C-\x200D"
                              , "\x2070-\x218F"
                              , "\x2C00-\x2FEF"
                              , "\x3001-\xD7FF"
                              , "\xF900-\xFDCF"
                              , "\xFDF0-\xFFFD"
                              , "\x10000-\xEFFFF"
                              ]

pnCharsU :: Parser Char
pnCharsU = satisfy isCharU

isCharU :: Char -> Bool
isCharU '_' = True
isCharU c = isBaseChar c

isChar :: Char -> Bool
isChar c = isCharU c || inClass chars c
  where chars = concat [ "-"
                       , "0-9"
                       , "\x00B7"
                       , "\x0300-\x036F"
                       , "\x203F-\x2040"
                       ]

dottedChars :: Parser Text
dottedChars = do
  cs <- takeWhile1 isCharOrDot
  if T.last cs == '.'
     then fail "dottedChars: ended on a dot."
    else return cs
  where isCharOrDot '.' = True
        isCharOrDot c = isChar c

pnPrefix :: Parser Text
pnPrefix = cons <$> pnCharsBase <*> option "" dottedChars

pnLocal :: Parser Text
pnLocal = cons <$> (pnCharsU <|> digit) <*> option "" dottedChars

-- | Parser for PNAME_LN
pnameLN :: Parser Text
pnameLN = mappend <$> pnameNS <*> pnLocal

-- | Parser for PNAME_NS
pnameNS :: Parser Text
pnameNS = mappend <$> option "" pnPrefix <*> string ":"

-- | Parser for BLANK_NODE_LABEL
blankNodeLabel :: Parser Text
blankNodeLabel = mappend <$> string "_:" <*> pnLocal
