{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.RFC5234
Copyright   :  (c) 2013, 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser for the parts of RFC 5234 referenced in the OWL2 Functional-Style grammar.
-}

module Hazel.Parser.OWL.RFC5234 ( alpha
                                , hexDig
                                ) where

import Data.Char (isHexDigit)
import Data.Attoparsec.Text ( Parser
                            , satisfy
                            )

-- | Parser for ALPHA
alpha :: Parser Char
alpha = satisfy isAlpha
  where isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

-- | Parser for HEXDIG
hexDig :: Parser Char
hexDig = satisfy isHexDigit
