{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.RFC5234
Copyright   :  (c) 2013 Maximilian Marx
License     :  GPL3
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
                            , inClass
                            )

-- | Parser for ALPHA
alpha :: Parser Char
alpha = satisfy $ inClass "a-zA-Z"

-- | Parser for HEXDIG
hexDig :: Parser Char
hexDig = satisfy isHexDigit
