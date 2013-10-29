-- TODO fill in header
{- |
 - Module      :  Hazel.Core
 - Description :  Provides core functionality for EL Reasoner "Hazel"
 - Copyright   :  (c) <Authors or Affiliations>
 - License     :  <license>
 -
 - Maintainer  :  <email>
 - Stability   :  experimental
 - Portability :  portable | non-portable (<reason>)
 -
 - <module description starting at first column>
 - -}
module Hazel.Core(
    Role (..),
    Concept (..),
    GCI (..),
    TBox
) where

import Data.HashTable (hashString)
-- import Data.Set


-- Datatypes --

data Role =
    Role String

data Concept =
    Top |
    Name String Bool | -- Bool flag is true if it's not a dummy
    And Concept Concept |
    Exists Role Concept

data GCI =
    Subclass Concept Concept

type TBox =
    [GCI]


-- show functions defined according to Manchester OWL Syntax used by Protege:

instance Show Role where
    show (Role s) = s

instance Show Concept where
    show c = case c of
        Top ->
            "Thing"  
        Name s True ->
            s
        Name s False ->
            show $ hashString s
        And c1 c2 ->
	    "(" ++ (show c1) ++ " and " ++ (show c2) ++ ")"
        Exists r c1 ->
            "(" ++ (show r) ++ " some " ++ (show c1) ++ ")"

instance Show GCI where
    show (Subclass c1 c2) =
        (show c1) ++ " SubClassOf " ++ (show c2)
