-- TODO fill in header
{- |
 - Module      :  Hazel.Completion
 - Description :  Provides reasoning according to completion algorithm
 - Copyright   :  (c) <Authors or Affiliations>
 - License     :  <license>
 -
 - Maintainer  :  <email>
 - Stability   :  experimental
 - Portability :  portable | non-portable (<reason>)
 -
 - <module description starting at first column>
 - -}
module Hazel.Completion
where

import Hazel.Core


data CGraph =
    -- |   S mapping              R mapping
    CGraph (Concept -> [Concept]) (Role -> [CEdge])
        
type CEdge = (Concept,Concept)
        
        

-- Auxiliary functions

-- | Returns a function that is like f, except that x is mapped to yn
except :: (Eq a) => (a -> b) -> a -> b -> (a -> b)
except f x yn =
    fn
  where
    fn o
        | o == x = yn
        | otherwise = f o

init_graph =
    CGraph s r
  where
    s Top = [Top]
    s (Name n f) = [Name n f, Top]
    r _ = []


-- Functions applying Completion Rules

cr1 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr2 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr3 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr4 :: GCI -> CGraph -> Concept -> Concept -> (CGraph, Bool)

cr1 (Subclass c' d) (CGraph s r) c
    | (c' `elem` s c) && not (d `elem` s c) = (CGraph sneu r, True)
    | otherwise = (CGraph s r, False)
  where
    sneu = except s c (d:s c)

cr2 (Subclass (And c1 c2) d) (CGraph s r) c
    | (c1 `elem` s c) && (c2 `elem` s c) && not (d `elem` s c) =
        (CGraph sneu r, True)
    | otherwise = (CGraph s r, False)
  where
    sneu = except s c (d:s c)

cr3 (Subclass c' (Exists role d)) (CGraph s r) c
    | (c' `elem` s c) && not ((c, d) `elem` r role) =
        (CGraph s rneu, True)
    | otherwise = (CGraph s r, False)
  where
    rneu = except r role ((c, d):r role) 

cr4 (Subclass (Exists role d') e) (CGraph s r) c d
    | ((c, d) `elem` r role) && (d' `elem` s d) && not (e `elem` s c) =
        (CGraph sneu r, True)
    | otherwise = (CGraph s r, False)
  where
    sneu = except s c (e:s c)


-- Functions ensuring the rules are applied exhaustively

--computeCGraph :: TBox -> CGraph
--computeCGraph TBox g:gs cs rs
--         = 
--         
--
--
--
--
--         
--applyRules :: CGraph -> GCI -> CGraph
--applyRules CGraph
