module Hazel.CompletionGraph
where

import Hazel.Core


--                  S mapping          R mapping
data CGraph =
        CGraph (Concept -> [Concept]) (Role -> [CEdge])
        
type CEdge = (Concept,Concept)
        
        
computeCGraph :: TBox -> CGraph
computeCGraph TBox g:gs cs rs
         = 
         




         
applyRules :: CGraph -> GCI -> CGraph
applyRules CGraph


cr1 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr2 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr3 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr4 :: GCI -> CGraph -> Concept -> (CGraph, Bool)

cr1 (Subclass c' d) (CGraph s r) c
        | ( c' `elem` s c ) && not (d `elem` s c) = (CGraph sneu r, True)
        | otherwise = (CGraph s r, False)
        where
                sneu c = d:(s c)
                sneu = s