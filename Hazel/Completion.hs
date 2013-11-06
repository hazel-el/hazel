-- TODO fill in header
{- |
* Module      :  Hazel.Normalize

* Description :  Functions for reasoning according to completion algorithm

* Copyright   :  (c) <Authors or Affiliations>

* License     :  <license>

* Maintainer  :  <email>

* Stability   :  experimental

* Portability :  portable | non-portable (<reason>)

* <module description starting at first column>
-}

module Hazel.Completion
       where

import Data.List
import Hazel.Core

data CGraph =
    CGraph (Concept -> [Node]) (Role -> [CEdge])

type Node = Concept
type CEdge = (Concept, Concept)



-- Auxiliary functions

-- | Returns a function that is like f, except one argument gets a new value
except :: (Eq a) =>
          (a -> b) -- ^ original function
          -> a     -- ^ argument whose function value should be replaced
          -> b     -- ^ new value
          -> a -> b
except f x' y' x
  | x == x' = y'
  | otherwise = f x

initGraph :: [Node] -> CGraph
-- ^ Initialization of the completion graph
initGraph allNodes =
    CGraph n r
  where
    n Top = nub $ Top : allNodes
    n x@(Name _) = [x]
    n x@(Dummy _) = [x]
    n _ = error "Complex concepts are not valid nodes in CGraph"
    r _ = []

-- Functions applying Completion Rules

cr1 :: GCI -> CGraph -> Concept -> CGraph
cr2 :: GCI -> CGraph -> Concept -> CGraph
cr3 :: GCI -> CGraph -> Concept -> CGraph
cr4 :: GCI -> CGraph -> (Concept, Concept) -> CGraph

cr1 (Subclass c' d) (CGraph n r) c
    | (c `notElem` n d) && (c `elem` n c') = CGraph n' r
    | otherwise = CGraph n r
  where
    n' = except n d (c:n d)

cr2 (Subclass (And c1 c2) d) (CGraph n r) c
    | (c `elem` n c1) && (c `elem` n c2) && (c `notElem` n d) =
        CGraph n' r
    | otherwise = CGraph n r
  where
    n' = except n d (c:n d)
cr2 _ _ _ = error "Application of Rule CR2 not possible"

cr3 (Subclass c' (Exists role d)) (CGraph n r) c
    | (c `elem` n c') && ((c, d) `notElem` r role) =
        CGraph n r'
    | otherwise = CGraph n r
  where
    r' = except r role ((c, d):r role)
cr3 _ _ _ = error "Application of Rule CR3 not possible"

cr4 (Subclass (Exists role d') e) (CGraph n r) (c, d)
    | ((c, d) `elem` r role) && (d `elem` n d') && (c `notElem` n e) =
        CGraph n' r
    | otherwise = CGraph n r
  where
    n' = except n e (c:n e)
cr4 _ _ _ = error "Application of Rule CR4 not possible"


-- Functions ensuring the rules are applied exhaustively

--cr1 :: GCI -> CGraph -> Concept -> CGraph
--cr2 :: GCI -> CGraph -> Concept -> CGraph
--cr3 :: GCI -> CGraph -> Concept -> CGraph
--cr4 :: GCI -> CGraph -> (Concept, Concept) -> CGraph
--
--iterateNodes :: GCI -> CGraph -> CGraph
-----iterateNodes gci@(Subclass (Name c) (Name d)) cGraph = foldr (cr2 gci) cGraph allNodes
-----iterateNodes gci@(Subclass (Name c) (Name d)) cGraph = foldr (cr3 gci) cGraph allNodes
--iterateNodes gci@(Subclass (Exists role d') e) (CGraph s r) = foldr (cr4 gci) cGraph (r role)
--iterateNodes gci@(Subclass c d) cGraph = foldr (cr1 gci) cGraph allNodes

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
