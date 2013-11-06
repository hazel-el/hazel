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

import Hazel.Core

data CGraph =
    -- | Labelling function for nodes, and labelling function for edges
    CGraph (Concept -> [Concept]) (Role -> [CEdge])

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

initGraph :: CGraph
-- ^ Initialization of the completion graph
initGraph =
    CGraph s r
  where
    s Top = [Top]
    s (Name n) = [Name n, Top]
    s (Dummy n) = [Dummy n, Top]
    s _ = error "Complex concepts are not valid nodes in CGraph"
    r _ = []

-- Functions applying Completion Rules

cr1 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr2 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr3 :: GCI -> CGraph -> Concept -> (CGraph, Bool)
cr4 :: GCI -> CGraph -> Concept -> Concept -> (CGraph, Bool)

cr1 (Subclass c' d) (CGraph s r) c
    | (c' `elem` s c) && (d `notElem` s c) = (CGraph s' r, True)
    | otherwise = (CGraph s r, False)
  where
    s' = except s c (d:s c)

cr2 (Subclass (And c1 c2) d) (CGraph s r) c
    | (c1 `elem` s c) && (c2 `elem` s c) && (d `notElem` s c) =
        (CGraph s' r, True)
    | otherwise = (CGraph s r, False)
  where
    s' = except s c (d:s c)
cr2 _ _ _ = error "Application of Rule CR2 not possible"

cr3 (Subclass c' (Exists role d)) (CGraph s r) c
    | (c' `elem` s c) && ((c, d) `notElem` r role) =
        (CGraph s r', True)
    | otherwise = (CGraph s r, False)
  where
    r' = except r role ((c, d):r role)
cr3 _ _ _ = error "Application of Rule CR3 not possible"

cr4 (Subclass (Exists role d') e) (CGraph s r) c d
    | ((c, d) `elem` r role) && (d' `elem` s d) && (e `notElem` s c) =
        (CGraph s' r, True)
    | otherwise = (CGraph s r, False)
  where
    s' = except s c (e:s c)
cr4 _ _ _ _ = error "Application of Rule CR4 not possible"

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
