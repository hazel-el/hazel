{- |
Module      :  Hazel.Normalize
Description :  Functions for reasoning according to completion algorithm
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Completion Algorithm for EL
-}

module Hazel.Completion
       where

import Control.Monad.State.Lazy
import Data.List
import Data.Set (elems)
import Hazel.Core


-- data structure for completion graph
-- getNodes models the (inverse) function S from completion algorithm
-- getRoles models the (inverse) function R from completion algorithm
data CGraph = CGraph { getNodes :: Concept -> [Node]
                     , getRoles :: Role -> [CEdge]
                     }

-- data structure for factorized neighborhood of concepts
-- according to the PhD thesis of Dr. Suntisrivaraporn (on page 69)
data CNeighbors = CNeighbors { getUpper :: Concept -> [Concept]
			     , getEquivalent :: Concept -> [Concept]
			     , getLower :: Concept -> [Concept]
			     }

-- function to extract the concept hierarchy from the completion graph
-- according to the PhD thesis of Dr. Suntisrivaraporn (on page 69)
-- this just computes the reflexive-transitive reduction
-- of the factorization w.r.t. concept equivalence
extractHierarchy :: CGraph -> CHierarchy
extractHierarchy = undefined


type Node = Concept
type CEdge = (Concept, Concept)

data CState = CState [Node] [CEdge]
              deriving (Eq)
type Completion = State CState CGraph


-- Auxiliary functions

-- | Returns a function that is like f, except one argument gets a new value
except :: (Eq a)
          => (a -> b) -- ^ original function
          -> a        -- ^ argument whose function value should be replaced
          -> b        -- ^ new value
          -> a -> b
except f x' y' x
    | x == x'   = y'
    | otherwise = f x

initGraph :: [Node] -> CGraph
-- ^ Initialization of the completion graph
initGraph allNodes =
    CGraph n r
  where
    n Top         = nub $ Top : allNodes
    n x@(Name _)  = [x]
    n x@(Dummy _) = [x]
    n _           = error "Complex concepts are not valid nodes in CGraph"
    r _           = []


-- Functions applying Completion Rules

cr1 :: GCI -> CGraph -> Concept -> Completion
cr2 :: GCI -> CGraph -> Concept -> Completion
cr3 :: GCI -> CGraph -> Concept -> Completion
cr4 :: GCI -> CGraph -> (Concept, Concept) -> Completion

changeNode :: Node -> State CState ()
changeNode node = do
    CState ns es <- get
    put $ CState (node:ns) es

cr1 (Subclass c' d) (CGraph n r) c
    | (c `notElem` n d) && (c `elem` n c') = do
        changeNode c
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n d (c:n d)

cr2 (Subclass (And c1 c2) d) (CGraph n r) c
    | (c `elem` n c1) && (c `elem` n c2) && (c `notElem` n d) = do
        changeNode c
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n d (c:n d)
cr2 _ _ _ = error "Application of Rule CR2 not possible"

cr3 (Subclass c' (Exists role d)) (CGraph n r) c
    | (c `elem` n c') && ((c, d) `notElem` r role) = do
        CState ns es <- get
        put $ CState ns ((c, d):es)
        return $ CGraph n r'
    | otherwise = return $ CGraph n r
  where
    r' = except r role ((c, d):r role)
cr3 _ _ _ = error "Application of Rule CR3 not possible"

cr4 (Subclass (Exists role d') e) (CGraph n r) (c, d)
    | ((c, d) `elem` r role) && (d `elem` n d') && (c `notElem` n e) = do
        changeNode c
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n e (c:n e)
cr4 _ _ _ = error "Application of Rule CR4 not possible"


-- Functions ensuring the rules are applied exhaustively

emptyState :: CState
emptyState = CState [] []

complete :: TBox -> CGraph
complete (TBox gcis cs _) =
    go . initGraph $ elems cs
  where
    go :: CGraph -> CGraph
    go graph
        | state' == emptyState = graph'
        | otherwise            = go graph'
      where
        (graph', state') = runState (iterateGCI graph gcis) emptyState

iterateNodes :: CGraph -> GCI -> Completion
iterateNodes cG@(CGraph n r) gci = case gci of
    (Subclass (And c d) _)       -> foldM (cr2 gci) cG (n c `intersect` n d)
    (Subclass c' (Exists _ _))   -> foldM (cr3 gci) cG (n c')
    (Subclass (Exists role _) _) -> foldM (cr4 gci) cG (r role)
    (Subclass c' _)              -> foldM (cr1 gci) cG (n c')

iterateGCI :: CGraph -> [GCI] -> Completion
iterateGCI = foldM iterateNodes
