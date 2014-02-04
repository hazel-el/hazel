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

import Prelude hiding ( null )

import Control.Monad.State.Lazy
import Data.List ( intersect
                 , nub
                 )
import Data.Set (elems)
import Hazel.Core
import Data.HashMap.Strict ( HashMap
                           , empty
                           , lookupDefault
                           , null
                           , singleton
                           , union
                           )


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
-- extractHierarchy :: CGraph -> CHierarchy
-- extractHierarchy = undefined


type Node = Concept
type CEdge = (Concept, Concept)

data CState = CState { newNodes :: HashMap Concept [Node]
                     , roleChange :: Bool
                     }
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

addLabel :: Node -> Concept -> State CState ()
addLabel node c = do
    CState nn rc <- get
    let oldNodes = lookupDefault [] c nn
    put $ CState (singleton c (node:oldNodes) `union` nn) rc

cr1 (Subclass c' d) (CGraph n r) c
    | (c `notElem` n d) && (c `elem` n c') = do
        addLabel c d
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n d (c:n d)

cr2 (Subclass (And c1 c2) d) (CGraph n r) c
    | (c `elem` n c1) && (c `elem` n c2) && (c `notElem` n d) = do
        addLabel c d
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n d (c:n d)
cr2 _ _ _ = error "Application of Rule CR2 not possible"

cr3 (Subclass c' (Exists role d)) (CGraph n r) c
    | (c `elem` n c') && ((c, d) `notElem` r role) = do
        CState nn _ <- get
        put $ CState nn True
        return $ CGraph n r'
    | otherwise = return $ CGraph n r
  where
    r' = except r role ((c, d):r role)
cr3 _ _ _ = error "Application of Rule CR3 not possible"

cr4 (Subclass (Exists role d') e) (CGraph n r) (c, d)
    | ((c, d) `elem` r role) && (d `elem` n d') && (c `notElem` n e) = do
        addLabel c e
        return $ CGraph n' r
    | otherwise = return $ CGraph n r
  where
    n' = except n e (c:n e)
cr4 _ _ _ = error "Application of Rule CR4 not possible"


-- Functions ensuring the rules are applied exhaustively

emptyState :: CState
emptyState = CState empty False

complete :: TBox -> CGraph
complete (TBox gcis cs _) =
    go True empty $ initGraph $ elems cs
  where
    go :: Bool -> HashMap Concept [Node] -> CGraph -> CGraph
    go first nn graph
        | null nn' && not rc' = graph'
        | otherwise           = go False nn' graph'
      where
        (graph', CState nn' rc') =
            runState (iterateGCI first nn graph gcis) emptyState

iterateNodes :: Bool -> HashMap Concept [Node] -> CGraph -> GCI -> Completion
iterateNodes first nn cG@(CGraph n r) gci = case gci of
    (Subclass (And c d) _)       ->
        foldM (cr2 gci) cG $ newlyEither c d
    (Subclass c' (Exists _ _))   -> foldM (cr3 gci) cG $ newly c'
    (Subclass (Exists role _) _) -> foldM (cr4 gci) cG $ r role
    (Subclass c' _)              -> foldM (cr1 gci) cG $ newly c'
  where
    newly c' | first     = n c'
             | otherwise = n c' `intersect` lookupDefault [] c' nn
    newlyEither c d
             | first     = n c `intersect` n d
             | otherwise = n c `intersect` n d `intersect`
                           (lookupDefault [] c nn ++ lookupDefault [] d nn)

iterateGCI :: Bool -> HashMap Concept [Node] -> CGraph -> [GCI] -> Completion
iterateGCI first nn = foldM (iterateNodes first nn)
