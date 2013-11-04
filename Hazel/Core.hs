-- TODO fill in header
{- |
* Module      :  Hazel.Core

* Description :  Provides core functionality for EL Reasoner "Hazel"

* Copyright   :  (c) <Authors or Affiliations>

* License     :  <license>

* Maintainer  :  <email>

* Stability   :  experimental

* Portability :  portable | non-portable (<reason>)

* <module description starting at first column>
-}
module Hazel.Core(
    Role (..),
    Concept (..),
    GCI (..),
    TBox (..),
    tBoxFromList,
    tBoxUnion
) where

import Data.HashTable (hashString)
import Data.Set


-- Datatypes --

data Role =
    Role String
    deriving (Eq, Ord)
    
data Concept =
    Top |
    Name String Bool | -- ^ Bool flag is true if it's not a dummy
    And Concept Concept |
    Exists Role Concept
    deriving (Eq, Ord)

data GCI =
    Subclass Concept Concept

data TBox =
    -- | Stores the GCIs and the signature (concept names and role names)
    TBox [GCI] (Set Concept) (Set Role) 


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
	    "(" ++ show c1 ++ " and " ++ show c2 ++ ")"
        Exists r c1 ->
            "(" ++ show r ++ " some " ++ show c1 ++ ")"

instance Show GCI where
    show (Subclass c1 c2) =
        show c1 ++ " SubClassOf " ++ show c2

instance Show TBox where
    show (TBox gs _ _) = show gs


-- Auxiliary functions

pairUnion :: Ord a => Ord b => (Set a, Set b) -> (Set a, Set b) -> (Set a, Set b)
pairUnion (sx, sy) (tx, ty) =
    (sx `union` tx, sy `union` ty)


getNames :: Concept -> (Set Concept, Set Role)
getNames c = case c of
    Top ->
        (empty, empty)
    And c d ->
        getNames c `pairUnion` getNames d
    Exists r d ->
        getNames d `pairUnion` (empty, singleton r)
    Name s b -> (singleton (Name s b), empty)


getNamesGCI :: GCI -> (Set Concept, Set Role)
getNamesGCI (Subclass c d) =
    getNames c `pairUnion` getNames d


addGCI :: TBox -> GCI -> TBox
addGCI (TBox gs cs rs) g =
    TBox (g:gs) (cs `union` gcs) (rs `union` grs)
  where
    (gcs, grs) = getNamesGCI g


tBoxFromList :: [GCI] -> TBox
-- ^ Converts a list of GCIs to a TBox datastructure
tBoxFromList [] = TBox [] empty empty
tBoxFromList (g:gt) =
    addGCI (tBoxFromList gt) g


tBoxUnion :: TBox -> TBox -> TBox
-- ^ returns the union of two TBoxes
tBoxUnion (TBox gs sc sr) (TBox hs tc tr) =
    TBox (gs ++ hs) (sc `union` tc) (sr `union` tr)
