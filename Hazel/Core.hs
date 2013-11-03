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
    tBox_from_list,
    tBox_union
) where

import Data.HashTable (hashString)
import Data.Set


-- Datatypes --

newtype Role = Role String
             deriving (Show, Eq, Ord)

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

instance Show TBox where
    show (TBox gs _ _) = show gs


-- Auxiliary functions

pair_union :: Ord a => Ord b => (Set a, Set b) -> (Set a, Set b) -> (Set a, Set b)
pair_union (sx, sy) (tx, ty) =
    (sx `union` tx, sy `union` ty)


get_names :: Concept -> (Set Concept, Set Role)
get_names c = case c of
    Top ->
        (empty, empty)
    And c d ->
        get_names c `pair_union` get_names d
    Exists r d ->
        get_names d `pair_union` (empty, singleton r)
    Name s b -> (singleton (Name s b), empty)


get_names_gci :: GCI -> (Set Concept, Set Role)
get_names_gci (Subclass c d) =
    get_names c `pair_union` get_names d


add_GCI :: TBox -> GCI -> TBox
add_GCI (TBox gs cs rs) g =
    TBox (g:gs) (cs `union` gcs) (rs `union` grs)
  where
    (gcs, grs) = get_names_gci g


tBox_from_list :: [GCI] -> TBox
-- ^ Converts a list of GCIs to a TBox datastructure
tBox_from_list [] = TBox [] empty empty
tBox_from_list (g:gt) =
    add_GCI (tBox_from_list gt) g


tBox_union :: TBox -> TBox -> TBox
-- ^ returns the union of two TBoxes
tBox_union (TBox gs sc sr) (TBox hs tc tr) =
    TBox (gs ++ hs) (sc `union` tc) (sr `union` tr)
