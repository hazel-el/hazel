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
module Hazel.Core ( Role (..)
                  , Concept (..)
                  , GCI (..)
                  , TBox (..)
                  , gciToTBox
                  , gcisToTBox
                  )
       where

import Control.Arrow
import Data.Foldable
import Data.HashTable (hashString)
import Data.Monoid
import Data.Set hiding (map)
import qualified Data.Text as T


-- Datatypes --

newtype Role = Role T.Text
             deriving (Show, Eq, Ord)

data Concept = Top
             | Name T.Text
             | Dummy T.Text
             | And Concept Concept
             | Exists Role Concept
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
        Name s ->
            T.unpack s
        Dummy s ->
            show . hashString $ T.unpack s
        And c1 c2 ->
	    "(" ++ (show c1) ++ " and " ++ (show c2) ++ ")"
        Exists r c1 ->
            "(" ++ (show r) ++ " some " ++ (show c1) ++ ")"

instance Show GCI where
    show (Subclass c1 c2) =
        (show c1) ++ " SubClassOf " ++ (show c2)

instance Show TBox where
    show (TBox gs _ _) = show gs

instance Monoid TBox where
  mempty = TBox [] empty empty
  mappend (TBox gs sc sr) (TBox hs tc tr) = TBox (gs ++ hs) (sc `union` tc) (sr `union` tr)

-- Auxiliary functions

unionPair :: (Ord a, Ord b) => (Set a, Set b) -> (Set a, Set b) -> (Set a, Set b)
unionPair (a, b) = union a *** union b

conceptNames :: Concept -> (Set Concept, Set Role)
conceptNames Top = (empty, empty)
conceptNames (And c d) = conceptNames c `unionPair` conceptNames d
conceptNames (Exists r d) = conceptNames d `unionPair` (empty, singleton r)
conceptNames nd = (singleton nd, empty)

gciNames :: GCI -> (Set Concept, Set Role)
gciNames (Subclass c d) = conceptNames c `unionPair` conceptNames d

gciToTBox :: GCI -> TBox
gciToTBox g = TBox [g] gcs grs
  where (gcs, grs) = gciNames g

gcisToTBox :: [GCI] -> TBox
-- ^ Converts a list of GCIs to a TBox datastructure
gcisToTBox = foldMap gciToTBox
