{- |
Module      :  Hazel.Core
Description :  Provides core functionality for EL Reasoner "Hazel"
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Hazel core
-}
module Hazel.Core ( Role (..)
                  , Concept (..)
                  , GCI (..)
                  , TBox (..)
                  , gciToTBox
                  , gcisToTBox
                  , gciNames
                  )
       where

import Control.Arrow ((***))
import Data.Foldable (foldMap)
import Data.Hashable (hash)
import Data.Monoid ( Monoid (..)
                   , mempty
                   )
import Data.Set ( Set
                , empty
                , singleton
                , union
                )
import Data.Text ( Text
                 , unpack
                 )

-- Datatypes
newtype Role = Role Text
             deriving (Eq, Ord)

data Concept = Top
             | Name Text
             | Dummy Text
             | And Concept Concept
             | Exists Role Concept
             deriving (Eq, Ord)

data GCI = Subclass Concept Concept
           deriving (Eq)

data TBox =
    -- | Stores the GCIs and the signature (concept names and role names)
    TBox [GCI] (Set Concept) (Set Role)
    deriving (Eq)


-- show functions defined according to Manchester OWL Syntax used by Protege:
instance Show Role where
    show (Role r) = unpack r

instance Show Concept where
    show Top          = "Thing"
    show (Name s)     = unpack s
    show (Dummy s)    = show . hash . unpack $ s
    show (And c1 c2)  = concat ["(", show c1, " and ", show c2, ")"]
    show (Exists r c) = concat ["(", show r, " some ", show c, ")"]

instance Show GCI where
    show (Subclass c1 c2) = concat [show c1, " SubClassOf ", show c2]

instance Show TBox where
    show (TBox gs _ _) = show gs

instance Monoid TBox where
    mempty = TBox [] empty empty
    mappend (TBox gs sc sr) (TBox hs tc tr) = TBox (gs ++ hs) (sc `union` tc) (sr `union` tr)


-- Auxiliary functions

unionPair :: (Ord a, Ord b) => (Set a, Set b) -> (Set a, Set b) -> (Set a, Set b)
unionPair (a, b) = union a *** union b

conceptNames :: Concept -> (Set Concept, Set Role)
conceptNames Top          = (empty, empty)
conceptNames (And c d)    = conceptNames c `unionPair` conceptNames d
conceptNames (Exists r d) = conceptNames d `unionPair` (empty, singleton r)
conceptNames nd           = (singleton nd, empty)

gciNames :: GCI -> (Set Concept, Set Role)
gciNames (Subclass c d) = conceptNames c `unionPair` conceptNames d

gciToTBox :: GCI -> TBox
gciToTBox g =
    TBox [g] gcs grs
  where
    (gcs, grs) = gciNames g

gcisToTBox :: [GCI] -> TBox
-- ^ Converts a list of GCIs to a TBox datastructure
gcisToTBox = foldMap gciToTBox
