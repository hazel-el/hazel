{- |
Module      :  Hazel.Normalize
Description :  Normalization of TBoxes as preprocessing for completion algorithm
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

TBox normalization for EL
-}
module Hazel.Normalize ( normalize
                       , normalizeGCI
                       )
       where

import Control.Arrow ( (***)
                     , (>>>)
                     )
import Data.Foldable (foldMap)
import Data.Monoid ( mempty
                   , (<>)
                   )
import Data.Text (pack)

import Hazel.Core

normalizeGCI :: GCI -> TBox
-- ^ returns a TBox obtained by exhaustively applying normalization rules to
-- a GCI (according to /Pushing the EL Envelope/)
normalizeGCI gci@(Subclass lhs rhs) =
    go lhs rhs
  where
    dummy = Dummy . pack . show $ gci
    -- Rule NF2 in Pushing the EL Envelope
    go (And c d@(And _ _)) e = (d, dummy) `normUnion` (And dummy c, e)
    go (And c d@(Exists _ _)) e = (d, dummy) `normUnion` (And dummy c, e)
    -- Rule NF2 with the order of conjuncts flipped
    go (And d@(And _ _) c) e = (d, dummy) `normUnion` (And dummy c, e)
    go (And d@(Exists _ _) c) e = (d, dummy) `normUnion` (And dummy c, e)
    -- Rule NF3
    go (Exists r c@(And _ _)) d = (c, dummy) `normUnion` (Exists r dummy, d)
    go (Exists r c@(Exists _ _)) d = (c, dummy) `normUnion` (Exists r dummy, d)
    -- Rule NF6
    go d (Exists r c@(And _ _)) = (dummy, c) `normUnion` (d, Exists r dummy)
    go c (Exists r d@(Exists _ _)) = (c, Exists r dummy) `normUnion` (dummy, d)
    -- Rule NF5
    go (And _ _) (Exists _ _) = (lhs, dummy) `normUnion` (dummy, rhs)
    go (Exists _ _) (And _ _) = (lhs, dummy) `normUnion` (dummy, rhs)
    go (And _ _) (And _ _) = (lhs, dummy) `normUnion` (dummy, rhs)
    go (Exists _ _) (Exists _ _) = (lhs, dummy) `normUnion` (dummy, rhs)
    -- Rule NF7
    go c (And d e) = (c, d) `normUnion` (c, e)
    -- remove tautologies
    go _ Top = mempty
    go _ _ = gciToTBox gci

normUnion :: (Concept, Concept) -> (Concept, Concept) -> TBox
-- ^ Normalizes two GCIs (represented in pair form) and returns the union of
-- the resulting TBoxes
normUnion =
    curry $ normalizeConcept *** normalizeConcept >>> uncurry (<>)
  where
    normalizeConcept = normalizeGCI . uncurry Subclass

-- TODO unschön: wenn TBox der Typ links wäre würden wir die Signatur
-- rausschmeißen und neu berechnen, deshalb nur [GCI] links
--
normalize :: [GCI] -> TBox
-- ^ TBox obtained by normalizing all GCIs in a list
normalize = foldMap normalizeGCI
