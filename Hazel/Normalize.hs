-- TODO fill in header
{- |
* Module      :  Hazel.Normalize

* Description :  Normalization of TBoxes as preprocessing for completion algorithm

* Copyright   :  (c) <Authors or Affiliations>

* License     :  <license>

* Maintainer  :  <email>

* Stability   :  experimental

* Portability :  portable | non-portable (<reason>)

* <module description starting at first column>
-}
module Hazel.Normalize ( normalize
                       , normalizeGCI)
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
normalizeGCI gci@(Subclass lhs rhs) = go lhs rhs
  where dummy = Dummy . pack . show $ gci
        -- Rule NF2 in Pushing the EL Envelope
        go (And c d@(And _ _)) e = (d, dummy) `union` (And dummy c, e)
        go (And c d@(Exists _ _)) e = (d, dummy) `union` (And dummy c, e)
        -- Rule NF2 with the order of conjuncts flipped
        go (And d@(And _ _) c) e = (d, dummy) `union` (And dummy c, e)
        go (And d@(Exists _ _) c) e = (d, dummy) `union` (And dummy c, e)
        -- Rule NF3
        go (Exists r c@(And _ _)) d = (c, dummy) `union` (Exists r dummy, d)
        go (Exists r c@(Exists _ _)) d = (c, dummy) `union` (Exists r dummy, d)
        -- Rule NF6
        go d (Exists r c@(And _ _)) = (dummy, c) `union` (d, Exists r dummy)
        go c (Exists r d@(Exists _ _)) = (c, Exists r dummy) `union` (dummy, d)
        -- Rule NF5
        go (And _ _) (Exists _ _) = (lhs, dummy) `union` (dummy, rhs)
        go (Exists _ _) (And _ _) = (lhs, dummy) `union` (dummy, rhs)
        go (And _ _) (And _ _) = (lhs, dummy) `union` (dummy, rhs)
        go (Exists _ _) (Exists _ _) = (lhs, dummy) `union` (dummy, rhs)
        -- Rule NF7
        go c (And d e) = (c, d) `union` (c, e)
        -- remove tautologies
        go _ Top = mempty
        go _ _ = gciToTBox gci

union :: (Concept, Concept) -> (Concept, Concept) -> TBox
union = curry $ normalizeConcept *** normalizeConcept >>> uncurry (<>)
  where normalizeConcept = normalizeGCI . uncurry Subclass

-- TODO unschön: wenn TBox der Typ links wäre würden wir die Signatur
-- rausschmeißen und neu berechnen, deshalb nur [GCI] links
--
normalize :: [GCI] -> TBox
-- ^ TBox obtained by normalizing all GCIs in a list
normalize = foldMap normalizeGCI
