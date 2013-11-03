-- TODO fill in header
{- |
* Module      :  Hazel.Normalize

* Description :  Normalization of TBoxes as preprocessing for completion
algorithm

* Copyright   :  (c) <Authors or Affiliations>

* License     :  <license>

* Maintainer  :  <email>

* Stability   :  experimental

* Portability :  portable | non-portable (<reason>)

* <module description starting at first column>
-}
module Hazel.Normalize where

import Hazel.Core


normalizeGCI :: GCI -> TBox
-- ^ returns a TBox obtained by exhaustively applying normalization rules to
-- a GCI (according to /Pushing the EL Envelope/)
normalizeGCI gci = case gci of
    -- Rule NF2 in Pushing the EL Envelope
    Subclass (And c (And d1 d2)) e -> 
        (normalizeGCI $ Subclass (And d1 d2) dummy) `tBox_union`
        (normalizeGCI $ Subclass (And dummy c) e)
    Subclass (And c (Exists r d)) e -> 
        (normalizeGCI $ Subclass (Exists r d) dummy) `tBox_union`
        (normalizeGCI $ Subclass (And dummy c) e)
    -- Rule NF2 with the order of conjuncts flipped
    Subclass (And (And d1 d2) c) e -> 
        (normalizeGCI $ Subclass (And d1 d2) dummy) `tBox_union`
        (normalizeGCI $ Subclass (And dummy c) e)
    Subclass (And (Exists r d) c) e -> 
        (normalizeGCI $ Subclass (Exists r d) dummy) `tBox_union`
        (normalizeGCI $ Subclass (And dummy c) e)
    -- Rule NF3
    Subclass (Exists r (And c1 c2)) d ->
        (normalizeGCI $ Subclass (And c1 c2) dummy) `tBox_union`
        (normalizeGCI $ Subclass (Exists r dummy) d)
    Subclass (Exists r (Exists r1 c1)) d ->
        (normalizeGCI $ Subclass (Exists r1 c1) dummy) `tBox_union`
        (normalizeGCI $ Subclass (Exists r dummy) d)
    -- Rule NF6
    Subclass d (Exists r (And c1 c2)) ->
        (normalizeGCI $ Subclass dummy (And c1 c2)) `tBox_union`
        (normalizeGCI $ Subclass d (Exists r dummy))
    Subclass c (Exists r (Exists s d)) ->
        (normalizeGCI $ Subclass c (Exists r dummy)) `tBox_union`
        (normalizeGCI $ Subclass dummy (Exists s d))
    -- Rule NF5
    Subclass (And c d) (Exists re e) ->
        (normalizeGCI $ Subclass (And c d) dummy) `tBox_union`
        (normalizeGCI $ Subclass dummy (Exists re e))
    Subclass (Exists r c) (And d1 d2) ->
        (normalizeGCI $ Subclass (Exists r c) dummy) `tBox_union`
        (normalizeGCI $ Subclass dummy (And d1 d2))
    Subclass (And c1 c2) (And d e) ->
        (normalizeGCI $ Subclass (And c1 c2) dummy) `tBox_union`
        (normalizeGCI $ Subclass dummy (And d e))
    Subclass (Exists rc c) (Exists r d) ->
        (normalizeGCI $ Subclass (Exists rc c) dummy) `tBox_union`
        (normalizeGCI $ Subclass dummy (Exists r d))
    -- Rule NF7
    Subclass c (And d e) ->
        (normalizeGCI $ Subclass c d) `tBox_union`
        (normalizeGCI $ Subclass c e)
    -- remove tautologies:
    Subclass _ Top -> tBox_from_list []
    g ->
        tBox_from_list [g]
  where
    dummy = Dummy . T.pack . show $ gci

-- TODO unschön: wenn TBox der Typ links wäre würden wir die Signatur
-- rausschmeißen und neu berechnen, deshalb nur [GCI] links
--
normalize :: [GCI] -> TBox
-- ^ TBox obtained by normalizing all GCIs in a list
normalize = (foldl1 tBox_union) . (map normalizeGCI)
