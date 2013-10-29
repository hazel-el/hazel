-- TODO fill in header
{- |
 - Module      :  Hazel.Completion
 - Description :  Provides reasoning according to completion algorithm
 - Copyright   :  (c) <Authors or Affiliations>
 - License     :  <license>
 -
 - Maintainer  :  <email>
 - Stability   :  experimental
 - Portability :  portable | non-portable (<reason>)
 -
 - <module description starting at first column>
 - -}
module Hazel.Completion where

import Hazel.Core
-- import Data.Set


normalizeGCI :: GCI -> TBox
normalizeGCI gci = case gci of
    -- Rule NF2 in Pushing the EL Envelope
    Subclass (And c (And d1 d2)) e -> 
        (normalizeGCI $ Subclass (And d1 d2) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    Subclass (And c (Exists r d)) e -> 
        (normalizeGCI $ Subclass (Exists r d) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    -- Rule NF2 with the order of conjuncts flipped
    Subclass (And (And d1 d2) c) e -> 
        (normalizeGCI $ Subclass (And d1 d2) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    Subclass (And (Exists r d) c) e -> 
        (normalizeGCI $ Subclass (Exists r d) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    -- Rule NF3
    Subclass (Exists r (And c1 c2)) d ->
        (normalizeGCI $ Subclass (And c1 c2) toDummy) ++
        (normalizeGCI $ Subclass (Exists r toDummy) d)
    Subclass (Exists r (Exists r1 c1)) d ->
        (normalizeGCI $ Subclass (Exists r1 c1) toDummy) ++
        (normalizeGCI $ Subclass (Exists r toDummy) d)
    -- Rule NF6
    Subclass d (Exists r (And c1 c2)) ->
        (normalizeGCI $ Subclass toDummy (And c1 c2)) ++
        (normalizeGCI $ Subclass d (Exists r toDummy))
    Subclass c (Exists r (Exists s d)) ->
        (normalizeGCI $ Subclass c (Exists r toDummy)) ++
        (normalizeGCI $ Subclass toDummy (Exists s d))
    -- Rule NF5
    Subclass (And c d) e ->
        (normalizeGCI $ Subclass (And c d) toDummy) ++
        (normalizeGCI $ Subclass toDummy e)
    Subclass (Exists r c) d ->
        (normalizeGCI $ Subclass (Exists r c) toDummy) ++
        (normalizeGCI $ Subclass toDummy d)
    Subclass c (And d e) ->
        (normalizeGCI $ Subclass c toDummy) ++
        (normalizeGCI $ Subclass toDummy (And d e))
    Subclass c (Exists r d) ->
        (normalizeGCI $ Subclass c toDummy) ++
        (normalizeGCI $ Subclass toDummy (Exists r d))
    -- Rule NF7
    Subclass c (And d e) ->
        (normalizeGCI $ Subclass c d) ++
        (normalizeGCI $ Subclass c e)
    g ->
        [g]
  where
    toDummy = Name (show gci) False
