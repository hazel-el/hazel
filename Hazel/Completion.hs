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
    g ->
        [g]
  where
    toDummy = Name (show gci) False
