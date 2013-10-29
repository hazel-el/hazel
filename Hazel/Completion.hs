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
import Data.HashTable (hashString)
-- import Data.Set


normalizeGCI :: GCI -> TBox
-- Rule NF2 in Pushing the EL Envelope
normalizeGCI gci = case gci of
    Subclass (And c (And d1 d2)) e -> 
        (normalizeGCI $ Subclass (And d1 d2) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    Subclass (And c (Exists r d)) e -> 
        (normalizeGCI $ Subclass (Exists r d) toDummy) ++
        (normalizeGCI $ Subclass (And toDummy c) e)
    g ->
        [g]
  where
    toDummy = Name ("C" ++ show (hashString $ show gci))")
