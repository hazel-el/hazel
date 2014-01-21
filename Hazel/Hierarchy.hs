{- |
Module      :  Hazel.Hierarchy
Description :  Methods for extracting the concept hierarchy from completion graph
License     :  GPL3
Stability   :  experimental
Portability :  unknown

Concept Hierarchy Extraction for EL
-}

module Hazel.Hierarchy where

import Data.List
import Hazel.Core
import Hazel.Completion
import Hazel.Util

-- data structure for factorized neighborhood of concepts
-- according to the PhD thesis of Dr. Suntisrivaraporn (on page 69)
data Hierarchy = Hierarchy { parents      :: Concept -> [Concept]
                           , equivalents  :: Concept -> [Concept]
                           , domain       :: [Concept]
                           }

data CGraph2 = CGraph2 { getNodes    :: Concept -> [Node]
                       , getRoles    :: Role -> [CEdge]
                       , getConcepts :: [Concept]
                       }

-- function to extract the concept hierarchy from the completion graph
-- according to the PhD thesis of Dr. Suntisrivaraporn (on page 69)
-- this just computes the reflexive-transitive reduction
-- of the factorization w.r.t. concept equivalence
extractH   :: CGraph2 -> Hierarchy

initH      :: Hierarchy

classifyH  :: CGraph2
           -> Hierarchy
           -> Concept -> Hierarchy

insertH    :: Hierarchy
           -> Concept -> [Concept]
           -> Hierarchy

-- should be okay, TEST IT!!
extractH cgraph@(CGraph2 _ _ concepts) = foldl (classifyH cgraph) initH concepts

-- trivial
initH = Hierarchy (\c -> []) (\c -> []) []

-- maybe here is something missing
-- insert must be called from within
classifyH cgraph hierarchy@(Hierarchy pa eq cl) concept 
	| elem concept cl = hierarchy
	| otherwise       = insertH (insertEquivalents (foldl (classifyH cgraph) hierarchy candidates) concept equivalents) concept candidates
				where (equivalents,candidates) = partitionSubsumers cgraph concept
-- should be okay
insertH (Hierarchy pa eq cl) concept candidates =
        Hierarchy pa' eq (concept:cl)
		where pa' = except pa concept parents
			where parents = filter noParent candidates
				where noParent :: Concept -> Bool
				      noParent c = elem c (concat (map pa candidates)) 

-- inserts new equivalents for a concept
insertEquivalents :: Hierarchy -> Concept -> [Concept] -> Hierarchy
insertEquivalents (Hierarchy parents equivalents domain) concept equivalents' =
	Hierarchy parents (extend equivalents concept equivalents') domain

-- inserts new parents for a concept
insertParents :: Hierarchy -> Concept -> [Concept] -> Hierarchy
insertParents (Hierarchy parents equivalents domain) concept parents' =
	Hierarchy (extend parents concept parents') equivalents domain

-- should be okay
-- nts means non trivial subsumers
partitionSubsumers :: CGraph2 -> Concept -> ([Concept],[Concept])
partitionSubsumers (CGraph2 n _ d) concept = 
	( intersect nts (n concept)
	, (\\) nts (n concept)
	)
	where nts = (filter ((flip elem) [concept,Top]) (inv d n concept))
