module Hazel.TBoxer where

import Hazel.Parser
import Hazel.Parser.OWL.Functional
import Hazel.Core
import Data.Set

toTBox :: OntologyDocument -> TBox
toTBox (OntologyDocument _ (Ontology _ _ _ axioms)) = TBox gcis (extractConceptNames gcis) (extractRoleNames gcis)
	where gcis = Prelude.map catg (extractClassAxioms axioms)

extractConceptNames :: [GCI] -> Set Concept
extractConceptNames (gci:gcis) = undefined

extractRoleNames :: [GCI] -> Set Role
extractRoleNames (gci:gcis) = undefined

extractClassAxioms :: [Axiom] -> [ClassAxiom]
extractClassAxioms [] = []
extractClassAxioms ((AxiomClass axiom):axioms) = axiom:(extractClassAxioms axioms)
extractClassAxioms (_:axioms) = extractClassAxioms axioms

catg :: ClassAxiom -> GCI
catg (SubClassOf _ (SubClassExpression c) (SuperClassExpression d)) = Subclass (foo c) (foo d)
catg _ = error "no subClassOf axiom"

foo :: ClassExpression -> Concept
foo (ObjectIntersectionOf c d []) = And (foo c) (foo d)
foo (ObjectIntersectionOf c d es) = foo2 (c:d:es)
foo (ObjectSomeValuesFrom r c) = Exists (bar r) (foo c)
foo _ = error "unsupported class expression"

foo2 :: [ClassExpression] -> Concept
foo2 [] = Top
foo2 (c:[]) = undefined
foo2 (c:ds) = And (foo c) (foo2 ds)

bar :: ObjectPropertyExpression -> Role
bar (Left (ObjectProperty r)) = Role r
bar _ = error "unsupported object property expression" 
