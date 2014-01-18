{- |
Module      :  Hazel.Conversion
Description :  Converts between parser output and core datastructures
License     :  GPL-3
Stability   :  experimental
Portability :  unknown

Hazel conversion
-}

module Hazel.Conversion ( extractGCIs )
       where

import Hazel.Core ( GCI )
import Hazel.Parser.OWL.Functional

convertObjectProperty :: ObjectPropertyExpression -> Role
convertObjectProperty Left "owl:bottomObjectProperty"
    = error "owl:bottomObjectProperty not yet supported"
convertObjectProperty Left "owl:topObjectProperty"
    = error "owl:topObjectProperty not yet supported"
convertObjectProperty Left s
    = Role s
convertObjectProperty Right _
    = error "InverseObjectProperty not part of OWL 2 EL specs"

convertClass :: ClassExpression -> Concept
convertClass c = case c of
    Class' "owl:Thing"       -> Top
    Class' "owl:Nothing"     -> notSupported "owl:Nothing"
    Class' s                 -> Name s
    ObjectIntersectionOf c d -> foldr (And . convertClass) (c' `And` d')
    ObjectUnionOf{}          -> notEL "ObjectUnionOf"
    ObjectComplementOf _     -> notEL "ObjectComplementOf"
    ObjectOneOf{}            -> notSupported "ObjectOneOf"
    ObjectSomeValuesFrom r c -> Exists r' c'
    ObjectAllValuesFrom{}    -> notEL "ObjectAllValuesFrom"
    ObjectHasValue{}         -> notSupported "ObjectHasValue"
    ObjectHasSelf _          -> notSupported "ObjectHasSelf"
    ObjectMinCardinality{}   -> notEL "ObjectMinCardinality"
    ObjectMaxCardinality{}   -> notEL "ObjectMaxCardinality"
    ObjectExactCardinality{} -> notEL "ObjectExactCardinality"
    DataSomeValuesFrom{}     -> notSupported "DataSomeValuesFrom"
    DataAllValuesFrom{}      -> notEL "DataAllValuesFrom"
    DataHasValue{}           -> notSupported "DataHasValue"
    DataMinCardinality{}     -> notEL "DataMinCardinality"
    DataMaxCardinality{}     -> notEL "DataMaxCardinality"
    DataExactCardinality{}   -> notEL "DataExactCardinality"
  where
    c' = convertClass c
    d' = convertClass d
    r' = convertObjectProperty r
    notEL s = error (s + " not part of OWL 2 EL specifications")
    notSupported s = error (s + " not yet supported")

addGCI :: Axiom -> [GCI] -> [GCI]
addGCI a gcis = case a of
    AxiomDeclaration _             -> gcis
    AxiomAnnotation _              -> gcis
    AxiomAssertion _               -> gcis  -- as long as we don't have
                                            -- nominals we can ignore
                                            -- assertions
    AxiomClass (SubClassOf _  (SubClassExpression c) (SuperClassExpression d))
                                   -> Subclass c' d' : gcis
    AxiomClass (EquivalentClasses _  c d)
                                   -> Subclass c' d' : Subclass d' c' : gcis
    AxiomClass (DisjointClasses{}) -> notSupported "DisjointClasses"
    AxiomClass (DisjointUnion{})   -> notEL "DisjointUnion"
    AxiomObjectProperty _          -> notSupported "AxiomObjectProperty"
    AxiomDataProperty _            -> notSupported "AxiomDataProperty"
    AxiomDatatype _                -> notSupported "AxiomDatatype"
    AxiomHasKey _                  -> notSupported "AxiomHasKey"
  where
    c' = convertClass c
    d' = convertClass d
    notEL s = error (s + " not part of OWL 2 EL specifications")
    notSupported s = error (s + " not yet supported")


extractGCIs :: OntologyDocument -> [GCI]
extractGCIs (OntologyDocument _ o) =
    foldr addGCI [] $ ontologyAxioms o
