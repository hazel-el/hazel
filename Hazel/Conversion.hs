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

import Hazel.Core 
import Hazel.Parser.OWL.Functional

import Data.Text (unpack)


convertObjectProperty :: ObjectPropertyExpression -> Role
convertObjectProperty (Left iri)
    | unpack iri == "owl:bottomObjectProperty"
        = error "owl:bottomObjectProperty not yet supported"
    | unpack iri == "owl:topObjectProperty"
        = error "owl:topObjectProperty not yet supported"
    | otherwise = Role iri
convertObjectProperty (Right _)
    = error "InverseObjectProperty not part of OWL 2 EL specs"

convertClass :: ClassExpression -> Concept
convertClass (Class' iri)
    | unpack iri == "owl:Thing"   = Top
    | unpack iri == "owl:Nothing" = error "owl:Nothing not yet supported"
    | otherwise                   = Name iri
convertClass c = case c of
    ObjectIntersectionOf c d cs -> foldr (And . convertClass) (c' `And` d') cs
        where c' = convertClass c
              d' = convertClass d
    ObjectUnionOf{}             -> notEL "ObjectUnionOf"
    ObjectComplementOf _        -> notEL "ObjectComplementOf"
    ObjectOneOf{}               -> notSupported "ObjectOneOf"
    ObjectSomeValuesFrom r c    -> Exists r' c'
        where c' = convertClass c
              r' = convertObjectProperty r
    ObjectAllValuesFrom{}       -> notEL "ObjectAllValuesFrom"
    ObjectHasValue{}            -> notSupported "ObjectHasValue"
    ObjectHasSelf _             -> notSupported "ObjectHasSelf"
    ObjectMinCardinality{}      -> notEL "ObjectMinCardinality"
    ObjectMaxCardinality{}      -> notEL "ObjectMaxCardinality"
    ObjectExactCardinality{}    -> notEL "ObjectExactCardinality"
    DataSomeValuesFrom{}        -> notSupported "DataSomeValuesFrom"
    DataAllValuesFrom{}         -> notEL "DataAllValuesFrom"
    DataHasValue{}              -> notSupported "DataHasValue"
    DataMinCardinality{}        -> notEL "DataMinCardinality"
    DataMaxCardinality{}        -> notEL "DataMaxCardinality"
    DataExactCardinality{}      -> notEL "DataExactCardinality"
  where
    notEL s = error (s ++ " not part of OWL 2 EL specifications")
    notSupported s = error (s ++ " not yet supported")

addGCI :: Axiom -> [GCI] -> [GCI]
addGCI a gcis = case a of
    AxiomDeclaration _             -> gcis
    AxiomAnnotation _              -> gcis
    AxiomAssertion _               -> gcis  -- as long as we don't have
                                            -- nominals we can ignore
                                            -- assertions
    AxiomClass (SubClassOf _  (SubClassExpression c) (SuperClassExpression d))
                                   -> Subclass c' d' : gcis
        where c' = convertClass c
              d' = convertClass d
    AxiomClass (EquivalentClasses _  c d [])
                                   -> Subclass c' d' : Subclass d' c' : gcis
        where c' = convertClass c
              d' = convertClass d
    AxiomClass EquivalentClasses{} -> notSupported "more than 2 classes in equivalence "
    AxiomClass DisjointClasses{}   -> notSupported "DisjointClasses"
    AxiomClass DisjointUnion{}     -> notEL "DisjointUnion"
    AxiomObjectProperty _          -> notSupported "AxiomObjectProperty"
    AxiomDataProperty _            -> notSupported "AxiomDataProperty"
    AxiomDatatype _                -> notSupported "AxiomDatatype"
    AxiomHasKey _                  -> notSupported "AxiomHasKey"
  where
    notEL s = error (s ++ " not part of OWL 2 EL specifications")
    notSupported s = error (s ++ " not yet supported")


extractGCIs :: OntologyDocument -> [GCI]
extractGCIs (OntologyDocument _ o) =
    foldr addGCI [] $ ontologyAxioms o
