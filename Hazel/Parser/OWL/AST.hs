{- |
Module      :  Hazel.Parser.OWL.AST
Copyright   :  (c) 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Abstract Syntax Tree for OWL 2 Web Ontology Language, Functional-Style syntax.
-}

module Hazel.Parser.OWL.AST ( OntologyDocument (..)
                            , PrefixDeclaration (..)
                            , Ontology (..)
                            , VersionedIRI (..)
                            , Entity (..)
                            , AnnotationSubject (..)
                            , AnnotationValue (..)
                            , Annotation (..)
                            , AnnotationAxiom (..)
                            , Literal (..)
                            , InverseObjectProperty (..)
                            , DataRange (..)
                            , ClassExpression (..)
                            , ClassAxiom (..)
                            , SubClassExpression (..)
                            , SuperClassExpression (..)
                            , DisjointClassExpressions (..)
                            , ObjectPropertyAxiom (..)
                            , SubObjectPropertyExpression (..)
                            , SuperObjectPropertyExpression (..)
                            , PropertyExpressionChain (..)
                            , DataPropertyAxiom (..)
                            , SubDataPropertyExpression (..)
                            , SuperDataPropertyExpression (..)
                            , DatatypeDefinition (..)
                            , HasKey (..)
                            , Assertion (..)
                            , SourceIndividual (..)
                            , TargetIndividual (..)
                            , TargetValue ()
                            , LanguageTag (..)
                            , IRI ()
                            , NodeID ()
                            , Individual ()
                            , ObjectPropertyExpression ()
                            , DataProperty ()
                            , DataType ()
                            , Class ()
                            , Axiom (..)
                            , Declaration ()
                            , AnnotationProperty (..)
                            , SubAnnotationProperty (..)
                            , SuperAnnotationProperty (..)
                            ) where
import Data.Text (Text)

import Hazel.Parser.OWL.BCP47 (LanguageTag (..))

type IRI = Text
type NodeID = Text

data OntologyDocument = OntologyDocument [PrefixDeclaration] Ontology
                      deriving Show

data PrefixDeclaration = PrefixDeclaration Text Text
                       deriving Show

data VersionedIRI = VersionedIRI { ontologyIRI :: IRI
                                 , versionIRI :: Maybe IRI
                                 }
                    deriving Show

data Ontology = Ontology { ontologyVersionedIRI :: Maybe VersionedIRI
                         , ontologyImports :: [IRI]
                         , ontologyOntologyAnnotations :: [Annotation]
                         , ontologyAxioms :: [Axiom]
                         }
                deriving Show

data Entity = Class IRI
            | Datatype IRI
            | ObjectProperty IRI
            | DataProperty IRI
            | AnnotationProperty IRI
            | NamedIndividual IRI
            deriving Show

type Declaration = ([Annotation], Entity)

data AnnotationSubject = AnonymousSubject NodeID
                       | NamedSubject IRI
                       deriving Show

data AnnotationValue = AnonymousValue NodeID
                     | NamedValue IRI
                     | LiteralValue Literal
                     deriving Show

data Annotation = Annotation [Annotation] IRI AnnotationValue
                  deriving Show

newtype AnnotationProperty = AnnotationProperty' IRI
                           deriving Show

newtype SubAnnotationProperty = SubAnnotationProperty IRI
                              deriving Show

newtype SuperAnnotationProperty = SuperAnnotationProperty IRI
                                deriving Show

data AnnotationAxiom = AnnotationAssertion [Annotation] AnnotationProperty AnnotationSubject AnnotationValue
                     | SubAnnotationPropertyOf [Annotation] SubAnnotationProperty SuperAnnotationProperty
                     | AnnotationPropertyDomain [Annotation] AnnotationProperty IRI
                     | AnnotationPropertyRange [Annotation] AnnotationProperty IRI
                     deriving Show

type Individual = Either IRI NodeID

data Literal = TypedLiteral Text IRI
             | StringLiteralNoLanguage Text
             | StringLiteralWithLanguage Text LanguageTag
             deriving Show

newtype InverseObjectProperty = InverseObjectProperty IRI
                              deriving Show
type ObjectProperty = IRI
type ObjectPropertyExpression = Either ObjectProperty InverseObjectProperty

type DataProperty = IRI

type DataType = IRI
type ConstrainingFacet = IRI
type RestrictionValue = Literal

-- While technically some components could be collapsed into the
-- lists following them, this would loosen the invariant that a certain
-- number of arguments are present. Hence, we keep the explicit, yet verbose
-- form used in the OWL 2 functional-style grammar.
data DataRange = DataType DataType
               | DataIntersectionOf DataRange DataRange [DataRange]
               | DataUnionOf DataRange DataRange [DataRange]
               | DataComplementOf DataRange
               | DataOneOf Literal [Literal]
               | DataTypeRestriction DataType ConstrainingFacet RestrictionValue
                 [(ConstrainingFacet, RestrictionValue)]
               deriving Show

data ClassExpression = Class' IRI
                     | ObjectIntersectionOf ClassExpression ClassExpression [ClassExpression]
                     | ObjectUnionOf ClassExpression ClassExpression [ClassExpression]
                     | ObjectComplementOf ClassExpression
                     | ObjectOneOf Individual [Individual]
                     | ObjectSomeValuesFrom ObjectPropertyExpression ClassExpression
                     | ObjectAllValuesFrom ObjectPropertyExpression ClassExpression
                     | ObjectHasValue ObjectPropertyExpression Individual
                     | ObjectHasSelf ObjectPropertyExpression
                     | ObjectMinCardinality Integer ObjectPropertyExpression (Maybe ClassExpression)
                     | ObjectMaxCardinality Integer ObjectPropertyExpression (Maybe ClassExpression)
                     | ObjectExactCardinality Integer ObjectPropertyExpression (Maybe ClassExpression)
                     | DataSomeValuesFrom DataProperty [DataProperty] DataRange
                     | DataAllValuesFrom DataProperty [DataProperty] DataRange
                     | DataHasValue DataProperty Literal
                     | DataMinCardinality Integer DataProperty (Maybe DataRange)
                     | DataMaxCardinality Integer DataProperty (Maybe DataRange)
                     | DataExactCardinality Integer DataProperty (Maybe DataRange)
                     deriving Show

type Class = IRI

data Axiom = AxiomDeclaration Declaration
           | AxiomClass ClassAxiom
           | AxiomObjectProperty ObjectPropertyAxiom
           | AxiomDataProperty DataPropertyAxiom
           | AxiomDatatype DatatypeDefinition
           | AxiomHasKey HasKey
           | AxiomAssertion Assertion
           | AxiomAnnotation AnnotationAxiom
           deriving Show

data ClassAxiom = SubClassOf [Annotation] SubClassExpression SuperClassExpression
                | EquivalentClasses [Annotation] ClassExpression ClassExpression [ClassExpression]
                | DisjointClasses [Annotation] ClassExpression ClassExpression [ClassExpression]
                | DisjointUnion [Annotation] Class DisjointClassExpressions
                deriving Show

newtype SubClassExpression = SubClassExpression ClassExpression
                           deriving Show

newtype SuperClassExpression = SuperClassExpression ClassExpression
                             deriving Show

data DisjointClassExpressions = DisjointClassExpressions ClassExpression ClassExpression [ClassExpression]
                              deriving Show

data ObjectPropertyAxiom = SubObjectPropertyOf [Annotation] SubObjectPropertyExpression SuperObjectPropertyExpression
                         | EquivalentObjectProperties [Annotation] ObjectPropertyExpression ObjectPropertyExpression [ObjectPropertyExpression]
                         | DisjointObjectProperties [Annotation] ObjectPropertyExpression ObjectPropertyExpression [ObjectPropertyExpression]
                         | InverseObjectProperties [Annotation] ObjectPropertyExpression ObjectPropertyExpression
                         | ObjectPropertyDomain [Annotation] ObjectPropertyExpression ClassExpression
                         | ObjectPropertyRange [Annotation] ObjectPropertyExpression ClassExpression
                         | FunctionalObjectProperty [Annotation] ObjectPropertyExpression
                         | InverseFunctionalObjectProperty [Annotation] ObjectPropertyExpression
                         | ReflexiveObjectProperty [Annotation] ObjectPropertyExpression
                         | IrreflexiveObjectProperty [Annotation] ObjectPropertyExpression
                         | SymmetricObjectProperty [Annotation] ObjectPropertyExpression
                         | AsymmetricObjectProperty [Annotation] ObjectPropertyExpression
                         | TransitiveObjectProperty [Annotation] ObjectPropertyExpression
                         deriving Show

data SubObjectPropertyExpression = SubObjectPropertyExpression ObjectPropertyExpression
                                 | SubObjectPropertyExpressionChain PropertyExpressionChain
                                 deriving Show

newtype SuperObjectPropertyExpression = SuperObjectPropertyExpression ObjectPropertyExpression
                                      deriving Show

data PropertyExpressionChain = PropertyExpressionChain ObjectPropertyExpression ObjectPropertyExpression [ObjectPropertyExpression]
                             deriving Show


data DataPropertyAxiom = SubDataPropertyOf [Annotation] SubDataPropertyExpression SuperDataPropertyExpression
                       | EquivalentDataProperties [Annotation] DataProperty DataProperty [DataProperty]
                       | DisjointDataProperties [Annotation] DataProperty DataProperty [DataProperty]
                       | DataPropertyDomain [Annotation] DataProperty ClassExpression
                       | DataPropertyRange [Annotation] DataProperty DataRange
                       | FunctionalDataProperty [Annotation] DataProperty
                       deriving Show

newtype SubDataPropertyExpression = SubDataPropertyExpression DataProperty
                                  deriving Show

newtype SuperDataPropertyExpression = SuperDataPropertyExpression DataProperty
                                    deriving Show

data DatatypeDefinition = DatatypeDefinition [Annotation] DataType DataRange
                        deriving Show

data HasKey = HasKey [Annotation] ClassExpression [ObjectPropertyExpression] [DataProperty]
            deriving Show

data Assertion = SameIndividual [Annotation] Individual Individual [Individual]
               | DifferentIndividuals [Annotation] Individual Individual [Individual]
               | ClassAssertion [Annotation] ClassExpression Individual
               | ObjectPropertyAssertion [Annotation] ObjectPropertyExpression SourceIndividual TargetIndividual
               | NegativeObjectPropertyAssertion [Annotation] ObjectPropertyExpression SourceIndividual TargetIndividual
               | DataPropertyAssertion [Annotation] DataProperty SourceIndividual TargetValue
               | NegativeDataPropertyAssertion [Annotation] DataProperty SourceIndividual TargetValue
               deriving Show

newtype SourceIndividual = SourceIndividual Individual
                         deriving Show

newtype TargetIndividual = TargetIndividual Individual
                         deriving Show

type TargetValue = Literal
