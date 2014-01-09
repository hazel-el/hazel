{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.Functional
Copyright   :  (c) 2013, 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser for OWL 2 Web Ontology Language, Functional-Style syntax.
-}

module Hazel.Parser.OWL.Functional where

import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Control.Arrow ( (&&&)
                     , (>>>)
                     )
import Data.Text ( Text
                 , pack
                 )
import Data.Attoparsec.Text

import Hazel.Parser.Utils ((<<))
import Hazel.Parser.OWL.BCP47 ( LanguageTag
                              , langTag
                              )
import Hazel.Parser.OWL.RFC3987 (iriReference)
import Hazel.Parser.OWL.SPARQL ( blankNodeLabel
                               , pnameNS
                               , pnameLN
                               )

isWhiteSpace :: Char -> Bool
isWhiteSpace = inClass " \t\r\n"

isWhiteSpaceOrStartOfComment :: Char -> Bool
isWhiteSpaceOrStartOfComment = isWhiteSpace &&& ('#'==) >>> uncurry (||)

whitespace :: Parser ()
whitespace = skipWhile isWhiteSpace

comment :: Parser ()
comment = "#" .*> skipWhile (not . inClass "\r\n")

isDelimiter :: Char -> Bool
isDelimiter = inClass "=()<>@^"

skipSpaceOrComment :: Parser ()
skipSpaceOrComment = do
  whitespace
  option () comment
  mChar <- peekChar
  case isWhiteSpaceOrStartOfComment <$> mChar of
    Just True -> skipSpaceOrComment
    _ -> return ()

skipOrDelimiter :: Parser ()
skipOrDelimiter = do
  mChar <- peekChar
  case isDelimiter <$> mChar of
    Just False -> mustSkip -- not a delimiter, skip whitespace/comments
    Just True -> skipSpaceOrComment -- at delimiter, skip if necessary
    Nothing -> return ()             -- at end of input, don't skip

mustSkip :: Parser ()
mustSkip = do
  mChar <- peekChar
  case isWhiteSpaceOrStartOfComment <$> mChar of
    Just True -> skipSpaceOrComment
    _ -> fail "expected whitespace or start of comment"
  <?> "mustSkip"

(.?>) :: Text -> Parser a -> Parser a
(.?>) str p = str .*> (skipSpaceOrComment >> p)

(<?.) :: Parser a -> Text -> Parser a
(<?.) p str = (p << skipSpaceOrComment) <*. str

(.!>) :: Text -> Parser a -> Parser a
(.!>) str p = str .*> (skipOrDelimiter >> p)

(<!.) :: Parser a -> Text -> Parser a
(<!.) p str = (p << skipOrDelimiter) <*. str

infixr 4 <?., .?>, <!., .!>

bracketed :: Text -> Parser a -> Parser a
bracketed tag p = tag .!> ("(" .?> p <?. ")") << skipSpaceOrComment

nonNegativeInteger :: Parser Integer
nonNegativeInteger = decimal << skipOrDelimiter

quotedString :: Parser Text
quotedString = "\"" .*> scan False go <*. "\"" << skipOrDelimiter
  where go False '\\' = Just True -- escape character
        go True '\\' = Just False -- escaped backslash
        go True '"' = Just False  -- escaped doublequote
        go True _ = Nothing       -- invalid escape sequence
        go False '"' = Nothing    -- unescaped doublequote, i.e. end of string
        go False _ = Just False   -- any other unescaped character is okay

languageTag :: Parser LanguageTag
languageTag = "@" .*> langTag << skipOrDelimiter

type IRI = Text
fullIRI :: Parser IRI
fullIRI = pack . show <$> "<" .*> iriReference <*. ">" << skipOrDelimiter

type NodeID = Text
nodeID :: Parser NodeID
nodeID = blankNodeLabel << skipOrDelimiter

prefixName :: Parser Text
prefixName = pnameNS << skipOrDelimiter

abbreviatedIRI :: Parser IRI
abbreviatedIRI = pnameLN << skipOrDelimiter

iri :: Parser IRI
iri = fullIRI <|> abbreviatedIRI

data OntologyDocument = OntologyDocument [PrefixDeclaration] Ontology
                      deriving Show

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyDocument <$> many' prefixDeclaration
                                    <*> ontology

data PrefixDeclaration = PrefixDeclaration Text Text
                       deriving Show

prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = bracketed "Prefix" $
                    PrefixDeclaration <$> prefixName
                                      <*> ("=" .?> fullIRI)

data VersionedIRI = VersionedIRI { versionedIRIIRI :: IRI
                                 , versionedIRIVersion :: Maybe IRI
                                 }
                    deriving Show

data Ontology = Ontology { ontologyVersionedIRI :: Maybe VersionedIRI
                         , ontologyImports :: [IRI]
                         , ontologyOntologyAnnotations :: [Annotation]
                         , ontologyAxioms :: [Axiom]
                         }
                deriving Show

ontology :: Parser Ontology
ontology = bracketed "Ontology" $
           Ontology <$> versionedIRI
                    <*> directlyImportsDocuments
                    <*> ontologyAnnotations
                    <*> axioms

  where versionedIRI :: Parser (Maybe VersionedIRI)
        versionedIRI = option Nothing $
                       Just <$> (VersionedIRI <$> iri
                                              <*> (option Nothing $
                                                   Just <$> iri))

ontologyIRI :: Parser IRI
ontologyIRI = iri

versionIRI :: Parser IRI
versionIRI = iri

directlyImportsDocuments :: Parser [Text]
directlyImportsDocuments = many' $ bracketed "Import" iri

ontologyAnnotations :: Parser [Annotation]
ontologyAnnotations = many' annotation

axioms :: Parser [Axiom]
axioms = many' axiom

data Entity = Class IRI
            | Datatype IRI
            | ObjectProperty IRI
            | DataProperty IRI
            | AnnotationProperty IRI
            | NamedIndividual IRI
            deriving Show

type Declaration = ([Annotation], Entity)
declaration :: Parser Declaration
declaration = bracketed "Declaration" $ (,) <$> axiomAnnotations
                                            <*> entity

entity :: Parser Entity
entity = choice $ map ent [ ("Class", Class)
                          , ("Datatype", Datatype)
                          , ("ObjectProperty", ObjectProperty)
                          , ("DataProperty", DataProperty)
                          , ("AnnotationProperty", AnnotationProperty)
                          , ("NamedIndividual", NamedIndividual)
                          ]
  where ent :: (Text, Text -> Entity) -> Parser Entity
        ent (tag, ctor)= bracketed tag $ ctor <$> iri

data AnnotationSubject = AnonymousSubject NodeID
                       | NamedSubject IRI
                       deriving Show

annotationSubject :: Parser AnnotationSubject
annotationSubject = AnonymousSubject <$> anonymousIndividual
                    <|> NamedSubject <$> iri

data AnnotationValue = AnonymousValue NodeID
                     | NamedValue IRI
                     | LiteralValue Literal
                     deriving Show

annotationValue :: Parser AnnotationValue
annotationValue = AnonymousValue <$> anonymousIndividual
                  <|> NamedValue <$> iri
                  <|> LiteralValue <$> literal

axiomAnnotations :: Parser [Annotation]
axiomAnnotations = many' annotation

data Annotation = Annotation [Annotation] IRI AnnotationValue
                  deriving Show

annotation :: Parser Annotation
annotation = bracketed "Annotation" $
             Annotation <$> annotationAnnotations
                        <*> iri
                        <*> annotationValue

annotationAnnotations :: Parser [Annotation]
annotationAnnotations = many' annotation

data AnnotationAxiom = AnnotationAssertion [Annotation] IRI AnnotationSubject AnnotationValue
                     | SubAnnotationPropertyOf [Annotation] IRI IRI
                     | AnnotationPropertyDomain [Annotation] IRI IRI
                     | AnnotationPropertyRange [Annotation] IRI IRI
                     deriving Show

annotationAxiom :: Parser AnnotationAxiom
annotationAxiom = annotationAssertion
                  <|> subAnnotationPropertyOf
                  <|> annotationPropertyDomain
                  <|> annotationPropertyRange

annotationAxiom' :: Text -> ([Annotation] -> IRI -> a -> AnnotationAxiom) -> Parser a -> Parser AnnotationAxiom
annotationAxiom' tag ctor p = bracketed tag $
                              ctor <$> annotationAnnotations
                                   <*> iri
                                   <*> p

annotationAssertion :: Parser AnnotationAxiom
annotationAssertion = bracketed "AnnotationAssertion" $
                      AnnotationAssertion <$> axiomAnnotations
                                          <*> iri
                                          <*> annotationSubject
                                          <*> annotationValue

subAnnotationPropertyOf :: Parser AnnotationAxiom
subAnnotationPropertyOf = annotationAxiom' "SubAnnotationPropertyOf" SubAnnotationPropertyOf iri

subAnnotationProperty :: Parser IRI
subAnnotationProperty = iri

superAnnotationProperty :: Parser IRI
superAnnotationProperty = iri

annotationPropertyDomain :: Parser AnnotationAxiom
annotationPropertyDomain = annotationAxiom' "AnnotationPropertyDomain" AnnotationPropertyDomain iri

annotationPropertyRange :: Parser AnnotationAxiom
annotationPropertyRange = annotationAxiom' "AnnotationPropertyRange" AnnotationPropertyRange iri

class' :: Parser Entity
class' = Class <$> iri

datatype :: Parser Entity
datatype = Datatype <$> iri

objectProperty :: Parser Entity
objectProperty = ObjectProperty <$> iri

dataProperty :: Parser Entity
dataProperty = DataProperty <$> iri

annotationProperty :: Parser Entity
annotationProperty = AnnotationProperty <$> iri

type Individual = Either IRI NodeID

individual :: Parser Individual
individual = Left <$> namedIndividual'
             <|> Right <$> anonymousIndividual

namedIndividual' :: Parser IRI
namedIndividual' = iri

namedIndividual :: Parser Entity
namedIndividual = NamedIndividual <$> namedIndividual'

anonymousIndividual :: Parser NodeID
anonymousIndividual = nodeID

data Literal = TypedLiteral Text IRI
             | StringLiteralNoLanguage Text
             | StringLiteralWithLanguage Text LanguageTag
             deriving Show

literal :: Parser Literal
literal = typedLiteral
          <|> stringLiteralNoLanguage
          <|> stringLiteralWithLanguage

typedLiteral :: Parser Literal
typedLiteral = TypedLiteral <$> lexicalForm
                            <*> ("^^" .*> iri)

lexicalForm :: Parser Text
lexicalForm = quotedString

stringLiteralNoLanguage :: Parser Literal
stringLiteralNoLanguage = StringLiteralNoLanguage <$> quotedString

stringLiteralWithLanguage :: Parser Literal
stringLiteralWithLanguage = StringLiteralWithLanguage <$> quotedString
                                                      <*> languageTag

data InverseObjectProperty = InverseObjectProperty IRI
                           deriving Show
type ObjectProperty = IRI
type ObjectPropertyExpression = Either ObjectProperty InverseObjectProperty

unObjectProperty :: Entity -> IRI
unObjectProperty (ObjectProperty i) = i
unObjectProperty _ = error "Entity is not an ObjectProperty"

objectPropertyExpression :: Parser ObjectPropertyExpression
objectPropertyExpression = Left . unObjectProperty <$> objectProperty
                           <|> Right <$> inverseObjectProperty

inverseObjectProperty :: Parser InverseObjectProperty
inverseObjectProperty = InverseObjectProperty . unObjectProperty <$>
                        bracketed "ObjectInverseOf" objectProperty

type DataProperty = IRI

unDataProperty :: Entity -> IRI
unDataProperty (DataProperty i) = i
unDataProperty _ = error "Entity is not a DataProperty"

dataPropertyExpression :: Parser DataProperty
dataPropertyExpression = unDataProperty <$> dataProperty

type DataType = IRI
type ConstrainingFacet = IRI
type RestrictionValue = Literal

unDatatype :: Entity -> DataType
unDatatype (Datatype i) = i
unDatatype _ = error "Entity is not a Datatype"

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

dataRange :: Parser DataRange
dataRange = dataType
            <|> dataIntersectionOf
            <|> dataUnionOf
            <|> dataComplementOf
            <|> dataOneOf
            <|> dataTypeRestriction

dataType :: Parser DataRange
dataType = DataType . unDatatype <$> datatype


dataIntersectionOf :: Parser DataRange
dataIntersectionOf = bracketed "DataIntersectionOf" $
                     DataIntersectionOf <$> dataRange
                                        <*> dataRange
                                        <*> many' dataRange

dataUnionOf :: Parser DataRange
dataUnionOf = bracketed "DataUnionOf" $
              DataUnionOf <$> dataRange
                          <*> dataRange
                          <*> many' dataRange

dataComplementOf :: Parser DataRange
dataComplementOf = bracketed "DataComplementOf" $
                   DataComplementOf <$> dataRange

dataOneOf :: Parser DataRange
dataOneOf = bracketed "DataOneOf" $
            DataOneOf <$> literal
                      <*> many' literal

dataTypeRestriction :: Parser DataRange
dataTypeRestriction = bracketed "DatatypeRestriction" $
                      DataTypeRestriction <$> iri
                                          <*> iri
                                          <*> literal
                                          <*> many' restriction
  where restriction = (,) <$> iri
                          <*> literal

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
unClass :: Entity -> Class
unClass (Class i) = i
unClass _ = error "Entity is not a class"

class'' :: Parser ClassExpression
class'' = Class' . unClass <$> class'

objectIntersectionOf :: Parser ClassExpression
objectIntersectionOf = bracketed "ObjectIntersectionOf" $
                       ObjectIntersectionOf <$> classExpression
                                            <*> classExpression
                                            <*> many' classExpression

objectUnionOf :: Parser ClassExpression
objectUnionOf = bracketed "ObjectUnionOf" $
                ObjectUnionOf <$> classExpression
                              <*> classExpression
                              <*> many' classExpression

objectComplementOf :: Parser ClassExpression
objectComplementOf = bracketed "ObjectComplementOf" $
                     ObjectComplementOf <$> classExpression

objectOneOf :: Parser ClassExpression
objectOneOf = bracketed "ObjectOneOf" $
              ObjectOneOf <$> individual
                          <*> many' individual

objectSomeValuesFrom :: Parser ClassExpression
objectSomeValuesFrom = bracketed "ObjectSomeValuesFrom" $
                       ObjectSomeValuesFrom <$> objectPropertyExpression
                                            <*> classExpression

objectAllValuesFrom :: Parser ClassExpression
objectAllValuesFrom = bracketed "ObjectAllValuesFrom" $
                      ObjectAllValuesFrom <$> objectPropertyExpression
                                          <*> classExpression

objectHasValue :: Parser ClassExpression
objectHasValue = bracketed "ObjectHasValue" $
                 ObjectHasValue <$> objectPropertyExpression
                                <*> individual

objectHasSelf :: Parser ClassExpression
objectHasSelf = bracketed "ObjectHasSelf" $
                ObjectHasSelf <$> objectPropertyExpression

cardinality' :: Text
                -> (Integer
                    -> a
                    -> Maybe b
                    -> ClassExpression)
                -> Parser a
                -> Parser b
                -> Parser ClassExpression
cardinality' tag ctor p q = bracketed tag $
                              ctor <$> nonNegativeInteger
                                   <*> p
                                   <*> (option Nothing $
                                        Just <$> q)

objectCardinality' :: Text
                      -> (Integer
                          -> ObjectPropertyExpression
                          -> Maybe ClassExpression
                          -> ClassExpression)
                      -> Parser ClassExpression
objectCardinality' tag ctor = cardinality' tag ctor
                              objectPropertyExpression classExpression

objectMinCardinality :: Parser ClassExpression
objectMinCardinality = objectCardinality' "ObjectMinCardinality"
                       ObjectMinCardinality

objectMaxCardinality :: Parser ClassExpression
objectMaxCardinality = objectCardinality' "ObjectMaxCardinality"
                       ObjectMaxCardinality

objectExactCardinality :: Parser ClassExpression
objectExactCardinality = objectCardinality'  "ObjectExactCardinality"
                         ObjectExactCardinality

dataSomeValuesFrom :: Parser ClassExpression
dataSomeValuesFrom = bracketed "DataSomeValuesFrom" $
                     DataSomeValuesFrom <$> dataPropertyExpression
                                        <*> many' dataPropertyExpression
                                        <*> dataRange

dataAllValuesFrom :: Parser ClassExpression
dataAllValuesFrom = bracketed "DataAllValuesFrom" $
                    DataAllValuesFrom <$> dataPropertyExpression
                                      <*> many' dataPropertyExpression
                                      <*> dataRange

dataHasValue :: Parser ClassExpression
dataHasValue = bracketed "DataHasValue" $
               DataHasValue <$> dataPropertyExpression
                            <*> literal

dataCardinality' :: Text
                      -> (Integer
                          -> DataProperty
                          -> Maybe DataRange
                          -> ClassExpression)
                      -> Parser ClassExpression
dataCardinality' tag ctor = cardinality' tag ctor
                            dataPropertyExpression dataRange

dataMinCardinality :: Parser ClassExpression
dataMinCardinality = dataCardinality' "DataMinCardinality" DataMinCardinality

dataMaxCardinality :: Parser ClassExpression
dataMaxCardinality = dataCardinality' "DataMaxCardinality" DataMaxCardinality

dataExactCardinality :: Parser ClassExpression
dataExactCardinality = dataCardinality' "DataExactCardinality"
                       DataExactCardinality

classExpression :: Parser ClassExpression
classExpression = class''
                  <|> objectIntersectionOf
                  <|> objectUnionOf
                  <|> objectComplementOf
                  <|> objectOneOf
                  <|> objectSomeValuesFrom
                  <|> objectAllValuesFrom
                  <|> objectHasValue
                  <|> objectHasSelf
                  <|> objectMinCardinality
                  <|> objectMaxCardinality
                  <|> objectExactCardinality
                  <|> dataSomeValuesFrom
                  <|> dataAllValuesFrom
                  <|> dataHasValue
                  <|> dataMinCardinality
                  <|> dataMaxCardinality
                  <|> dataExactCardinality

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

data SubClassExpression = SubClassExpression ClassExpression
                          deriving Show

subClassExpression :: Parser SubClassExpression
subClassExpression = SubClassExpression <$> classExpression

data SuperClassExpression = SuperClassExpression ClassExpression
                            deriving Show

superClassExpression :: Parser SuperClassExpression
superClassExpression = SuperClassExpression <$> classExpression

subClassOf :: Parser ClassAxiom
subClassOf = bracketed "SubClassOf" $
             SubClassOf <$> axiomAnnotations
                        <*> subClassExpression
                        <*> superClassExpression

equivalentClasses :: Parser ClassAxiom
equivalentClasses = bracketed "EquivalentClasses" $
                    EquivalentClasses <$> axiomAnnotations
                                      <*> classExpression
                                      <*> classExpression
                                      <*> many' classExpression

disjointClasses :: Parser ClassAxiom
disjointClasses = bracketed "DisjointClasses" $
                  DisjointClasses <$> axiomAnnotations
                                  <*> classExpression
                                  <*> classExpression
                                  <*> many' classExpression

data DisjointClassExpressions = DisjointClassExpressions ClassExpression ClassExpression [ClassExpression]
                              deriving Show

disjointClassExprressions :: Parser DisjointClassExpressions
disjointClassExprressions = DisjointClassExpressions <$> classExpression
                                                     <*> classExpression
                                                     <*> many' classExpression

disjointUnion :: Parser ClassAxiom
disjointUnion = bracketed "DisjointUnion" $
                DisjointUnion <$> axiomAnnotations
                              <*> (unClass <$> class')
                              <*> disjointClassExprressions

classAxiom :: Parser ClassAxiom
classAxiom = subClassOf
             <|> equivalentClasses
             <|> disjointClasses
             <|> disjointUnion

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

data SubObjectPropertyExpression = SubObjectPropertyExpression ObjectPropertyExpression                                 | SubObjectPropertyExpressionChain PropertyExpressionChain
                                 deriving Show

subObjectPropertyExpression :: Parser SubObjectPropertyExpression
subObjectPropertyExpression = SubObjectPropertyExpression <$> objectPropertyExpression
                              <|> SubObjectPropertyExpressionChain <$> propertyExpressionChain

data SuperObjectPropertyExpression = SuperObjectPropertyExpression ObjectPropertyExpression
                                   deriving Show

superObjectPropertyExpression :: Parser SuperObjectPropertyExpression
superObjectPropertyExpression = SuperObjectPropertyExpression <$> objectPropertyExpression

data PropertyExpressionChain = PropertyExpressionChain ObjectPropertyExpression ObjectPropertyExpression [ObjectPropertyExpression]
                             deriving Show

propertyExpressionChain :: Parser PropertyExpressionChain
propertyExpressionChain = bracketed "ObjectPropertyChain" $
                          PropertyExpressionChain <$> objectPropertyExpression
                                                  <*> objectPropertyExpression
                                                  <*> many' objectPropertyExpression

subObjectPropertyOf :: Parser ObjectPropertyAxiom
subObjectPropertyOf = bracketed "SubObjectPropertyOf" $
                      SubObjectPropertyOf <$> axiomAnnotations
                                          <*> subObjectPropertyExpression
                                          <*> superObjectPropertyExpression

properties' :: Text
                     -> ([Annotation]
                         -> a
                         -> a
                         -> [a]
                         -> b)
                     -> Parser a
                     -> Parser b
properties' tag ctor p = bracketed tag $
                         ctor <$> axiomAnnotations
                              <*> p
                              <*> p
                              <*> many' p

equivalentObjectProperties :: Parser ObjectPropertyAxiom
equivalentObjectProperties = properties' "EquivalentObjectProperties"
                             EquivalentObjectProperties
                             objectPropertyExpression

disjointObjectProperties :: Parser ObjectPropertyAxiom
disjointObjectProperties = properties' "DisjointObjectProperties"
                           DisjointObjectProperties
                           objectPropertyExpression

objectPropertyDomain :: Parser ObjectPropertyAxiom
objectPropertyDomain = bracketed "ObjectPropertyDomain" $
                       ObjectPropertyDomain <$> axiomAnnotations
                                            <*> objectPropertyExpression
                                            <*> classExpression

objectPropertyRange :: Parser ObjectPropertyAxiom
objectPropertyRange = bracketed "ObjectPropertyRange" $
                      ObjectPropertyRange <$> axiomAnnotations
                                          <*> objectPropertyExpression
                                          <*> classExpression

inverseObjectProperties :: Parser ObjectPropertyAxiom
inverseObjectProperties = bracketed "InverseObjectProperties" $
                          InverseObjectProperties <$> axiomAnnotations
                                                  <*> objectPropertyExpression
                                                  <*> objectPropertyExpression

objectProperty' :: Text
                   -> ([Annotation]
                       -> ObjectPropertyExpression
                       -> ObjectPropertyAxiom)
                   -> Parser ObjectPropertyAxiom
objectProperty' tag ctor = bracketed tag $
                           ctor <$> axiomAnnotations
                                <*> objectPropertyExpression

functionalObjectProperty :: Parser ObjectPropertyAxiom
functionalObjectProperty = objectProperty' "FunctionalObjectProperty"
                           FunctionalObjectProperty

inverseFunctionalObjectProperty :: Parser ObjectPropertyAxiom
inverseFunctionalObjectProperty = objectProperty'
                                  "InverseFunctionalObjectProperty"
                                  InverseFunctionalObjectProperty

reflexiveObjectProperty :: Parser ObjectPropertyAxiom
reflexiveObjectProperty = objectProperty' "ReflexiveObjectProperty"
                          ReflexiveObjectProperty

irreflexiveObjectProperty :: Parser ObjectPropertyAxiom
irreflexiveObjectProperty = objectProperty' "IrreflexiveObjectProperty"
                          IrreflexiveObjectProperty

symmetricObjectProperty :: Parser ObjectPropertyAxiom
symmetricObjectProperty = objectProperty' "SymmetricObjectProperty"
                          SymmetricObjectProperty

asymmetricObjectProperty :: Parser ObjectPropertyAxiom
asymmetricObjectProperty = objectProperty' "AsymmetricObjectProperty"
                          AsymmetricObjectProperty

transitiveObjectProperty :: Parser ObjectPropertyAxiom
transitiveObjectProperty = objectProperty' "TransitiveObjectProperty"
                          TransitiveObjectProperty

objectPropertyAxiom :: Parser ObjectPropertyAxiom
objectPropertyAxiom = subObjectPropertyOf
                      <|> equivalentObjectProperties
                      <|> disjointObjectProperties
                      <|> inverseObjectProperties
                      <|> objectPropertyDomain
                      <|> objectPropertyRange
                      <|> functionalObjectProperty
                      <|> inverseFunctionalObjectProperty
                      <|> reflexiveObjectProperty
                      <|> irreflexiveObjectProperty
                      <|> symmetricObjectProperty
                      <|> asymmetricObjectProperty
                      <|> transitiveObjectProperty

data DataPropertyAxiom = SubDataPropertyOf [Annotation] SubDataPropertyExpression SuperDataPropertyExpression
                       | EquivalentDataProperties [Annotation] DataProperty DataProperty [DataProperty]
                       | DisjointDataProperties [Annotation] DataProperty DataProperty [DataProperty]
                       | DataPropertyDomain [Annotation] DataProperty ClassExpression
                       | DataPropertyRange [Annotation] DataProperty DataRange
                       | FunctionalDataProperty [Annotation] DataProperty
                       deriving Show

data SubDataPropertyExpression = SubDataPropertyExpression DataProperty
                               deriving Show

subDataPropertyExpression :: Parser SubDataPropertyExpression
subDataPropertyExpression = SubDataPropertyExpression <$> dataPropertyExpression

data SuperDataPropertyExpression = SuperDataPropertyExpression DataProperty
                                 deriving Show

superDataPropertyExpression :: Parser SuperDataPropertyExpression
superDataPropertyExpression = SuperDataPropertyExpression <$> dataPropertyExpression

subDataPropertyOf :: Parser DataPropertyAxiom
subDataPropertyOf = bracketed "SubDataPropertyOf" $
                    SubDataPropertyOf <$> axiomAnnotations
                                      <*> subDataPropertyExpression
                                      <*> superDataPropertyExpression

equivalentDataProperties :: Parser DataPropertyAxiom
equivalentDataProperties = properties' "EquivalentDataProperties"
                           EquivalentDataProperties
                           dataPropertyExpression

disjointDataProperties :: Parser DataPropertyAxiom
disjointDataProperties = properties' "DisjointDataProperties"
                         DisjointDataProperties
                         dataPropertyExpression

dataPropertyDomain :: Parser DataPropertyAxiom
dataPropertyDomain = bracketed "DataPropertyDomain" $
                     DataPropertyDomain <$> axiomAnnotations
                                        <*> dataPropertyExpression
                                        <*> classExpression

dataPropertyRange :: Parser DataPropertyAxiom
dataPropertyRange = bracketed "DataPropertyRange" $
                    DataPropertyRange <$> axiomAnnotations
                                      <*> dataPropertyExpression
                                      <*> dataRange

functionalDataProperty :: Parser DataPropertyAxiom
functionalDataProperty = bracketed "FunctionalDataProperty" $
                         FunctionalDataProperty <$> axiomAnnotations
                                                <*> dataPropertyExpression

dataPropertyAxiom :: Parser DataPropertyAxiom
dataPropertyAxiom = subDataPropertyOf
                    <|> equivalentDataProperties
                    <|> disjointDataProperties
                    <|> dataPropertyDomain
                    <|> dataPropertyRange
                    <|> functionalDataProperty

data DatatypeDefinition = DatatypeDefinition [Annotation] DataType DataRange
                        deriving Show

datatypeDefinition :: Parser DatatypeDefinition
datatypeDefinition = bracketed "DatatypeDefinition" $
                     DatatypeDefinition <$> axiomAnnotations
                                        <*> iri
                                        <*> dataRange

data HasKey = HasKey [Annotation] ClassExpression [ObjectPropertyExpression] [DataProperty]
            deriving Show

hasKey :: Parser HasKey
hasKey = bracketed "HasKey" $
         HasKey <$> axiomAnnotations
                <*> classExpression
                <*> ("(" .?> many' objectPropertyExpression <?. ")")
                <*> ("(" .?> many' dataPropertyExpression <?. ")")

data Assertion = SameIndividual [Annotation] Individual Individual [Individual]
               | DifferentIndividuals [Annotation] Individual Individual [Individual]
               | ClassAssertion [Annotation] ClassExpression Individual
               | ObjectPropertyAssertion [Annotation] ObjectPropertyExpression SourceIndividual TargetIndividual
               | NegativeObjectPropertyAssertion [Annotation] ObjectPropertyExpression SourceIndividual TargetIndividual
               | DataPropertyAssertion [Annotation] DataProperty SourceIndividual TargetValue
               | NegativeDataPropertyAssertion [Annotation] DataProperty SourceIndividual TargetValue
               deriving Show

data SourceIndividual = SourceIndividual Individual
                      deriving Show

sourceIndividual :: Parser SourceIndividual
sourceIndividual = SourceIndividual <$> individual

data TargetIndividual = TargetIndividual Individual
                      deriving Show

targetIndividual :: Parser TargetIndividual
targetIndividual = TargetIndividual <$> individual

type TargetValue = Literal

targetValue :: Parser TargetValue
targetValue = literal

sameIndividual :: Parser Assertion
sameIndividual = bracketed "SameIndividual" $
                 SameIndividual <$> axiomAnnotations
                                <*> individual
                                <*> individual
                                <*> many' individual

differentIndividuals :: Parser Assertion
differentIndividuals = bracketed "DifferentIndividuals" $
                       DifferentIndividuals <$> axiomAnnotations
                                            <*> individual
                                            <*> individual
                                            <*> many' individual

classAssertion :: Parser Assertion
classAssertion = bracketed "ClassAssertion" $
                 ClassAssertion <$> axiomAnnotations
                                <*> classExpression
                                <*> individual

assertion' :: Text
              -> ([Annotation] -> a -> SourceIndividual -> c -> Assertion)
              -> Parser a
              -> Parser c
              -> Parser Assertion
assertion' tag ctor p q = bracketed tag $
                          ctor <$> axiomAnnotations
                               <*> p
                               <*> sourceIndividual
                               <*> q

objectPropertyAssertion :: Parser Assertion
objectPropertyAssertion = assertion' "ObjectPropertyAssertion"
                          ObjectPropertyAssertion
                          objectPropertyExpression
                          targetIndividual

negativeObjectPropertyAssertion :: Parser Assertion
negativeObjectPropertyAssertion = assertion' "NegativeObjectPropertyAssertion"
                                  NegativeObjectPropertyAssertion
                                  objectPropertyExpression
                                  targetIndividual

dataPropertyAssertion :: Parser Assertion
dataPropertyAssertion = assertion' "DataPropertyAssertion"
                        DataPropertyAssertion
                        dataPropertyExpression
                        targetValue

negativeDataPropertyAssertion :: Parser Assertion
negativeDataPropertyAssertion = assertion' "NegativeDataPropertyAssertion"
                                NegativeDataPropertyAssertion
                                dataPropertyExpression
                                targetValue

assertion :: Parser Assertion
assertion = sameIndividual
            <|> differentIndividuals
            <|> classAssertion
            <|> objectPropertyAssertion
            <|> negativeObjectPropertyAssertion
            <|> dataPropertyAssertion
            <|> negativeDataPropertyAssertion

axiom :: Parser Axiom
axiom = AxiomDeclaration <$> declaration
        <|> AxiomClass <$> classAxiom
        <|> AxiomObjectProperty <$> objectPropertyAxiom
        <|> AxiomDataProperty <$> dataPropertyAxiom
        <|> AxiomDatatype <$> datatypeDefinition
        <|> AxiomHasKey <$> hasKey
        <|> AxiomAssertion <$> assertion
        <|> AxiomAnnotation <$> annotationAxiom
