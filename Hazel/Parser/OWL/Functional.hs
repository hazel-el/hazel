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

module Hazel.Parser.OWL.Functional (ontologyDocument) where

import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Data.Text (Text)
import Data.Attoparsec.Text

import Hazel.Parser.Utils ((<<))
import Hazel.Parser.OWL.BCP47 (langTag)
import Hazel.Parser.OWL.RFC3987 (iriReference, showIRI)
import Hazel.Parser.OWL.SPARQL ( blankNodeLabel
                               , pnameNS
                               , pnameLN
                               )
import Hazel.Parser.OWL.AST

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace '\r' = True
isWhiteSpace '\n' = True
isWhiteSpace _ = False

isWhiteSpaceOrStartOfComment :: Char -> Bool
isWhiteSpaceOrStartOfComment '#' = True
isWhiteSpaceOrStartOfComment c = isWhiteSpace c

whitespace :: Parser ()
whitespace = skipWhile isWhiteSpace

isEOL :: Char -> Bool
isEOL '\r' = True
isEOL '\n' = True
isEOL _ = False

comment :: Parser ()
comment = "#" .*> skipWhile (not . isEOL)

isDelimiter :: Char -> Bool
isDelimiter '=' = True
isDelimiter '(' = True
isDelimiter ')' = True
isDelimiter '<' = True
isDelimiter '>' = True
isDelimiter '@' = True
isDelimiter '^' = True
isDelimiter _ = False

skipSpaceOrComment :: Parser ()
skipSpaceOrComment = do
  whitespace
  mChar <- peekChar
  case mChar of
    Just '#' -> comment >> skipSpaceOrComment
    _ -> return ()

skipOrDelimiter :: Parser ()
skipOrDelimiter = do
  mChar <- peekChar
  case isDelimiter <$> mChar of
    Just False -> mustSkip -- not a delimiter, skip whitespace/comments
    Just True -> return ()  -- at delimiter, nothing to skip here
    Nothing -> return ()    -- at end of input, don't skip

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

infixr 4 <?., .?>

bracketed :: Text -> Parser a -> Parser a
bracketed tag p = tag .?> ("(" .?> p <?. ")") << skipSpaceOrComment

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

fullIRI :: Parser IRI
fullIRI = showIRI <$> "<" .*> iriReference <*. ">" << skipSpaceOrComment

nodeID :: Parser NodeID
nodeID = blankNodeLabel << skipOrDelimiter

prefixName :: Parser Text
prefixName = pnameNS << skipOrDelimiter

abbreviatedIRI :: Parser IRI
abbreviatedIRI = pnameLN << skipOrDelimiter

iri :: Parser IRI
iri = fullIRI <|> abbreviatedIRI

ontologyDocument :: Parser OntologyDocument
ontologyDocument = OntologyDocument <$> many' prefixDeclaration
                                    <*> ontology

prefixDeclaration :: Parser PrefixDeclaration
prefixDeclaration = bracketed "Prefix" $
                    PrefixDeclaration <$> prefixName
                                      <*> ("=" .?> fullIRI)

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

directlyImportsDocuments :: Parser [Text]
directlyImportsDocuments = many' $ bracketed "Import" iri

ontologyAnnotations :: Parser [Annotation]
ontologyAnnotations = many' annotation

axioms :: Parser [Axiom]
axioms = many' axiom

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

annotationSubject :: Parser AnnotationSubject
annotationSubject = AnonymousSubject <$> anonymousIndividual
                    <|> NamedSubject <$> iri

annotationValue :: Parser AnnotationValue
annotationValue = AnonymousValue <$> anonymousIndividual
                  <|> NamedValue <$> iri
                  <|> LiteralValue <$> literal

axiomAnnotations :: Parser [Annotation]
axiomAnnotations = many' annotation

annotation :: Parser Annotation
annotation = bracketed "Annotation" $
             Annotation <$> annotationAnnotations
                        <*> iri
                        <*> annotationValue

annotationAnnotations :: Parser [Annotation]
annotationAnnotations = many' annotation

annotationAxiom :: Parser AnnotationAxiom
annotationAxiom = annotationAssertion
                  <|> subAnnotationPropertyOf
                  <|> annotationPropertyDomain
                  <|> annotationPropertyRange

annotationAxiom' :: Text -> ([Annotation] -> AnnotationProperty -> a -> AnnotationAxiom) -> Parser a -> Parser AnnotationAxiom
annotationAxiom' tag ctor p = bracketed tag $
                              ctor <$> annotationAnnotations
                                   <*> annotationProperty
                                   <*> p

annotationAssertion :: Parser AnnotationAxiom
annotationAssertion = bracketed "AnnotationAssertion" $
                      AnnotationAssertion <$> axiomAnnotations
                                          <*> annotationProperty
                                          <*> annotationSubject
                                          <*> annotationValue

subAnnotationPropertyOf :: Parser AnnotationAxiom
subAnnotationPropertyOf = bracketed "SubAnnotationPropertyOf" $
                          SubAnnotationPropertyOf <$> annotationAnnotations
                                                  <*> subAnnotationProperty
                                                  <*> superAnnotationProperty

subAnnotationProperty :: Parser SubAnnotationProperty
subAnnotationProperty = SubAnnotationProperty <$> iri

superAnnotationProperty :: Parser SuperAnnotationProperty
superAnnotationProperty = SuperAnnotationProperty <$> iri

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

annotationProperty :: Parser AnnotationProperty
annotationProperty = AnnotationProperty' <$> iri


individual :: Parser Individual
individual = Left <$> namedIndividual'
             <|> Right <$> anonymousIndividual

namedIndividual' :: Parser IRI
namedIndividual' = iri

anonymousIndividual :: Parser NodeID
anonymousIndividual = nodeID

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

unObjectProperty :: Entity -> IRI
unObjectProperty (ObjectProperty i) = i
unObjectProperty _ = error "Entity is not an ObjectProperty"

objectPropertyExpression :: Parser ObjectPropertyExpression
objectPropertyExpression = Left . unObjectProperty <$> objectProperty
                           <|> Right <$> inverseObjectProperty

inverseObjectProperty :: Parser InverseObjectProperty
inverseObjectProperty = InverseObjectProperty . unObjectProperty <$>
                        bracketed "ObjectInverseOf" objectProperty

unDataProperty :: Entity -> IRI
unDataProperty (DataProperty i) = i
unDataProperty _ = error "Entity is not a DataProperty"

dataPropertyExpression :: Parser DataProperty
dataPropertyExpression = unDataProperty <$> dataProperty

unDatatype :: Entity -> DataType
unDatatype (Datatype i) = i
unDatatype _ = error "Entity is not a Datatype"

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

subClassExpression :: Parser SubClassExpression
subClassExpression = SubClassExpression <$> classExpression

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

subObjectPropertyExpression :: Parser SubObjectPropertyExpression
subObjectPropertyExpression = SubObjectPropertyExpression <$> objectPropertyExpression
                              <|> SubObjectPropertyExpressionChain <$> propertyExpressionChain

superObjectPropertyExpression :: Parser SuperObjectPropertyExpression
superObjectPropertyExpression = SuperObjectPropertyExpression <$> objectPropertyExpression

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

subDataPropertyExpression :: Parser SubDataPropertyExpression
subDataPropertyExpression = SubDataPropertyExpression <$> dataPropertyExpression

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

datatypeDefinition :: Parser DatatypeDefinition
datatypeDefinition = bracketed "DatatypeDefinition" $
                     DatatypeDefinition <$> axiomAnnotations
                                        <*> iri
                                        <*> dataRange

hasKey :: Parser HasKey
hasKey = bracketed "HasKey" $
         HasKey <$> axiomAnnotations
                <*> classExpression
                <*> ("(" .?> many' objectPropertyExpression <?. ")")
                <*> ("(" .?> many' dataPropertyExpression <?. ")")

sourceIndividual :: Parser SourceIndividual
sourceIndividual = SourceIndividual <$> individual

targetIndividual :: Parser TargetIndividual
targetIndividual = TargetIndividual <$> individual

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
