{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.RFC3987
Copyright   :  (c) 2013, 2014 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser for the parts of RFC 3987 referenced in the OWL2 Functional-Style grammar.
-}

module Hazel.Parser.OWL.RFC3987 ( IRI (..)
                                , iri
                                , iRelativeRef
                                , iriReference
                                ) where

import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Data.Monoid (mappend)
import Data.Text ( Text
                 , cons
                 , pack
                 , singleton
                 , snoc
                 , unpack
                 )
import Data.Attoparsec.Text
import qualified Data.Text as T

import Hazel.Parser.Utils (countFromTo)
import Hazel.Parser.OWL.RFC5234 ( alpha
                                , hexDig
                                )

-- | RFC 3987 IRI
data IRI = AbsoluteIRI { iriScheme :: Text          -- ^ scheme
                       , iriIHierPart :: Text       -- ^ ihier-part
                       , iriIQuery :: Maybe Text    -- ^ iquery
                       , iriIFragment :: Maybe Text -- ^ ifragment
                       }
         | RelativeIRI { iriIRelativePart :: Text   -- ^ irelative-part
                       , iriIQuery :: Maybe Text    -- ^ iquery
                       , iriIFragment :: Maybe Text -- ^ ifragment
                       }

queryFragment :: IRI -> [Text]
queryFragment i = [ maybe "" (cons '?') $ iriIQuery i
                  , maybe "" (cons '#') $ iriIFragment i
                  ]


instance Show IRI where
  show i@(AbsoluteIRI _ _ _ _) = concatMap unpack $ [ iriScheme i
                                                    , ":"
                                                    , iriIHierPart i
                                                    ] ++ (queryFragment i)
  show i@(RelativeIRI _ _ _) = concatMap unpack $
                               (iriIRelativePart i):(queryFragment i)

ucsChar :: Parser Char
ucsChar = satisfy $ inClass $ concat [ "\xA0-\xD7FF"
                                     , "\xF900-\xFDCF"
                                     , "\xFDF0-\xFFEF"
                                     , "\x10000-\x1FFFD"
                                     , "\x20000-\x2FFFD"
                                     , "\x30000-\x3FFFD"
                                     , "\x40000-\x4FFFD"
                                     , "\x50000-\x5FFFD"
                                     , "\x60000-\x6FFFD"
                                     , "\x70000-\x7FFFD"
                                     , "\x80000-\x8FFFD"
                                     , "\x90000-\x9FFFD"
                                     , "\xA0000-\xAFFFD"
                                     , "\xB0000-\xBFFFD"
                                     , "\xC0000-\xCFFFD"
                                     , "\xD0000-\xDFFFD"
                                     , "\xE1000-\xEFFFD"
                                     ]

pctEncoded :: Parser Text
pctEncoded = pack <$> ((:) <$> char '%' <*> count 2 hexDig)

subDelims :: Parser Char
subDelims = satisfy $ inClass "!$&'()*+,;="

iPrivate :: Parser Char
iPrivate = satisfy $ inClass $ concat [ "\xE000-\xF8FF"
                                      , "\xF0000-\xFFFFD"
                                      , "\x100000-\x10FFFD"
                                      ]

unreserved :: Parser Char
unreserved = alpha
             <|> digit
             <|> satisfy (inClass "._~-")

iUnreserved :: Parser Char
iUnreserved = alpha
              <|> digit
              <|> satisfy (inClass "._~-")
              <|> ucsChar

ipChar :: Parser Text
ipChar = singleton <$> iUnreserved
         <|> pctEncoded
         <|> singleton <$> subDelims
         <|> singleton <$> satisfy (inClass ":@")

scheme :: Parser Text
scheme = pack <$> ((:) <$> alpha <*> many' (alpha <|> digit <|> satisfy (inClass "+.-")))

iUserInfo :: Parser Text
iUserInfo = T.concat <$> many' (singleton <$> iUnreserved
                                <|> pctEncoded
                                <|> singleton <$> subDelims
                                <|> singleton <$> satisfy (inClass ":"))

port :: Parser Text
port = pack <$> many' digit

ipLiteral :: Parser Text
ipLiteral = cons <$> char '['
                 <*> (snoc <$> (ipv6Address <|> ipvFuture)
                           <*> char ']')

iRegName :: Parser Text
iRegName = T.concat <$> many' (singleton <$> iUnreserved
                               <|> pctEncoded
                               <|> singleton <$> subDelims)

decOctet :: Parser Text
decOctet = singleton <$> digit
           <|> (cons <$> satisfy (inClass "1-9") <*> (singleton <$> digit))
           <|> cons <$> char '1'
                    <*> (pack <$> count 2 digit)
           <|> (cons <$> char '2'
                     <*> (cons <$> satisfy (inClass "0-4")
                               <*> (singleton <$> digit)))
           <|> snoc <$> string "25"
                    <*> satisfy (inClass "0-5")

ipv4Address :: Parser Text
ipv4Address = mappend <$> (T.concat <$> count 3 (snoc <$> decOctet <*> char '.')) <*> decOctet

h16 :: Parser Text
h16 = pack <$> countFromTo 1 4 hexDig

ls32 :: Parser Text
ls32 = (mappend <$> h16
                <*> (cons <$> char ':'
                          <*> h16))
       <|> ipv4Address

ipv6Address :: Parser Text
ipv6Address = suffix 6
              <|> (mappend <$> string "::"
                           <*> suffix 5)
              <|> prefixSuffix 4
              <|> prefixSuffix 3
              <|> prefixSuffix 2
              <|> prefixSuffix 1
              <|> prefixSuffix 0
              <|> (mappend <$> prefix 5
                           <*> h16)
              <|> prefix 6
  where h16c = snoc <$> h16 <*> char ':'
        suffix n = mappend <$> (T.concat <$> count n h16c)
                           <*> ls32
        prefix n = mappend <$> option "" (mappend <$> (T.concat <$> countFromTo 0 n h16c)
                                                  <*> h16)
                           <*> string "::"
        prefixSuffix n = mappend <$> prefix (4 - n)
                                 <*> suffix n

ipvFuture :: Parser Text
ipvFuture = cons <$> char 'v'
                 <*> (mappend <$> (pack <$> many1 hexDig)
                              <*> (pack <$> ((:) <$> char '.'
                                                 <*> many1 (unreserved
                                                            <|> subDelims
                                                            <|> char ':'))))

iHost :: Parser Text
iHost = ipLiteral
        <|> ipv4Address
        <|> iRegName

iAuthority :: Parser Text
iAuthority = mappend <$> option "" (snoc <$> iUserInfo <*> char '@')
                     <*> (mappend <$> iHost
                                  <*> option "" (cons <$> char ':' <*> port))

iSegment :: Parser Text
iSegment = T.concat <$> many' ipChar

iSegmentNz :: Parser Text
iSegmentNz = T.concat <$> many1 ipChar

iSegmentNzNc :: Parser Text
iSegmentNzNc = T.concat <$> many' (T.singleton <$> iUnreserved
                                   <|> pctEncoded
                                   <|> singleton <$> subDelims
                                   <|> singleton <$> char '@')

iPathAbEmpty :: Parser Text
iPathAbEmpty = T.concat <$> many' (cons <$> char '/'
                                        <*> iSegment)

iPathAbsolute :: Parser Text
iPathAbsolute = cons <$> char '/'
                     <*> option "" iPathRootless

iPathRootless :: Parser Text
iPathRootless = mappend <$> iSegmentNz
                        <*> (T.concat <$> many' (cons <$> char '/'
                                                      <*> iSegment))

iPathNoScheme :: Parser Text
iPathNoScheme = mappend <$> iSegmentNzNc
                        <*> (T.concat <$> many' (cons <$> char '/'
                                                      <*> iSegment))

iPathEmpty :: Parser Text
iPathEmpty = scan () (\ _ _ -> Nothing)

iHierPart :: Parser Text
iHierPart = mappend <$> string "//"
                    <*> (mappend <$> iAuthority
                                 <*> iPathAbEmpty)
            <|> iPathAbsolute
            <|> iPathRootless
            <|> iPathEmpty

iQuery :: Parser Text
iQuery = T.concat <$> many' (ipChar
                                 <|> singleton <$> iPrivate
                                 <|> singleton <$> satisfy (inClass "/?"))

iFragment :: Parser Text
iFragment = T.concat <$> many' (ipChar <|> singleton <$> satisfy (inClass "/?"))

iRelativePart :: Parser Text
iRelativePart = mappend <$> string "//"
                        <*> (mappend <$> iAuthority
                                     <*> iPathAbEmpty)
            <|> iPathAbsolute
            <|> iPathNoScheme
            <|> iPathEmpty

-- | Parser for an IRI
iri :: Parser IRI
iri = AbsoluteIRI <$> scheme <*. ":"
                  <*> iHierPart
                  <*> option Nothing (Just <$> "?" .*> iQuery)
                  <*> option Nothing (Just <$> "#" .*> iFragment)

-- | Parser for a relative IRI reference
iRelativeRef :: Parser IRI
iRelativeRef = RelativeIRI <$> iRelativePart
                           <*> option Nothing (Just <$> "?" .*> iQuery)
                           <*> option Nothing (Just <$> "#" .*> iFragment)

-- | Parser for an IRI reference
iriReference :: Parser IRI
iriReference = iri
               <|> iRelativeRef
