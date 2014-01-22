{-# LANGUAGE BangPatterns, OverloadedStrings, ViewPatterns #-}

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
                                , showIRI
                                ) where

import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Data.Monoid ( mappend
                   , (<>)
                   )
import Data.Text ( Text
                 , cons
                 , pack
                 , singleton
                 , snoc
                 , unpack
                 )
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT

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

showIRI :: IRI -> Text
showIRI (AbsoluteIRI ischeme hierPart query fragment) = T.concat [ ischeme
                                                                 , ":"
                                                                 , hierPart
                                                                 , maybe "" (cons '?') query
                                                                 , maybe "" (cons '#') fragment
                                                                 ]
showIRI (RelativeIRI relativePart query fragment) = T.concat [ relativePart
                                                             , maybe "" (cons '?') query
                                                             , maybe "" (cons '#') fragment
                                                             ]

instance Show IRI where
  show = unpack . showIRI

isUcsChar :: Char -> Bool
isUcsChar = inClass $ concat [ "\xA0-\xD7FF"
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

isSubDelim :: Char -> Bool
isSubDelim '!' = True
isSubDelim '$' = True
isSubDelim '&' = True
isSubDelim '\'' = True
isSubDelim '(' = True
isSubDelim ')' = True
isSubDelim '*' = True
isSubDelim '+' = True
isSubDelim ',' = True
isSubDelim ';' = True
isSubDelim '=' = True
isSubDelim _ = False

subDelims :: Parser Char
subDelims = satisfy isSubDelim

isIPrivate :: Char -> Bool
isIPrivate c = ('\xE000' <= c && c <= '\xF8FF')
               || ('\xF0000' <= c && c <= '\xFFFFD')
               || ('\x100000' <= c && c <= '\x10FFFD')

unreserved :: Parser Char
unreserved = alpha
             <|> digit
             <|> satisfy (inClass "._~-")

isIUnreserved :: Char -> Bool
isIUnreserved c = ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '9')
                  || isOther c
  where isOther '.' = True
        isOther '_' = True
        isOther '~' = True
        isOther '-' = True
        isOther d = isUcsChar d
        {-# INLINE isOther #-}

isIpChar' :: Char -> Bool
isIpChar' c = isIUnreserved c
              || isSubDelim c
              || c == ':'
              || c == '@'

scheme :: Parser Text
scheme = cons <$> alpha
              <*> AT.takeWhile isChr
              <?> "scheme"
  where isChr c = ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '9')
                  || c == '+'
                  || c == '.'
                  || c == '-'
        {-# INLINE isChr #-}

iUserInfo :: Parser Text
iUserInfo = iSegment' isChr
  where isChr c = c == ':' || isSubDelim c || isIUnreserved c
        {-# INLINE isChr #-}

port :: Parser Text
port = pack <$> many' digit

ipLiteral :: Parser Text
ipLiteral = cons <$> char '['
                 <*> (snoc <$> (ipv6Address <|> ipvFuture)
                           <*> char ']')

iRegName :: Parser Text
iRegName = iSegment' isChr
  where isChr c = isSubDelim c || isIUnreserved c
        {-# INLINE isChr #-}

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


iSegment' :: (Char -> Bool) -> Parser Text
iSegment' predicate = do
  prefix <- AT.takeWhile predicate
  c <- peekChar'
  case c of
    '%' -> do
      pct <- pctEncoded
      let !prefix' = prefix <> pct
      rest <- iSegment
      return $! prefix' <> rest
    _ -> return $! prefix

iSegment1' :: (Char -> Bool) -> Parser Text
iSegment1' predicate = do
  c <- peekChar'
  prefix <- case c of
    '%' -> pctEncoded
    (predicate -> True) -> AT.take 1
    _ -> fail "iSegment1'"
  rest <- iSegment' predicate
  return $! prefix <> rest

iSegment :: Parser Text
iSegment = iSegment' isIpChar'

iSegmentNz :: Parser Text
iSegmentNz = iSegment1' isIpChar'

iSegmentNzNc :: Parser Text
iSegmentNzNc = iSegment1' isChr
  where isChr c = c == '@' || isSubDelim c || isIUnreserved c
        {-# INLINE isChr #-}

iPathAbEmpty :: Parser Text
iPathAbEmpty = do
  c <- peekChar'
  case c of
    '/' -> do
      slash <- AT.take 1
      segment <- iSegment
      let !prefix = slash <> segment
      d <- peekChar'
      case d of
        '/' -> do
          rest <- iPathAbEmpty
          return $! prefix <> rest
        _ -> return $! prefix
    _ -> return $! ""

iPathAbsolute :: Parser Text
iPathAbsolute = cons <$> char '/'
                     <*> option "" iPathRootless

iPathRootless :: Parser Text
iPathRootless = mappend <$> iSegmentNz
                        <*> iPathAbEmpty

iPathNoScheme :: Parser Text
iPathNoScheme = mappend <$> iSegmentNzNc
                        <*> iPathAbEmpty

iPathEmpty :: Parser Text
iPathEmpty = return $! ""

iHierPart :: Parser Text
iHierPart = do
  c <- peekChar'
  case c of
    '/' -> mappend <$> string "//"
                  <*> (mappend <$> iAuthority
                               <*> iPathAbEmpty)
         <|> iPathAbsolute
    _ -> iPathRootless
         <|> iPathEmpty

iQuery :: Parser Text
iQuery = iSegment' isChr
  where isChr c = isIpChar' c
                  || isIPrivate c
                  || c == '/'
                  || c == '?'
        {-# INLINE isChr #-}

iFragment :: Parser Text
iFragment = iSegment' isChr
  where isChr c = c == '/' || c == '?' || isIpChar' c
        {-# INLINE isChr #-}

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
