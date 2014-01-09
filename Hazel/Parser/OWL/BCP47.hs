{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.OWL.BCP47
Copyright   :  (c) 2013, 2013 Maximilian Marx
License     :  GPL-3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser for the parts of BCP 47 referenced in the OWL2 Functional-Style grammar.
-}

module Hazel.Parser.OWL.BCP47 ( LanguageTag (..)
                              , langTag
                              ) where

import Prelude hiding (takeWhile)
import Control.Applicative ( (<|>)
                           , (<$>)
                           , (<*>)
                           )
import Control.Monad (liftM)
import Data.Monoid (mappend)
import Data.Text ( Text
                 , cons
                 , pack
                 , unpack
                 )
import qualified Data.Text as T
import Data.Attoparsec.Text

import Hazel.Parser.Utils (countFromTo)
import Hazel.Parser.OWL.RFC5234 (alpha)

-- | BCP 47 Language Tag
data LanguageTag = LanguageTag { languageTagLanguage :: Text -- ^ language
                               , languageTagScript :: Maybe Text -- ^ script
                               , languageTagRegion :: Maybe Text -- ^ region
                               , languageTagVariant :: [Text]    -- ^ variants
                               , languageTagExtension :: [Text]  -- ^ extensions
                               , languageTagPrivateUse :: Maybe Text -- ^ reserved for private use
                               }

instance Show LanguageTag where
  show lt = concatMap unpack [ languageTagLanguage lt
                             , fromMaybe $ languageTagScript lt
                             , fromMaybe $ languageTagRegion lt
                             , fromList $ languageTagVariant lt
                             , fromList $ languageTagExtension lt
                             , fromMaybe $ languageTagPrivateUse lt
                             ]
    where fromMaybe = maybe "" prefix
          fromList = T.concat . map prefix
          prefix = cons '-'

alphanum :: Parser Char
alphanum = satisfy $ inClass "a-zA-Z0-9"

singleton :: Parser Char
singleton = satisfy $ inClass $ concat [ "0-9"
                                       , "A-W"
                                       , "Y-Z"
                                       , "a-w"
                                       , "y-z"
                                       ]

-- | Parser for BCP 47 Language Tags
langTag :: Parser LanguageTag
langTag = LanguageTag <$> language
                      <*> option Nothing (liftM Just script)
                      <*> option Nothing (liftM Just region)
                      <*> many' variant
                      <*> many' extension
                      <*> option Nothing (liftM Just privateUse)

language :: Parser Text
language = mappend <$> liftM pack (countFromTo 2 3 alpha) <*> option "" extlang
           <|> liftM pack (count 4 alpha)
           <|> liftM pack (countFromTo 5 8 alpha)

extlang :: Parser Text
extlang = mappend <$> liftM pack (count 3 alpha) <*> exts
  where exts = T.concat <$> countFromTo 0 2 (pack <$> ((:) <$> char '-' <*> count 3 alpha))

script :: Parser Text
script = liftM pack $ (:) <$> char '-' <*> count 4 alpha

region :: Parser Text
region = liftM pack $ (:) <$> char '-' <*> count 2 alpha <|> count 3 digit

variant :: Parser Text
variant = pack <$> (countFromTo 5 8 alphanum <|> (:) <$> digit
                                                     <*> count 3 alphanum)

extension :: Parser Text
extension = cons <$> singleton <*> extensions
  where extensions = T.concat <$> many1 (liftM pack ((:) <$> char '-'
                                                         <*> countFromTo 2 8 alphanum))

privateUse :: Parser Text
privateUse = cons <$> char 'x' <*> privates
  where privates = T.concat <$> many1 (liftM pack ((:) <$> char '-'
                                                       <*> countFromTo 1 8 alphanum))
