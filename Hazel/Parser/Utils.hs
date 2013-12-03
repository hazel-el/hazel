{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Hazel.Parser.Utils
Copyright   :  (c) 2013 Maximilian Marx
License     :  GPL3
Maintainer  :  Maximilian Marx <mmarx@wh2.tu-dresden.de>
Stability   :  experimental
Portability :  unknown

Parser utilities
-}

module Hazel.Parser.Utils ( (<<)
                          , countFromTo
                          ) where

import Control.Applicative ( Applicative (..)
                           , Alternative (..)
                           , (<$>)
                           , (<*>)
                           )
import Data.Attoparsec.Text

-- | Apply the given action between @from@ and @to@ times, returning every result.
countFromTo :: (Alternative m, Monad m) => Int -> Int -> m a -> m [a]
countFromTo from to action
  | from <= to = (++) <$> count from action <*> choice [ count n action | n <- [1..(to - from)] ]
  | otherwise = error "countFromTo: from > to."

-- | Sequence two actions, returning the first value
(<<) :: Monad m => m a -> m b -> m a
(<<) a b = a >>= (\value -> b >> return value)

infixl 1 <<
