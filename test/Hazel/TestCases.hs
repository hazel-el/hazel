{-# LANGUAGE OverloadedStrings #-}

module Hazel.TestCases
       where

import Data.Set ( Set
                , fromList
                )
import Data.Text (pack)

import Hazel.Core

toDummy :: GCI -> Concept
toDummy = Dummy . pack . show

role :: Role
role = Role "hasChild"

top, name, conjunction, existential :: Concept
top = Top
name = Name "Person"
conjunction = top `And` name
existential = Exists role name

gci :: GCI
gci = Subclass existential name

tbox :: TBox
tbox = gcisToTBox [gci, Subclass top conjunction]

gci2 :: GCI
gci2 = Subclass (And name existential) name

gci2' :: TBox
gci2' = gcisToTBox [ Subclass (Exists role name) dummy
                   , Subclass (And dummy name) name
                   ]
  where dummy = toDummy gci2

gci2b :: GCI
gci2b = Subclass (And name conjunction) name

gci2b' :: TBox
gci2b' = gcisToTBox [ Subclass conjunction dummy
                    , Subclass (And dummy name) name
                    ]
  where dummy = toDummy gci2b

gci3 :: GCI
gci3 = Subclass (Exists role existential) name

gci3' :: TBox
gci3' = gcisToTBox [ Subclass existential dummy
                   , Subclass (Exists role dummy) name
                   ]
  where dummy = toDummy gci3

gci3b :: GCI
gci3b = Subclass (Exists role conjunction) name

gci3b' :: TBox
gci3b' = gcisToTBox [ Subclass conjunction dummy
                    , Subclass (Exists role dummy) name
                    ]
  where dummy = toDummy gci3b

gci4 :: GCI
gci4 = Subclass name (Exists role existential)

gci4' :: TBox
gci4' = gcisToTBox [ Subclass name (Exists role dummy)
                   , Subclass dummy existential
                   ]
        where dummy = toDummy gci4

gci4b :: GCI
gci4b = Subclass name (Exists role conjunction)

gci4b' :: TBox
gci4b' = gcisToTBox [ Subclass dummy name
                    , Subclass name (Exists role dummy)
                    ]
  where dummy = toDummy gci4b

gci5 :: GCI
gci5 = Subclass existential conjunction

gci5' :: TBox
gci5' = gcisToTBox [ Subclass existential dummy
                   , Subclass dummy name
                   ]
  where dummy = toDummy gci5

gci5b :: GCI
gci5b = Subclass existential existential

gci5b' :: TBox
gci5b' = gcisToTBox [ Subclass existential dummy
                    , Subclass dummy existential
                    ]
  where dummy = toDummy gci5b

gci5c :: GCI
gci5c = Subclass conjunction existential

gci5c' :: TBox
gci5c' = gcisToTBox [ Subclass conjunction dummy
                    , Subclass dummy existential
                    ]
  where dummy = toDummy gci5c

gci5d :: GCI
gci5d = Subclass conjunction conjunction

gci5d' :: TBox
gci5d' = gcisToTBox [ Subclass conjunction dummy
                    , Subclass dummy name
                    ]
  where dummy = toDummy gci5d

gci6 :: GCI
gci6 = Subclass top (And name top)

gci6' :: TBox
gci6' = gciToTBox $ Subclass top name

signature3b5c :: (Set Concept, Set Role)
signature3b5c = ( fromList [ name
                           , toDummy gci3b
                           , toDummy gci5c
                           ]
                , fromList [role]
                )

cr1_gci :: GCI
cr1_gci = Subclass top name

cr2_gci :: GCI
cr2_gci = Subclass conjunction (Name "Human")

cr3_gci :: GCI
cr3_gci = Subclass name existential

cr4_gci :: GCI
cr4_gci = Subclass existential (Name "Father")
