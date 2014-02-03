{-# LANGUAGE OverloadedStrings #-}

module Hazel.TestCases
       where

import Data.Set ( Set
                , fromList
                )
import Data.Text (pack)

import Hazel.Core
import Hazel.Normalize (normalize)
import Hazel.Completion (Node)

toDummy :: GCI -> Concept
toDummy = Dummy . pack . show

role :: Role
role = Role "hasChild"

top, name, conjunction, existential :: Concept
top = Top
name = Name "Person"
conjunction = top `And` name
existential = Exists role name

allNodes :: [Node]
allNodes = [Name "Person", Name "Father", Name "Dummy", Name "Human"]

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

cr1Gci :: GCI
cr1Gci = Subclass top name

cr2Gci :: GCI
cr2Gci = Subclass conjunction (Name "Human")

cr3Gci :: GCI
cr3Gci = Subclass name existential

cr4Gci :: GCI
cr4Gci = Subclass existential (Name "Father")

a :: Concept
a = Name "A"

b :: Concept
b = Name "B"

c :: Concept
c = Name "C"

d :: Concept
d = Name "D"

r :: Role
r = Role "r"

exercise38 :: [GCI]
exercise38 =
    [ Subclass a $ Exists r c `And` b
    , Subclass c $ Exists r a `And` b
    , Subclass (Exists r (Exists r b) `And` d) $ Exists r $ And b a
    , Subclass (Exists r b `And` b) (d `And` c)
    ]

normalized38 :: TBox
normalized38 = normalize exercise38

topTest :: [GCI]
topTest =
    [ Subclass Top a
    , Subclass a b
    ]
