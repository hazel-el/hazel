{-# LANGUAGE OverloadedStrings #-}

import Hazel.Core
import Hazel.Completion
import Hazel.Normalize
-- import Data.Set

role = Role "hasChild"
top = Top
name = Name "Person"
conjunction = top `And` name
existential = Exists role name
gci = Subclass existential name
tbox = tBox_from_list [gci, Subclass top conjunction]
gci2 = Subclass (And name existential) name
gci2b = Subclass (And name conjunction) name
gci3 = Subclass (Exists role existential) name
gci3b = Subclass (Exists role conjunction) name
gci4 = Subclass name (Exists role existential)
gci4b = Subclass name (Exists role conjunction)
gci5 = Subclass existential conjunction
gci5b = Subclass existential existential
gci5c = Subclass conjunction existential
gci5d = Subclass conjunction conjunction
gci6 = Subclass top (And name top)

(CGraph s_init r_init) = init_graph

gci_cr1 = Subclass top name
(CGraph s_cr1 r_cr1, flag_cr1) = cr1 gci_cr1 init_graph top
(CGraph t_cr1 q_cr1, glag_cr1) = cr1 gci_cr1 init_graph name

gci_cr2 = Subclass conjunction (Name "Human")
(CGraph s_cr2 r_cr2, flag_cr2) = cr2 gci_cr2 init_graph name

gci_cr3 = Subclass name existential
(CGraph s_cr3 r_cr3, flag_cr3) = cr3 gci_cr3 init_graph name

gci_cr4 = Subclass existential (Name "Father")
(CGraph s_cr4 r_cr4, flag_cr4) = cr4 gci_cr4 (CGraph s_cr3 r_cr3) name name

show_names (TBox gs sc sr) = "(" ++ show sc ++ ", " ++ show sr ++ ")"

main = do
    putStrLn "\nTesting Show Functions"
    print role
    print top
    print name
    print conjunction
    print existential
    print gci
    print tbox
    putStrLn "\nTesting Normalization for GCIs"
    print gci2
    print $ normalizeGCI gci2
    print gci2b
    print $ normalizeGCI gci2b
    print gci3
    print $ normalizeGCI gci3
    print gci3b
    print $ normalizeGCI gci3b
    print gci4
    print $ normalizeGCI gci4
    print gci4b
    print $ normalizeGCI gci4b
    print gci5
    print $ normalizeGCI gci5
    print gci5b
    print $ normalizeGCI gci5b
    print gci5c
    print $ normalizeGCI gci5c
    print gci5d
    print $ normalizeGCI gci5d
    print gci6
    print $ normalizeGCI gci6
    putStrLn "\n Testing Signature Computation"
    putStrLn $ show_names $ normalizeGCI gci3b `tBox_union` normalizeGCI gci5c
    putStrLn "\n Testing TBox normalization"
    print $ normalize [gci3b, gci5c]
    print $ normalizeGCI gci3b `tBox_union` normalizeGCI gci5c
    putStrLn "\nTesting Completion Graph Initialization"
    print $ s_init (Name "Person")
    putStrLn "\nTesting Completion Rules"
    putStrLn $ "Applying CR1 to <" ++ show gci_cr1 ++ ">, Top, and init_graph"
    putStrLn $ "New successor found: " ++ show flag_cr1
    putStrLn $ "New successors of Top:"
    print $ s_cr1 Top
    putStrLn $ "Applying CR1 to <" ++ show gci_cr1 ++ ">, Person, and init_graph"
    putStrLn $ "New successor found: " ++ show glag_cr1
    putStrLn $ "New successors of Person:"
    print $ t_cr1 name
    putStrLn $ "New successors of Top:"
    print $ t_cr1 top
    putStrLn $ "Applying CR2 to <" ++ show gci_cr2 ++ ">, Person, and init_graph"
    putStrLn $ "New successor found: " ++ show flag_cr2
    putStrLn $ "New successors of Person:"
    print $ s_cr2 name
    putStrLn $ "New successors of Top:"
    print $ s_cr2 top
    putStrLn $ "New successors of Dummy:"
    print $ s_cr2 (Name "Dummy") -- FIXME: shouldn't this be a Dummy instead?
    putStrLn $ "Applying CR3 to <" ++ show gci_cr3 ++ ">, Person, and init_graph"
    putStrLn $ "New role pair found: " ++ show flag_cr3
    putStrLn $ "New pairs for hasChild"
    print $ r_cr3 role
    putStrLn $ "New pairs for marriedTo"
    print $ r_cr3 (Role "marriedTo")
    putStrLn $ "Applying CR4 to <" ++ show gci_cr4 ++ ">, Person, and result of previous application"
    putStrLn $ "New successors found: " ++ show flag_cr4
    putStrLn $ "New successors for Person"
    print $ s_cr4 name
    putStrLn $ "New pairs for Top"
    print $ s_cr4 top
