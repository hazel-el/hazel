import Hazel.Core
import Hazel.Completion
import Hazel.Normalize
-- import Data.Set

role = Role "hasChild"
top = Top
name = Name "Person" True
conjunction = top `And` name
existential = Exists role name
gci = Subclass existential name
tbox = tBoxFromList [gci, Subclass top conjunction]
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

(CGraph sInit rInit) = initGraph

gciCr1 = Subclass top name
(CGraph sCr1 rCr1, flagCr1) = cr1 gciCr1 initGraph top
(CGraph tCr1 qCr1, glagCr1) = cr1 gciCr1 initGraph name

gciCr2 = Subclass conjunction (Name "Human" True)
(CGraph sCr2 rCr2, flagCr2) = cr2 gciCr2 initGraph name

gciCr3 = Subclass name existential
(CGraph sCr3 rCr3, flagCr3) = cr3 gciCr3 initGraph name

gciCr4 = Subclass existential (Name "Father" True)
(CGraph sCr4 rCr4, flagCr4) = cr4 gciCr4 (CGraph sCr3 rCr3) name name

showNames (TBox gs sc sr) = "(" ++ show sc ++ ", " ++ show sr ++ ")"

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
    print $ showNames $ normalizeGCI gci3b `tBoxUnion` normalizeGCI gci5c
    putStrLn "\n Testing TBox normalization"
    print $ normalize [gci3b, gci5c]
    print $ normalizeGCI gci3b `tBoxUnion` normalizeGCI gci5c
    putStrLn "\nTesting Completion Graph Initialization"
    print $ sInit (Name "Person" True)
    putStrLn "\nTesting Completion Rules"
    print $ "Applying CR1 to <" ++ show gciCr1 ++ ">, Top, and initGraph"
    print $ "New successor found: " ++ show flagCr1
    print "New successors of Top:"
    print $ sCr1 Top
    print $ "Applying CR1 to <" ++ show gciCr1 ++ ">, Person, and initGraph"
    print $ "New successor found: " ++ show glagCr1
    print "New successors of Person:"
    print $ tCr1 name
    print "New successors of Top:"
    print $ tCr1 top
    print $ "Applying CR2 to <" ++ show gciCr2 ++ ">, Person, and initGraph"
    print $ "New successor found: " ++ show flagCr2
    print "New successors of Person:"
    print $ sCr2 name
    print "New successors of Top:"
    print $ sCr2 top
    print "New successors of Dummy:"
    print $ sCr2 (Name "Dummy" True)
    print $ "Applying CR3 to <" ++ show gciCr3 ++ ">, Person, and initGraph"
    print $ "New role pair found: " ++ show flagCr3
    print "New pairs for hasChild"
    print $ rCr3 role
    print "New pairs for marriedTo"
    print $ rCr3 (Role "marriedTo")
    print $ "Applying CR4 to <" ++ show gciCr4 ++ ">, Person, and result of previous application"
    print $ "New successors found: " ++ show flagCr4
    print "New successors for Person"
    print $ sCr4 name
    print "New pairs for Top"
    print $ sCr4 top
