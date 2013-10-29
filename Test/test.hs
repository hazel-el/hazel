import Hazel.Core
import Hazel.Completion
-- import Data.Set

role = Role "hasChild"
top = Top
name = Name "Person" True
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

show_names (TBox gs sc sr) = "(" ++ show sc ++ ", " ++ show sr ++ ")"

main = do
    putStrLn $ show role
    putStrLn $ show top
    putStrLn $ show name
    putStrLn $ show conjunction
    putStrLn $ show existential
    putStrLn $ show gci
    putStrLn $ show tbox
    putStrLn $ show gci2
    putStrLn $ show $ normalizeGCI gci2
    putStrLn $ show gci2b
    putStrLn $ show $ normalizeGCI gci2b
    putStrLn $ show gci3
    putStrLn $ show $ normalizeGCI gci3
    putStrLn $ show gci3b
    putStrLn $ show $ normalizeGCI gci3b
    putStrLn $ show gci4
    putStrLn $ show $ normalizeGCI gci4
    putStrLn $ show gci4b
    putStrLn $ show $ normalizeGCI gci4b
    putStrLn $ show gci5
    putStrLn $ show $ normalizeGCI gci5
    putStrLn $ show gci5b
    putStrLn $ show $ normalizeGCI gci5b
    putStrLn $ show gci5c
    putStrLn $ show $ normalizeGCI gci5c
    putStrLn $ show gci5d
    putStrLn $ show $ normalizeGCI gci5d
    putStrLn $ show gci6
    putStrLn $ show $ normalizeGCI gci6
    putStrLn $ show_names $ normalizeGCI gci3b `tBox_union` normalizeGCI gci5c
