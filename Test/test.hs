import Hazel.Core

role = Role "hasChild"
top = Top
name = Name "Person"
conjunction = top `And` name
existential = Exists role name
gci = Subclass existential name
tbox = [gci, Subclass top conjunction]

main = do
    putStrLn $ show role
    putStrLn $ show top
    putStrLn $ show name
    putStrLn $ show conjunction
    putStrLn $ show existential
    putStrLn $ show gci
    putStrLn $ show tbox
