module Hazel.Writer
where

import Hazel.Completion
import Hazel.Core
import Hazel.Hierarchy
import Hazel.StringWriter
import Control.Arrow
import Control.Monad.State
import System.Directory


------------- WRITE METHOD -------------

writeHierarchy :: FilePath -> Hierarchy -> IO ()
writeHierarchy filePath hierarchy = do
    fullPath <- resolveFilePath filePath
    writeFile fullPath (show hierarchy)
-- resolveFilePath >>= \fullPath -> writeFile fullPath (createDocument hierarchy)
-- resolveFilePath >>=         flip.writeFile          (createDocument hierarchy)



------------- DOCUMENT CREATION (using State monad) -------------

instance Show Hierarchy where
  show = write >>> eval

write :: Hierarchy -> StringState
write h = preamble "http://insert.title.here" >> imports >> annotations >> axioms >> eof
  where preamble    :: String -> StringState
              imports     :: StringState
              annotations :: StringState
              axioms      :: StringState
              eof         :: StringState
              preamble title = append "Ontology( <" >> append title >> append ">" >> newLine
              imports        = append "Import()" >> newLine
              annotations    = append "Annotation()" >> newLine
              axioms         = append (transform >>> show'' $ h) >> newLine
              eof            = append ")"

show'' :: [GCI'] -> String
show'' = unlines.(map show)

transform :: Hierarchy -> [GCI']
transform (Hierarchy parents equivalents domain) = foldl (fff) [] domain
  where fff :: [GCI'] -> Concept -> [GCI']
              fff gcis concept = gcis
                               ++(    parentGCIs concept)
                               ++(equivalentGCIs concept)
        parentGCIs     :: Concept -> [GCI']
              equivalentGCIs :: Concept -> [GCI']
        parentGCIs concept     = map        (SubClassOf concept) (parents concept)
              equivalentGCIs concept = map (EquivalentClasses concept) (equivalents concept)


------------- DATA STRUCTURES -------------

-- modified data structure for gcis
data GCI' =        SubClassOf Concept Concept
          | EquivalentClasses Concept Concept

instance Show GCI' where
  show (SubClassOf c d)        =        "SubClassOf( " ++ (show' c) ++ " " ++ (show' d) ++ " )"
  show (EquivalentClasses c d) = "EquivalentClasses( " ++ (show' c) ++ " " ++ (show' d) ++ " )"

--newtype Concept' = Concept' Concept
--instance Show Concept' where
--  show (Concept' Top)          = "owl:Thing"
--  show (Concept' (Name c))     = "<"++(show c)++">"
--  show (Concept' (Dummy d))    = error "cannot show dummies"
--                                                                / here I would want to call show,
--                                                                / but then the called method is
--                                                                / show from instance Show Concept
--  show (Concept' (And c d))    = "ObjectIntersectionOf( "++(++" )"
--  show (Concept' (Exists r c)) = undefined

show' :: Concept -> String
show' (And c d)    = "ObjectIntersectionOf( "++(show' c)++" "++(show' d)++" )"
show' (Exists r c) = "ObjectSomeValuesFrom( "++(show r)++" "++(show' c)++" )"
show' (Name c)     = "<"++(show c)++">"
show' (Dummy c)    = error "cannot show dummies"
show' Top          = "owl:Thing"


------------- AUXILIARY METHODS -------------

-- Resolves the given relative file path towards the current directory.
-- Caution: It does not check, whether the returned file path is valid or exists.
resolveFilePath :: FilePath -> IO FilePath
resolveFilePath filePath = getCurrentDirectory >>= return.(++filePath)

