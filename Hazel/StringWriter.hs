module Hazel.StringWriter where

import Control.Monad.State


------------- StringState -------------

type StringState = State String (Maybe Bool)

eval :: StringState -> String
eval s = execState s ""

newLine :: StringState
append  :: String -> StringState
apply   :: (String -> String) -> StringState

newLine  = append "\n"
append s = apply (++s)
apply f  = get >>= put.(f$) >> return (Just True)

--newLine :: StringState
--newLine = do
--            t <- get
--            put $ t++"\n"
--            return True
-- get >>= put.(++"\n") >>= return True

--append :: String -> StringState
--append s = do
--           t <- get
--           put $ t++s
--           return True
-- get >>= put.(++s) >>= return True

--modify :: (String -> String) -> StringState
--modify f = do
--             t <- get
--             put $ f t
--             return True


