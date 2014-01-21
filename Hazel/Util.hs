module Hazel.Util where

import Data.List
import Control.Monad.State
        
-- helper functions       

-- inverts a list-valued function
inv :: (Eq a,Eq b)
    => [a]         -- domain list
    -> (a -> [b])  -- list-valued function
    -> b 	   -- codomain element
    -> [a]         -- sublist of domain
inv d f y = [x|x<-d, elem y (f x)]

-- modifies a function with one substituted value
--except :: (Eq a)
--       => (a -> b) -- input function
--       -> a        -- domain element
--       -> b        -- new value on specified domain element
--       -> (a -> b) -- output function
--except f x y = \z -> if (z==x) then y else (f x)
--except f x y z | x==z      = y
--               | otherwise = f x

extend :: (Eq a,Eq b)
       => (a -> [b])
       -> a
       -> [b]
       -> (a -> [b])
extend f x y z | x==z      = (f x) ++ y
	       | otherwise = f x
--insert f x y = except f x ((f x) ++ y)

-- monadic while expression
-- runs the given State unless the condition returns False
whileM :: (a -> Bool) -> State s a -> State s a
whileM condition state = do
	x <- state
	if (condition x) then whileM condition state
	else return x

whileNotM :: (a -> Bool) -> State s a -> State s a
whileNotM condition state = whileM (not.condition) state

containsSame :: (Eq a) => [a] -> [a] -> Bool
--containsSame x y = (subListOf x y) && (subListOf y x)
containsSame x y = ((length x')==(length y')) && (subListOf x' y')
                   where (x',y') = (nub x,nub y)

subListOf :: (Eq a) => [a] -> [a] -> Bool
subListOf [] _ = True
subListOf (x:xs) y = (elem x y) && (subListOf xs y)
