module Lib
    ( splitList, break'
    ) where

-- splits a list based on a predicate into two lists, discarding the element that 
-- passed the test (the head of the second list)
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p xs = 
    case break p xs of
        (f, []) -> (f, [])
        (f, r) -> (f, tail r)

-- splits a list into multiple lists using break'
splitList  :: (a -> Bool) -> [a] -> [[a]]
splitList _ [] = []
splitList p xs = first:splitList p rest
    where
        (first, rest) = break' p xs
