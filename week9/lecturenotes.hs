data Q a = Q [a] deriving (Eq, Show)
data Q2 a = Q2 [a] [a] deriving (Eq, Show)

{-
adding to the end of the list is very slow, so 
we split the list into 2 parts, with the second part
being reversed meaning we can append to the head of 
the first list.
add to the back, remove from front -> O(1) not O(n)
using cons (:) makes complexity constant

e.g. Queue [1,2,3] [6,5,4] is in reality a Queue of 1-6
-}

empty = Q []
add x (Q xs) = Q (xs ++ [x])
remove (Q (x:xs)) = Q xs
front (Q (x:xs)) = x
isEmpty (Q xs) = null xs

-- if the front gets empty, replace with the reversed back
fixQ (Q [] back) = Q (reverse back) []
-- if the first list isnt empty, return the q
fixQ q = q
empty' = Q2 [] []
isEmpty' = q = q == empty
-- adding to the front of the reversed back Q
add' x (Q2 front back) = fixQ (Q2 front (x:back))
front' (Q2 (x:front) back) = x
remove' (Q2 (x:front) back) = fixQ (Q2 front back)


