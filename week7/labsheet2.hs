import Data.Char
import Data.Ord
import Data.List.Split 

inRange :: Int -> Int -> [Int] -> [Int]
inRange min max [] = []
inRange min max (x:xs) | min <= x && x <= max = x:(inRange min max xs)
    | otherwise = inRange min max xs
    
countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = if x > 0 
    then 1 + countPositives xs
    else 0 + countPositives xs


makeLower :: [Char] -> [Char]
makeLower [] = []
makeLower (x:xs) = (toLower x):(makeLower xs)

capitalised :: String -> String
capitalised (x:xs) = (toUpper x):(makeLower xs)


-- work in progress
title :: String -> String
title x = capitalised x

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 xs = newElement:xs
insertAt newElement i (x:xs) = x : insertAt newElement (i - 1) xs

insertionSort :: Ord a => [a] -> [a]
insertionSort list = []

-- need to make a new list as lists are immutable

qsort :: [Int] -> [Int]
qsort [] = []
-- take the pivot as the first part.
-- if more than pivot, prepend the pivot 
-- if less than the pivot leave it. 
-- concatonate the lists 
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
    where ls = [a | a <- xs, a <= x]
          rs = [b | b <- xs, b > x]

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) | n < x = n : x : xs
    | otherwise = x: insert n xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (x:xs) (y:ys) | x > y = merge (y:x:xs) ys
    | x <= y = merge (x:y:xs) ys
    | otherwise = merge (x:xs) (y:ys)

rotor :: Int -> [Char] -> [Char]
rotor n [] = []
rotor 0 x = x
rotor n (x:xs) = if n >= 0 && n < length (x:xs)
    then rotor (n-1) (xs ++ [x])
    else error "Shift out of range of list"


encipher :: Int -> Char -> Char
encipher n c = chr((mod ((ord c - ord 'A') + n) 26) + ord 'A')
    
makeKey :: Int -> [(Char, Char)]
makeKey n = [(c, encipher n c)| c <- ['A'..'Z']] 


lookUp :: Char -> [(Char, Char)] -> Char
lookUp c chars = [c2 | (c1, c2) <- chars, c1 == c] !! 0


normalise :: [Char] -> [Char]
normalise cs = [toUpper c | c <- cs, not (c `elem` ",/.?!-:;\" \'")]

encipherString :: Int -> [Char] -> [Char]
encipherString n cs = [encipher n c | c <- normalise cs]
