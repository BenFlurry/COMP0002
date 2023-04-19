mult :: Num a => [a] -> a
mult xs = foldr (*) 1 xs

sum' xs = foldr (+) 0 xs

-- return the sum of squares of a list
sumsq :: [Int] -> Int
sumsq xs = foldr (+) 0 (map (\x->x^2) (filter (\x->x>0) xs))

-- return positive integers in a list
posList :: [Int] -> [Int]
posList xs = filter (\x -> x > 0) xs

-- determine whether all bools in a list are true
trueList :: [Bool] -> Bool
trueList xs = (filter (\x -> x==True) xs) == xs

-- determine whether all numbers are even
evenList :: [Int] -> Bool
evenList xs = (filter (\x -> x `mod` 2 == 0) xs) == xs

-- find the max of a list of items that can be ordered
maxList :: Ord a => [a] -> a
maxList xs = foldr1 (max) xs

-- return the numbers inbetween the range specified in the list
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = filter p xs
    where p x = x >= lo && x <= hi

-- does what it says on the tin
countPositives :: [Int] -> Int
countPositives xs = length (filter p xs)
    where p x = x > 0

-- define length using foldr and map
myLength :: Num a => [a] -> Int
myLength xs = foldr (+) 0 (map (\x -> 1) xs)

-- define map using foldr
-- myMap :: [a] -> [a]
-- myMap f xs = 

-- define length using foldr
myLength' :: Num a => [a] -> Int
myLength' xs = foldr (+) 0 (map (\x -> 1) xs)

