-- using if statements
abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- using guarded functions
abs' :: Int -> Int
abs' n | n >= 0 = n
    | otherwise = -n

signum :: Int -> Int
signum n | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1


-- using where to declare a function inside a function
doubleDouble :: Int -> Int
doubleDouble x = dubs * 2
    where dubs = x * 2

-- doing the same function just with lambdas
doubleDouble' :: Int -> Int
doubleDouble' x = (\dubs -> dubs * 2)x * 2

-- basic adding function
add :: Int -> Int -> Int
add x y = x + y

-- using a lambda expression to show how haskell takes in arguments
-- 1 at a time
-- add' :: Integer -> Integer -> Integer
-- add' x y = (\x -> (\y -> x + y))


-- write a function which returns the tail, or empty list if empty
-- using selection
safetail :: [a] -> [a]
safetail l = if null l then [] else tail l

-- using gaurded function
safetail' :: [a] -> [a]
safetail' l | null l = []
    | otherwise = tail l

-- using pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
-- finds returns everything but the first element
safetail'' (x:xs) = xs

-- . operator in hs

{-
f x = x + y*y where y = x + 1

get a list of int and return x < 5
-} 


func :: Int -> Int
func x = x + y^2 where y = x + 1
--(/x -> x + (\y -> x + 1)*(\y -> x + 1))

fn :: [Int] -> [Int]
fn l = filter (<5) l
-- or fn = filter (<5) because it can curry and curry is good

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum'(xs)

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
-- prepend n at the point where its smaller than x
-- otherwise prepend the head of the list to the 
-- recursive call of insert function of the tail of the list
insert n (x:xs) | n < x = n : x : xs
    | otherwise = x: insert n xs



