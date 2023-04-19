import Data.Char

square :: Int -> Int
square x = x^2

pyth :: Int -> Int -> Int
pyth x y = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = if pyth a b == square c
    then True
    else False

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c = do
    if pyth a b /= square c then 
        if pyth b c /= square a then
            if pyth a c /= square b then
                False
            else True
        else True
    else True


halfEven :: Int -> Int
halfEven x = if mod x 2 == 0 then div x 2 else x

halfEvens :: Int -> Int
halfEvens l = map halfEven l


inRange :: Int -> Int -> [Int] -> [Int]
inRange min max list = drop (min-1) (take (max-min+2) list)


countPositives :: [Int] -> Int
countPositives l = length [x | x <- l, x > 0] 


-- do funciton signatures otherwise inferred by ghc

-- capitalised :: String -> String
-- capitalised l = do
--     let first = toUpper head l 
--     drop 1 l
--     map toLower l
--     first ++ l


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

doubleDouble' :: Int -> Int
doubleDouble' x = (\y -> y * 2)x * 2


