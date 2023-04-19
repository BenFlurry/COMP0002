import Data.Char

f0 :: Num a => a -> a
f0 x = x*x

linearSearch :: (Eq a) => [a] -> a -> Bool
linearSearch [] y = False
linearSearch (x:xs) y = if x == y then True else linearSearch xs y

map1 :: (a -> a) -> [a] -> [a]
map1 f [] = []
map1 f (x:xs) = (f x):(map1 f xs)

map12 :: (a -> a) -> [a] -> [a]
map12 f [] = []
map12 f xs = [f x | x <- xs]


shift_char :: Int -> Char -> Char
shift_char y x = chr((mod(((ord x) - 65) + y) 26) + 65)

ceasar_cipher :: String -> Int -> String
ceasar_cipher xs y = map12 (shift_char y) xs

count_evens :: [Int] -> Int
count_evens [] = 0
count_evens (x:xs)
  | (mod x 2) == 0 = 1 + count_evens xs
  | otherwise = count_evens xs

count_evens2 :: [Int] -> Int
count_evens2 [] = 0
count_evens2 xs = length [x | x <- xs, mod x 2 == 0 ]

main :: IO()
main = putStr (show (f0 5))