-- Q1
squares :: (Num a) => [a] -> [a]
squares = map (^2)

sumSquares :: (Num a) => [a] -> a
sumSquares = sum . squares

gtZero :: (Num a, Ord a) => [a] -> Bool
gtZero = all (>0)

-- Q2
minFunc :: (Integral a, Ord b) => (a -> b) -> a -> b
minFunc f n = minimum . map f $ [0..n]

equalFunc :: (Integral a, Eq b) => (a -> b) -> a -> Bool
equalFunc f n = let result = f 0
                in all ((== result) . f) [0..n]

gtZeroFunc :: (Integral a, Num b, Ord b) => (a -> b) -> a -> Bool
gtZeroFunc f n = all ((> 0) . f) [0..n]

increasingFunc :: (Integral a, Ord b) => (a -> b) -> a -> Bool
increasingFunc _ (-1) = True
increasingFunc f n = f n > f (n - 1) && increasingFunc f (n - 1)

-- Q3
twice :: (a -> a) -> (a -> a)
twice f = f . f

-- Q4
iter :: (Integral a) => a -> (b -> b) -> b -> b
iter 0 f = id
iter n f = f . iter (n - 1) f

-- Q5
twoToTheN :: (Integral a) => a -> a
twoToTheN a = iter a (*2) 1

-- Q6
data RhType = Positive | Negative deriving (Show, Eq)
data ABOType = A | B | AB | O deriving (Show, Eq)
data BloodType = BloodType ABOType RhType deriving (Show, Eq)

patients = BloodType O Negative : [BloodType t r | t <- [A, AB], r <- [Positive, Negative]]

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType d _) (BloodType r _)
    | d == O = True
    | d == AB = r == AB
    | r == AB = True
    | r == d = True
    | otherwise = False

-- Q7
data Answer = Yes | No | Unknown

wonky :: Answer -> Answer
wonky Yes = No
wonky No = Unknown
wonky Unknown = Yes

-- Q8
data Shape = Ellipse Float Float deriving Show

circle :: Float -> Shape
circle r = Ellipse r r

area :: Shape -> Float
area (Ellipse a b) = 3.1415 * a * b