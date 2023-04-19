square :: Num a => [a] -> [a]
square = map (^2)


sumsq :: [Int] -> Int
sumsq ns = sum $ square ns


positive :: [Int] -> Bool
positive ns = all (>0) ns


minmap :: (Integral a, Ord b) => (a -> b) -> [a] -> b
minmap f ns = minimum $ map f ns


-- Q2
minfunc :: (Integral a, Ord b) => (a -> b) -> a -> b
minfunc f n = minimum . map f $ [0..n]

-- iseq :: (Integral a, Ord b) => (a -> b) -> a -> b
-- iseq f n = 


twice :: (a -> a) -> (a -> a)
twice f = f.f

iter :: (Integral a) => a -> (b -> b) -> b -> b
iter 0 f = id
iter n f = f . iter (n-1) f

double :: (Integral a) => a -> a
double n = 2 * n

binaryshift :: (Integral a) => a -> a
binaryshift 0 = 1
binaryshift n = iter n double 1


data RhType = Positive | Negative deriving (Show, Eq)
data ABOType = A | B | AB | O deriving (Show, Eq)
data BloodType = BloodType ABOType RhType deriving (Show, Eq)

patients = BloodType O Negative : [BloodType t r | t <- [A, AB], r <- [Positive, Negative]]

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType t1 _) (BloodType t2 _)
    | t1 == O = True
    | t1 == AB = t2 == AB
    | t2 == AB = True
    | t1 == t2 = True
    | otherwise = False

data Answer = Yes | No | Unknown deriving (Show, Eq)

wonky :: Answer -> Answer
wonky Yes = Unknown
wonky No = Yes
wonky Unknown = No




