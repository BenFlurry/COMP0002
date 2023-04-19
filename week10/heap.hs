-- heap.hs, heap with insertion implemented, removal is too painful and I CBA so
-- take it or leave it

-- Import this only for pretty trees
import Data.Tree

-- If you have never seen curly brackets in haskell before
-- then google: `Haskell Record syntax`
data Heap a
  = Empty { _height :: Int
          , _size :: Int
          }
  | HeapTree { _height :: Int
             , _size :: Int
             , _value :: a
             , _left :: (Heap a)
             , _right :: (Heap a)
             }
-- This function converts our tree into the tree from Data.Tree
-- We need this to make use of drawTree function, to implement our pretty Show instance
toTree :: Show a => Heap a -> Tree String
toTree (HeapTree _ _ v l r) = Node (show v) [toTree x | x <- [l, r], not (isEmpty x)]


instance Show a => Show (Heap a) where
  show h = drawTree(toTree h)

-- Shorthand for creating an empty node and setting height and size to 0
empty :: Heap a
empty = Empty 0 0

singleton :: a -> Heap a
singleton x = HeapTree 1 1 x empty empty

isEmpty :: Heap a -> Bool
isEmpty (Empty _ _) = True
isEmpty _  = False

-- if you want to use foldl then your insert must
-- take the heap first and then the value:
-- insert:: Heap a -> a -> Heap a
fromList :: Ord a => [a] -> Heap a
fromList xs = foldr (insert) empty xs

-- basically need to make sure that if we see a tree
-- then we can swap out values in a way that root is the smallest
-- if left is less than root, we swap
-- if right is less than root, we also swap,, else we don't do anything
bubbleUp :: Ord a => Heap a -> Heap a
bubbleUp (HeapTree h s v l r) =
  if isEmpty l
  then (HeapTree h s v l r)
  else if (_value l) < v
  then (HeapTree h s (_value l) (HeapTree (_height l) (_size l) v (_left l) (_right l)) r)
  else if isEmpty r
  then (HeapTree h s v l r)
  else if (_value r) < v
  then (HeapTree h s (_value r) l (HeapTree (_height r) (_size r) v (_left r) (_right r)))
  else (HeapTree h s v l r)

-- This WILL NOT make sense if you don't understand how heaps work
-- so watch a video about them first and then come back
-- NOT PERFECT LEFT -> LEFT
-- PERFECT LEFT, NOT PERFECT RIGHT -> RIGHT
-- BOTH PERFECT:
--   LEFT > RIGHT -> RIGHT
--   LEFT
insert :: Ord a => a -> Heap a -> Heap a
insert x (Empty _ _) = singleton x
insert x (HeapTree h s v l r) = let
  lh = _height l
  rh = _height r
  ls = _size l
  rs = _size r
  not_perfect_l = ls < 2^lh - 1
  not_perfect_r = rs < 2^rh - 1
  in if not_perfect_l
  then bubbleUp (HeapTree h (s + 1) v (insert x l) r)
  else if not_perfect_r
  then bubbleUp (HeapTree h (s + 1) v l (insert x r))
  else if ls > rs
  then bubbleUp (HeapTree h (s + 1) v l (insert x r))
  else bubbleUp (HeapTree (h+1) (s + 1) v (insert x l) r)


-- Just a main function to run the code
main = do
  let list = [2, 16, 1, 0, -3, 123, 54, 6, 8, 25, 19, 30]
  let heap = fromList list
  print list
  print heap