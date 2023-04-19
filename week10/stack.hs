-- Stack.hs, literally nothing special about this one

import Data.Maybe

data Stack a = Empty | Node a (Stack a)

instance Show a => Show (Stack a) where
  show Empty = " "
  show (Node x xs) = (show x) ++ "->" ++ (show xs)

fromList :: Ord a =>[a] -> Stack a
fromList xs = foldr (push) Empty xs

push :: a -> Stack a -> Stack a
push x Empty = Node x Empty
push x xs = (Node x xs)

-- Empty values don't have type so we can't return anything meaningful
-- if stack is indeed empty then just return NOTHING which can be done with a
-- maybe monad
pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (Node x xs) = Just (x, xs)


main = do
  let list = [1,15,3,64]
  print list
  let stack = fromList list
  print stack
  let result = pop stack
  print result
  -- Undo the maybe monad stuff
  let unmaybe = fromMaybe (-1, Empty) result
  print unmaybe
  print (fst unmaybe)
  print (snd unmaybe)