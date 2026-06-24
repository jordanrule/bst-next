{-# LANGUAGE GADTs, StandaloneDeriving #-}

module BSTv4 (BST(..), next) where

----------------------- BST datatype -----------------------

data BST a where
  Nil :: BST a
  Node :: (Eq a, Ord a) => { left :: BST a
                           , val :: a
                           , right :: BST a
                           } -> (BST a)
deriving instance Eq a => Eq (BST a)
deriving instance Show a => Show (BST a)

----------------------- Utilities -----------------------

min :: BST a -> BST a
min Nil = Nil
min n@(Node Nil _ _) = n
min (Node l _ _) = BSTv4.min l

-- Walk down from the root and carry the best "larger than a" candidate.
findSuccessor :: Ord a => BST a -> a -> (Bool, BST a)
findSuccessor t a = go t Nil
  where
    go Nil successor = (False, successor)
    go n@(Node l v r) successor
      | a < v = go l n
      | a > v = go r successor
      | otherwise = (True, successor)

next :: Ord a => BST a -> BST a -> BST a
next _ Nil = Nil
next _ (Node _ _ r@Node{}) = BSTv4.min r
next t (Node _ a _) =
  case findSuccessor t a of
    (True, successor) -> successor
    (False, _) -> Nil

----------------------- Tests -----------------------

--              8
--           /    \
--         3       10
--       /  \       \
--     1     6       14
--         /  \     /
--       4     7  13

n1 = Node Nil 1 Nil
n4 = Node Nil 4 Nil
n7 = Node Nil 7 Nil
n6 = Node n4 6 n7
n3 = Node n1 3 n6
n13 = Node Nil 13 Nil
n14 = Node n13 14 Nil
n10 = Node Nil 10 n14
n8 = Node n3 8 n10

test = do
  assert (next n8 n1) n3
  assert (next n8 n3) n4
  assert (next n8 n4) n6
  assert (next n8 n6) n7
  assert (next n8 n7) n8
  assert (next n8 n8) n10
  assert (next n8 n10) n13
  assert (next n8 n13) n14
  assert (next n8 n14) Nil

assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected = case actual == expected of
  True -> putStrLn $ "OK " ++ show expected ++ " is " ++ show actual
  False -> putStrLn $ "FAIL " ++ show expected ++ " is " ++ show actual


