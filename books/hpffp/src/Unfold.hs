module Unfold where
import qualified Data.List as List
-- >>> take 5 $ myIterate (*2) 10
-- [10,20,40,80,160]
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- >>> take 5 $ List.unfoldr (\x -> Nothing) 0
-- >>> take 5 $ myUnfoldr (\x -> Nothing) 0
-- >>> take 5 $ List.unfoldr (\x -> Just (x, (1-x))) 0
-- >>> take 5 $ myUnfoldr (\x -> Just (x, (1-x))) 0
-- []
-- []
-- [0,1,0,1,0]
-- [0,1,0,1,0]
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = g (f x)
  where g (Just (a,b)) = a : myUnfoldr f b
        g Nothing      = []

-- >>> take 5 $ myBetterIterate (*2) 10
-- [10,20,40,80,160]
myBetterIterate :: (a -> a) -> a -> [a]
myBetterIterate f = myUnfoldr (Just . (\a -> (a, f a)))

data BTree a = Leaf | Node (BTree a) a (BTree a)
  deriving (Eq, Ord, Show)

-- >>> treeBuild 2
-- Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
btunfold :: ( a -> Maybe (a,b,a) ) -> a -> BTree b
btunfold f x = g (f x)
  where g Nothing = Leaf
        g (Just (x,y,z)) = Node (btunfold f x) y (btunfold f z)

treeBuild :: Integer -> BTree Integer
treeBuild x = btunfold f 0
  where
    f a | a >= x = Nothing
        | otherwise = Just (a+1, a , a+1)

