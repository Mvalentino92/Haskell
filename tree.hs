data Tree a =  Nil | Node (Tree a) a (Tree a) deriving (Show,Eq,Ord)

treeAdd :: (Ord a, Eq a) => (Tree a) -> a -> (Tree a)
treeAdd Nil ele = Node Nil ele Nil
treeAdd (Node l n r) ele
        | ele < n = Node (treeAdd l ele) n r
        | otherwise = Node l n (treeAdd r ele)

addValues :: (Ord a, Eq a) => (Tree a) -> [a] -> (Tree a)
addValues tree [] = tree
addValues tree (x:xs) = addValues (treeAdd tree x) xs

treeToList :: (Ord a, Eq a) => (Tree a) -> [a]
treeToList Nil = []
treeToList (Node l n r) = (treeToList l) ++ [n] ++ (treeToList r)

t = Nil
toAdd = [5,10,3,6,11,1,12,4]
t' = addValues t toAdd
l = treeToList t'
