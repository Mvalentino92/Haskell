import System.Random

--Binary Search Tree 
data Tree a =  Nil | Node (Tree a) a (Tree a) deriving (Show,Eq,Ord)

--Adds an element to a Binary Search Tree
treeAdd :: (Ord a, Eq a) => (Tree a) -> a -> (Tree a)
treeAdd Nil ele = Node Nil ele Nil
treeAdd (Node l n r) ele
        | ele < n = Node (treeAdd l ele) n r
        | otherwise = Node l n (treeAdd r ele)

--Adds a list of elements to Binary Search Tree
addValues :: (Ord a, Eq a) => (Tree a) -> [a] -> (Tree a)
addValues tree [] = tree
addValues tree (x:xs) = addValues (treeAdd tree x) xs

--Converts a Binary Search Tree to a list of it's values (they are sorted)
treeToList :: (Ord a, Eq a) => (Tree a) -> [a]
treeToList Nil = []
treeToList (Node l n r) = (treeToList l) ++ [n] ++ (treeToList r)

--Checks if an element is present in a Binary Search Tree
isPresent :: (Eq a, Ord a) => (Tree a) -> a -> Bool
isPresent Nil _ = False
isPresent (Node l n r) ele 
          | ele == n = True
          | ele < n = isPresent l ele || False
          | otherwise = False || isPresent r ele

--Get the number of elements in a Binary Search Tree
treeSize :: (Tree a) -> Int
treeSize Nil = 0
treeSize (Node l _ r) = treeSize l + 1 + treeSize r

--Get the depth of a Binary Search Tree
treeDepth :: (Tree a) -> Int
treeDepth Nil = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

toAdd  = take 10 $ (randoms (mkStdGen 7)) :: [Int]
t = addValues (Nil) toAdd
l = treeToList t
