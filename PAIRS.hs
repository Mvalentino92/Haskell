pairs :: [a] -> [(a,a)]
pairs xs = zip xs $ tail xs

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x' == x]

sumPow :: Integral a => a -> a
sumPow n = sum [x^2 | x <- [1..n]]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--replicate' :: Int -> a -> [a]
--replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], x*x + y*y == z*z]

perfect :: Int -> [Int]
perfect n = [x | x <- [2..n], let factors x' = [f | f <- [1..x'-1], mod x' f == 0], (sum $ factors x) == x]

-- Finish this one
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [(xs !! x)*(ys !! y) | x <- [0..length(xs)-1], y <- [0..length(ys)-1], x == y]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
         | x <= y = x : y : ys
         | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs) 

sumDown :: Integral a => a -> a
sumDown 0 = 0
sumDown n = n + sumDown (n-1)

euclid :: Integral a => a -> a -> a
euclid _ 1 = 1
euclid 1 _ = 1
euclid m n 
         | m == n = m
         | otherwise = euclid smaller diff
         where smaller = minimum [m,n]
               diff = maximum[m,n] - smaller

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) =  x && allTrue xs

concat' :: [[a]] -> [a]
concat' (x:xs) 
        | null xs = x
        | otherwise = x ++ concat' xs

replicate' :: Integral a => a -> b -> [b]
replicate' 0 _ = []
replicate' m n = n : (replicate' (m-1) n)

getEle :: Integral a => a -> [b] -> b
getEle m ns
       | m == 0 = head ns
       | otherwise = getEle (m-1) (tail ns)

exists :: Eq a => a -> [a] -> Bool
exists _ [] = False
exists x (y:ys)
       | x == y = True
       | otherwise = exists x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
      | x <= y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
          where left = take half xs
                right = drop half xs
                half = (fromIntegral $ (length xs) `div` 2) 

sumList ::  Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last $ tail xs
