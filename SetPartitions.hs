import Data.List

--naive prime factor generator
factors :: Int -> [Int]
factors 1 = []
factors n = p : (factors $ div n p)
        where p = head [i | i <- [2..], mod n i == 0]

--Factorial
fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

--Choose function
choose :: Integer -> Integer -> Integer
choose n r = div (fac n) ((fac r)*(fac $ n - r))

--Helper function that actually calculates number of partitions
partitions' :: [Integer] -> Int -> Int-> [Integer]
partitions' xs n n'
         | n == n' = xs
         | otherwise = partitions' xs' n (n' + 1)
         where x = sum [c * (xs !! (fromIntegral (l - i))) | i <- [s..l], let c = choose l i]
               s = 0 :: Integer
               l = toInteger n'
               xs' = xs ++ [x]

--Gets how many partitions a set with this many elements has
partitions :: Int -> Integer
partitions n = last $ partitions' [1] n 0

--Get the number of digits a number has
digits :: Integer -> Int
digits 0 = 0
digits n = 1 + digits (div n 10)

--Generates the combinations from the Choose function
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++ if length xs < n then [] 
                                                              else combinations n xs
--Delete the first occurence of an element
deleteFirst :: (Ord a, Eq a) => [a] -> a -> [a]
deleteFirst [] _ = []
deleteFirst (x:xs) x' = if x == x' then xs else x : deleteFirst xs x'

--Kill off the specified elements from the list
kill :: (Ord a, Eq a) => [a] -> [a] -> [a]
kill [] _ = []
kill xs [] = xs
kill xs (y:ys) = kill (deleteFirst xs y) ys

--Generates the partitions of a set
genPart :: Ord a => [a] -> [[[a]]]
genPart [] = [[]]
genPart [x] = [[[x]]]
genPart [x,y] = [[[x],[y]],[[x,y]]]
genPart (x:xs) = concat [map (i:) $ genPart $ kill (x:xs) i | i <- combos]
        where combos = concat [map (x:) $ combinations i xs | i <- [0..length xs]]
------------------------------------------------------------------------------------------------------
--Another partition method. I got this algorithm exploring other peoples methods online
--I DO NOT TAKE CREDIT FOR THIS ALGORITHM
partish' :: Int -> Int -> Int -> [[Int]]
partish' n m max'
        | n == m = [[i] | i <- [1..r]]
        | otherwise = concat [map (i:) (partish' (n+1) m max'')| i <- [1..r], let max'' = max max' i]
        where r = min (max' + 1) n

partish :: [Int] -> [[[Int]]]
partish xs =  [[[y | (x,y) <- p, x == i] | i <- a] | c <- combos, let a = nub c, let p = zip c xs]
        where combos = partish' 1 (length xs) 1
-------------------------------------------------------------------------------------------------------

--Checks if a the lists, has lists with all unique elements
isUnique :: (Ord a, Eq a) => [[a]] -> Bool
isUnique xs = and [False | i <- xs, i /= nub i]

--Filters out any list with repeated primefactors
filterRepeats :: (Ord a, Eq a) => [[[a]]] -> [[[a]]]
filterRepeats xs = [i | i <- xs, isUnique i]

--Combines lists into factors via multiplication, and removes duplicates
filterDuplicates :: (Ord a, Eq a, Num a) => [[[a]]] -> [[a]]
filterDuplicates xs = nub $ map sort (map (map product) xs)

--Finds the number of square free factors. Either supply the number, or factors manually
squareFreeFactors = length . filterDuplicates . filterRepeats . genPart . factors
sqf xs = length . filterDuplicates . filterRepeats $ genPart xs

main = do
     let s = length $ genPart [1..13]
     print s
