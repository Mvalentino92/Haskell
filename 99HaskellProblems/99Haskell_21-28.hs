import Data.List
import System.Random
--Problem 21: Insert an element at the given position in a list.
insertAt :: Ord a => a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

--Problem 22: Create a list of all integers in a given range
range' :: Integer -> Integer -> [Integer]
range' s e = [s..e]

--Problem 23: Extract a given number of randomly selected elements from a list.
rnd_select :: Ord a => [a] -> Int -> [a]
rnd_select xs n = [xs !! i | i <- r]
           where r = take n $ randomRs (0,e) rnd
                 e = length xs - 1
                 rnd = mkStdGen n

--Problem 24: Draw N different random numbers from the set 1..n
diff_select :: Int -> Integer -> [Integer]
diff_select n m = take n (randomRs (1,m) $ mkStdGen 1)

--Problem 25: Generate a random permutation (Trying my own thing without too much knowledge of Random)
shuffle :: Int -> Int -> [Int]
shuffle (-1) _ = []
shuffle s e =  [x] ++ shuffle s' e' ++ shuffle s'' e''
            where x = head (randomRs (s,e) (mkStdGen (e-s)))
                  s' = if s == x then (-1) else s
                  e' = x - 1
                  s'' = if x == e then (-1) else x + 1
                  e'' = e

shuffle' :: Int -> Int -> [Int]
shuffle' s e = foldr (\x acc -> drop x acc ++ take x acc) xs (take (e+e+e) (randomRs (s,e) (mkStdGen 0)))
               where xs = shuffle s e

rnd_perm :: Ord a => [a] -> [a]
rnd_perm xs = [xs !! i | i <- shuffle' 0 (length xs - 1)]

--Problem 26: C n r
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++ if length xs < n then [] 
                                                              else combinations n xs

--Problem 27: Disjoint subsets of multinomial coefficient
filterOut :: Ord a => [a] -> [a] -> [a]
filterOut [] ys = ys
filterOut (x:xs) ys = filterOut xs (delete x ys)

groups :: Ord a => [Int] -> [a] -> [[[a]]]
groups [n] xs = [[xs]]
groups (n:ns) xs = [x:y | x <- (combinations n xs), y <- groups ns (filterOut x xs)]

groups2 :: Ord a => [Int] -> [a] -> [[[a]]]
groups2 [n] xs = [[xs]]
groups2 (n:ns) xs = helper (n:ns) cbn flt
               where cbn = combinations n xs
                     flt = map ((flip filterOut) xs) cbn
                     helper _ [] [] = []
                     helper (n:ns) (a:as) (b:bs) = [a:a' | a' <- (groups2 ns b)] ++ helper (n:ns) as bs

--Problem 28: Sort a list according the lengths of it's sublists
lsort :: Ord a => [[a]] -> [[a]]
lsort xs = map (\x -> snd x) ys
           where ls = map length xs
                 ys = sort $ zip ls xs

--Part B, sort by length frequency, rarest first
fsort :: Ord a => [[a]] -> [[a]]
fsort xs = concat [foldr (\x acc-> if length x == n' then x:acc else acc) [] xs | (n,n') <- sortedf]
         where sortedls = map length $ lsort xs
               sortedf = sort [(count x sortedls, x) | x <- nub sortedls] 
               count n xs = sum [1 | x <- xs, n == x]
