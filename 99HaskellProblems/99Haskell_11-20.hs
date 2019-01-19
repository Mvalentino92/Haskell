--Problem 11: Redo of number 10, with custom data types distinguishing between elements that occur multiple times, and elements that only occur a single time.
pack :: Ord a => [a] -> [[a]]
pack [] = []
pack (x:xs) = same : pack different 
            where same = takeWhile (==x) (x:xs)
                  different = dropWhile (==x) (x:xs)
 
encode :: Ord a => [a] -> [(Int,a)]
encode = map (\xs -> (length xs, head xs)) . pack

data Occur a = Multiple Int a | Single a deriving (Show,Read,Eq,Ord) 

encode' :: Ord a => [a]-> [Occur a]
encode' = wrap . encode
        where wrap [] = []
              wrap ((n,x):xs) = if n == 1 then (Single x) : wrap xs
                                else (Multiple n x) : wrap xs 

--Problem 12: Give a list from above, return the original input.
decode :: Ord a => [Occur a] -> [a]
decode [] = []
decode ((Multiple n x):xs) = (replicate n x) ++ decode xs
decode ((Single x):xs) = [x] ++ decode xs

--Problem 13: Skipped, actually is indentical to 11, given the translation of the problem to Haskell

--Problem 14: Duplicate the elements of a list
dupli :: Ord a => [a] -> [a]
dupli [] = []
dupli (x:xs) = (replicate 2 x) ++ dupli xs

dupli' :: Ord a => [a] -> [a]
dupli' = foldr (\x acc -> x : x : acc) [] 

--Problem 15: Replicate elements in a list the given number of times
repli :: Ord a => Int -> [a] -> [a]
repli _ [] = []
repli n (x:xs) = (replicate n x) ++ repli n xs

--Problem 16: Drop every N'th element from a list
drop' :: Ord a => [a] -> Int -> [a]
drop' xs n = [x | (i,x) <- zip [1..] xs, mod i n /= 0]

drop2 :: Ord a => [a] -> Int -> [a]
drop2 [] _  = []
drop2 xs n = take (n-1) xs ++ drop2 (drop n xs) n 

--Problem 17: Split a list into two parts. Length of first part is given
split' :: Ord a => [a] -> Int -> ([a],[a])
split' xs n = (take n xs, drop n xs)

--Problem 18: Extract a slice from a list
slice' :: Ord a => [a] -> Int -> Int -> [a]
slice' xs s e = take (e-s+1) (drop (s-1) xs)

--Problem 19: Rotate a list N places to the left
rotateL :: Ord a => [a] -> Int -> [a]
rotateL xs n
        | n == 0 = xs
        | n > 0 = drop n xs ++ take n xs
        | n < 0 = drop clip xs ++ take clip xs
              where clip = length xs + n 

--Problem 20: Remove the K'th element from a list
removeAt :: Ord a => Int -> [a] -> (a,[a])
removeAt n xs = (single,rest)
         where single = getSingle n xs
               rest = getRest n xs
               getSingle 1 (x:xs) = x
               getSingle n (_:xs) = getSingle (n-1) xs
               getRest _ [] = []
               getRest 1 (_:xs) = getRest 0 xs
               getRest n (x:xs) = x : getRest (n-1) xs 

removeAt' :: Ord a => Int -> [a] -> (a,[a])
removeAt' n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)
