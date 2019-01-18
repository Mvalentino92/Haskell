--Problem 1: Find the last element of list.
myLast :: Ord a => [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

--Problem 2: Find the second to last element
myButLast :: Ord a => [a] -> a
myButLast = myLast . init 

--Problem 3: Find the K'th element of a list. The firstelelent in the list is number 1.
elementAt :: Ord a => [a] -> Integer -> a
elementAt xs k  = head [x | (x,i) <- zip xs [1..k], i == k]   

elementAt2 :: Ord a => [a] -> Integer -> a
elementAt2 (x:xs) 1 = x
elementAt2 (x:xs) n = elementAt2 xs (n-1)

--Problem 4: Find the number of elements in a list
myLength :: Ord a => [a] -> Integer
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myLength2 :: Ord a => [a] -> Integer
myLength2 [] = 0
myLength2 xs = foldr (\_ acc -> acc + 1) 0 xs

--Problem 5: Reverese a list
myReverse :: Ord a => [a] -> [a]
myReverse = foldl (\xs x -> x : xs) []

myReverse2 :: Ord a => [a] -> [a]
myReverse2 = foldr (\x xs -> xs ++ [x]) []

--Problem 6: Find out whether a list is a palindrome.
isPalindrome :: Ord a => [a] -> Bool
isPalindrome xs = xs == myReverse2 xs

--Problem 7: Flatten a nested list structure
--Define a NestedList data type first
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: Ord a => NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat [flatten x | x <- xs]

flatten2 :: Ord a => NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List (x:xs)) = flatten x ++ flatten (List xs)

--Problem 8: Remove consecutive duplicates in a list, keep order
compress :: Ord a => [a] -> [a]
compress (x:xs) = foldr (\x acc -> if null acc then x : [] else 
                         if head acc == x then acc else x : acc) [] xs

compress2 :: Ord a => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = if x == head xs then compress xs else x : compress xs

--Problem 9: Pack consecutive duplicates into sublists
pack :: Ord a => [a] -> [[a]]
pack [] = []
pack (x:xs) = same : pack different 
              where same = takeWhile (==x) (x:xs)
                    different = dropWhile (==x) (x:xs)

--Problem 10: Store the length of consecutive duplicate elements in tuples.
encode :: Ord a => [a] -> [(Int,a)]
encode = map (\xs -> (length xs, head xs)) . pack 
