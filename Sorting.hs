{- Just to say, I copied this code from a Wiki to generate the random numbers.
 - I didn't feel like figuring it out when I just wanted it real quick. 
 - Like to clarify since everything else is originally written.-}
import System.Random
import Data.List
 
main = do
    seed  <- newStdGen
    let rs = randomlist 10000 seed -- See the benchmarks for each sorting this list at the end.
    let sorted = qSort rs
    print sorted
 
randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)
-- END OF COPIED CODE





{- SELECTION SORT. Two functions. getMin, and the actual sSort function. 
 - Worst performance. The recursive calls to find the minimum everytime is pretty slow-}

-- Function to be used with sSort. Returns a tuple containing the min value along with it's index.
getMin :: (Ord a, Integral b)  => [a] -> b -> (a,b)
getMin [] _ = error "Cannot find min of empty list"
getMin [x] n = (x,n)
getMin (x:xs) n
       | x <= fst minTail = (x,n)
       | otherwise = minTail
       where minTail = getMin xs (n+1)

-- Selection sort. Calls getMin to recursively build the sorted array. 
sSort :: Ord a => [a] -> [a]
sSort xs
      | null xs = []
      | otherwise = min : sSort (take mindex xs ++ drop (mindex + 1) xs)
      where
          min = fst minpair
          mindex = snd minpair
          minpair = getMin xs 0

---------------------------------------------------------------------------------------------------

{- BUBBLE SORT. Three function. bSort is the actual algorithm. checkOrder checks if its sorted yet.
 bubbleSort calls the other two until its sorted. Improved performance. Not too bad recursively.-}

--Performs the bubble sort algorithm using pattern matching.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) 
      | x <= y = x : bubbleSort (y:xs)
      | otherwise = y : bubbleSort (x:xs)

-- Checks to see if the current iteration of bSort has successfully sorted the list.
checkOrder :: Ord a => [a] -> Bool
checkOrder [] = True
checkOrder [x] = True
checkOrder (x:y:xs)
           | x <= y = checkOrder (y:xs)
           | otherwise = False

-- Keeps calling bSort until the list is sorted
bSort :: Ord a => [a] -> [a]
bSort xs
           | checkOrder sorted = sorted
           | otherwise = bSort sorted
           where sorted = bubbleSort xs

--------------------------------------------------------------------------------------------------------

{- INSERTION SORT. One support function, one helper, and actual function to be called.
 - Even better performance. Pretty decent using the recursive version.
 - Definitely the best out of the first three that can be done without recursion. -}

-- Correctly inserts element into a sorted list.
insert' :: Ord a => [a] -> a -> [a]
insert' [] n = n:[]
insert' (x:xs) n
       | x <= n = x : insert' xs n
       | otherwise = n:(x:xs)

--The insertion sort algorithm. Calls insert to build up from an empty list.
insertionSort :: Ord a => [a] -> [a] -> [a]
insertionSort [] _  = []
insertionSort [x] xs = insert' xs x
insertionSort (x:xs) ys = insertionSort xs (insert' ys x)

-- Calls insertionSort while passing the empty list.
iSort :: Ord a => [a] -> [a]
iSort xs = insertionSort xs []

----------------------------------------------------------------------------------------------------------

{- MERGE SORT. Now were getting to the good stuff! Typical two functions. 
 - Right at home with Haskells recursive nature. Only 11 lines! -}

-- Merges them.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allX@(x:xs) allY@(y:ys)
      | x <= y = x : merge xs allY
      | otherwise = y : merge ys allX

-- Actual mergeSort algorithm.
mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort (take half xs)) (mSort (drop half xs))
           where half = div (length xs) 2

------------------------------------------------------------------------------------------------------------

{- QUICKSORT. The typical "Why Haskell is cool example."
 - It's quicksort in 5 lines! -}

-- List comprehensions!
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = (qSort smaller) ++ [x] ++ (qSort bigger)
               where smaller = [x' | x' <- xs, x' <= x]
                     bigger  = [x' | x' <- xs, x' >  x]

{- BENCHMARKS
 - sSort = 40 seconds
 - bSort = 14 seconds
 - iSort = 3 seconds
 - mSort = 0.437 seconds
 - qSort = 0.415 seconds. -}
