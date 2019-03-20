import System.Random
import Data.List
import Data.List.Split
import qualified Data.PQueue.Min

--Board object (f,actual board,depth)
data Board = Board Int [Int] Int deriving (Show,Eq,Ord)

--Creates a board object, takes vector and depth
cb dep xs = Board ((h xs) + dep) xs dep

--Generates a random starting board, may not be unique
randomInit n = map (\x -> abs (mod x 9)) $ take 9 $ randoms (mkStdGen n) :: [Int]

--Keeping going til you get unqiue numbers
initBoard n = head [current | i <- [n..], let current = randomInit i, nub current == current]

----------------------------PERMUTATIONS AND INVERSIONS------------------------------------
perm :: Ord a => [a] -> [[a]]
perm [x] = [[x]]
perm xs = concat [map (h:) (perm tl) | (h:tl) <- pss]
       where l = length xs - 1
             zp = zip [0..l] xs
             pss = [ xs !! i : [p | (j,p) <- zp, j /= i ] | i <- [0..l]]

inver :: [Int] -> Int
inver [] = 0
inver (x:xs) = (length $ filter (\y -> x > y && y > 0) xs) + (inver xs)

isValid :: [Int] -> Bool
isValid xs = mod val 2 == 0 && val > 0
         where val = inver xs

----------------------------FUNCTIONS FOR CHILDREN GENERATION-----------------------------------------
--Generate all children that are possible moves
getChildren :: [Int] -> [[Int]]
getChildren xs = [xs'| f <- funcs, let xs' = f xs, xs /= xs']
        where funcs = [side1',side2,up1,up2]

--Helper
side1 :: [Int] -> Int -> Int -> [Int]
side1 [] _ _  = []
side1 (x:[]) _ _ = [x]
side1 (x:y:ys) a b
        | (x == 0 || y == 0) && a < b = y : x : side1 ys (mod (a+2) 3) (mod (b+2) 3)
        | otherwise = x : y : side1 ys (mod (a+2) 3) (mod (b+2) 3)

--Helper (TRUE)
side1' xs = side1 xs 0 1

--Helper
side2 :: [Int] -> [Int]
side2 xs = head xs : (side1 (tail xs) 1 2)

--Helper
up1 :: [Int] -> [Int]
up1 xs = concat $ transpose $ chunksOf 3 xs''
       where xs' = concat $ transpose $ chunksOf 3 xs
             xs'' = side1' xs'
             
--Helper
up2 :: [Int] -> [Int]
up2 xs = concat $ transpose $ chunksOf 3 xs''
       where xs' = concat $ transpose $ chunksOf 3 xs
             xs'' = side2 xs'
------------------------------------------------------------------------------------------------------

--Counts how many numbers are out of place
h :: [Int] -> Int
h = length . (filter (==False)) . (zipWith (==) ([1..8] ++ [0]))

--Gets the actual board from Board object
getVec :: Board -> [Int]
getVec (Board _ vec _ ) = vec 

--Gets the depth
getDepth :: Board -> Int
getDepth (Board _ _ dp) = dp

--Filters from the closed list
checkClosed cdr cl = filter (\x -> not $ elem (getVec x) cl) cdr

--Deletes child with this signature 
delChild vec child = [i | i <- child, getVec i /= vec]

--Returns a new open list, with the surviving children incorporated
--(c:cs) is really op, and op is really children
--TAKES OPEN LIST FIRST, THEN CHILDREN
checkOpen [] op = op
checkOpen (c:cs) op
          | elem c' op' = cb (if dp < dp' then dp else dp') c' : checkOpen cs (delChild c' op)
          | otherwise = c : checkOpen cs op
          where c' = getVec c
                op' = map getVec op
                dp = getDepth c
                dp' = head [getDepth i | i <- op, getVec i == c']

--Returns a new pq
newPQ children open closed = Data.PQueue.Min.fromList $ checkOpen open (checkClosed children closed)

--Finds the solution (open,closed,sol)
astar op cl sol c
          | c > 1000 = Nothing
          | Data.PQueue.Min.null op = Nothing
          | topVec == [1,2,3,4,5,6,7,8,0] = Just (reverse $ head [parent | parent <- sol, head parent == topVec])
          | otherwise = astar op' (topVec : cl) sol' (c+1)
          where cdr = getChildren topVec
                children = map (cb (topDepth+1)) cdr
                top = Data.PQueue.Min.findMin op
                topVec = getVec top
                op' = newPQ children (Data.PQueue.Min.toListU (Data.PQueue.Min.deleteMin op)) cl
                topDepth = getDepth top
                soll = head [[child : parent | child <- cdr] | parent <- sol, head parent == topVec]
                solr = [parent | parent <- sol, head parent /= topVec]
                sol' = soll ++ solr

--Because monads
fromJust Nothing = []
fromJust (Just x) = x

--Finds all solutions
solutions :: [[Int]] -> [(Int,[Int])]
solutions [] = []
solutions (x:xs) = (l,x) : solutions xs
          where op = Data.PQueue.Min.singleton (cb 0 x)
                sol = fromJust (astar op [x] [[x]] 0)
                l = length sol
main = do
       let vecs = filter isValid $ perm [0,1,2,3,4,5,6,7,8]
           sols = filter (\(x,y) -> x > 0) $ solutions vecs
       mapM_ print sols
