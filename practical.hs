{- I personally feel like my commmented out restrictions should define
 - when Strings are Isomorphic, but oh well.-}
rtuple :: (Char,Char) -> (Char,Char)
rtuple (x,y) = (y,x)

getMatches :: (Char,Char) -> [(Char,Char)] -> [(Char,Char)]
getMatches (x,y) ps = [(x',y') | (x',y') <- ps, x == x' || {-x == y' || y == x' ||-} y == y']

iso :: [(Char,Char)] -> Bool
iso [] = True
iso (x:xs) = (foldr (&&) True bools) && iso xs
           where matches = getMatches x xs
                 bools = [x == x' {-|| x == (rtuple x')-} | x' <- matches] 

isIso :: String -> String -> Bool
isIso a b = iso $ zip a b

--Returns duplicates of array
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x xs
        | x == head xs = 1 + (count x (tail xs))
        | otherwise = count x (tail xs)
getDuplicate :: Eq a => [a] -> [a]
getDuplicate [] = []
getDuplicate (x:xs)
             | occurences > 0 = x:(getDuplicate $ drop occurences xs)
             | otherwise = getDuplicate xs
             where occurences = count x xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
             where smaller = [x' | x' <- xs, x' <= x]
                   larger = [x' | x' <- xs, x' > x] 

gdups :: Ord a => [a] -> [a]
gdups xs = getDuplicate $ qsort xs
