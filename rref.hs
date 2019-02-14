rowKill :: (Fractional a) => [a] -> [a] -> [a]
rowKill (x:xs) (y:ys) = zipWith (\x' y' -> x'*k + y') (x:xs) (y:ys)
               where k = -y/x

ref :: (Fractional a,Eq a) => [[a]] -> [[a]]
ref [xs] = let x = head xs in [map (/x) xs]
ref (xs:xss)
           | x /= 0 = xs' : (ref $ map tail xss')
           | otherwise = ref $ swapRows xs xss
            where xss' = map (rowKill xs) xss
                  xs' = map (/x) xs
                  x = head xs

bref :: (Fractional a) => [[a]] -> [[a]]
bref [xs] = [xs]
bref (xs:xss) = xs : (bref xss'')
            where xss' = map (rowKill xs) $ trimDiag xss
                  xss'' = zipWith (\x y -> take (length x - length y) x ++ y) xss xss'


trimDiag :: (Num a) => [[a]] -> [[a]]
trimDiag xss = [(drop i xs) | (i,xs) <- zip [1..] xss]

swapRows :: (Num a,Eq a) => [a] -> [[a]] -> [[a]]
swapRows xs (xs':xss) 
          | head xs == 0 = (swapRows xs' xss) ++ [xs]
          | otherwise = [xs] ++ (xs':xss)

rref :: (Fractional a,Eq a) => [[a]] -> [a]
rref = map last . reverse . bref . reverse . ref
