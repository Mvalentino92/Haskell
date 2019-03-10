factors :: Int -> [Int]
factors 1 = []
factors n = p : (factors $ div n p)
        where p = head [i | i <- [2..], mod n i == 0]

fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

choose :: Integer -> Integer -> Integer
choose n r = div (fac n) ((fac r)*(fac $ n - r))

partitions' :: [Integer] -> Int -> Int-> [Integer]
partitions' xs n n'
         | n == n' = xs
         | otherwise = partitions' xs' n (n' + 1)
         where x = sum [c * (xs !! (fromIntegral (l - i))) | i <- [s..l], let c = choose l i]
               s = 0 :: Integer
               l = toInteger n'
               xs' = xs ++ [x]

partitions :: Int -> [Integer]
partitions n = partitions' [1] n 0

digits :: Integer -> Int
digits 0 = 0
digits n = 1 + digits (div n 10)
