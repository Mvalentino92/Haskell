gdigs :: Integer -> [Integer] -> Integer
gdigs _ [] = 0
gdigs n (p:ps)
        | z = n'' + gdigs n'' ps
        | otherwise = gdigs n'' ps
        where count _ [] = 0
              count p (x:xs)
                    | p == x = 1 + count '1' xs
                    | otherwise = count '1' xs
              n' = count '1' (show p)
              n'' = n + n'
              z = n'' == p

gdigs' :: Integer -> [Integer] -> [Integer]
gdigs' _ [] = []
gdigs' n (p:ps)
        | z = p:gdigs' n'' ps
        | otherwise = gdigs' n'' ps
        where count _ [] = 0
              count p (x:xs)
                    | p == x = 1 + count '1' xs
                    | otherwise = count '1' xs
              n' = count '1' (show p)
              n'' = n + n'
              z = n'' == p
              

main = do
       print $ gdigs 0 [1..10000000000]

