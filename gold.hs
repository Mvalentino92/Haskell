isPrime :: Int -> Bool
isPrime n
        | n == 1 = False
        | n == 2 = True
        | mod n 2 == 0 = False
        | otherwise = all (/= 0) [mod n x | x <- [3..(n-1)]]


gold :: Int -> [(Int,Int,Int)]
gold 0 = [(4,2,2)]
gold n = gold (n-2) ++ [(x+y,x,y) | (x,y) <- zip ls rls, isPrime x && isPrime y && x <= y]
         where ls = [3,5..(n-1)]
               rls = reverse ls

gratHelp :: Int  -> [(Int,Int,Int)] -> [(Int,Int)]
gratHelp _ [] = []
gratHelp n ns = [(n,count)] ++ (gratHelp (n+2) (drop count ns))
              where count = length $ takeWhile (==n) [z | (z,x,y) <- ns]

grat :: [(Int,Int,Int)] -> [(Int,Int)]
grat ns = gratHelp 4 ns

getMax :: [(Int,Int)] -> (Int,Int)
getMax ns = swap $ maximum [(c,n) | (n,c) <- ns]

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
