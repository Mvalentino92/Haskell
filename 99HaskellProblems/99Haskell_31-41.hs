import Data.List
--Problem 31: Test for primeness
isPrime :: Integer -> Bool
isPrime n 
        | n < 2 = False 
        | n == 2 = True
        | mod n 2 == 0 = False
        | otherwise = let sq = floor $ sqrt $ fromIntegral n
                      in and [mod n x /= 0 | x <- [3,5..sq]]

--Problem 32: GCD
gcd' :: Integer -> Integer -> Integer
gcd' m n 
      | n == 0 = m
      | m >= n = gcd' n (mod m n)
      | otherwise = gcd' n m

--Problem 33: determine is numbers are coprime
coprime :: Integer -> Integer -> Bool
coprime m n = gcd' m n == 1

--Problem 34: Calculate eulers totient function
totient :: Integer -> Integer
totient n  = sum [1 | x <- [1..(n-1)], coprime n x]

--Problem 35: Calculate the prime factors of a number
primefactors :: Integer -> [Integer]
primefactors 1 = []
primefactors n
             | isPrime n = [n]
             | otherwise = sort (primefactors d' ++ primefactors d)
                           where (d,d') = head [(div n x,x) | x <- [sq,(sq-1)..], mod n x == 0]
                                 sq = floor $ sqrt $ fromIntegral n

phelp :: Integer -> Integer -> [Integer]
phelp s 1 = []
phelp s n = p : phelp p (div n p)
        where p = last $ take 2 [x | x <- n:[s..sq], mod n x == 0] 
              sq = floor $ sqrt $ fromIntegral n

primefactors2 :: Integer -> [Integer]
primefactors2 n
        | isPrime n = [n]
        | otherwise = phelp 2 n 

--Problem 36: Prime factor multiplcity
occur :: [Integer] -> [(Integer,Int)]
occur [] = []
occur (x:xs) = (x, length t) : occur d
          where t = x : takeWhile (==x) xs
                d = dropWhile (==x) xs

--Problem 37: Improved Totient
tot :: Integer -> Integer
tot n = product [(p-1)*p^(m-1) | (p,m) <- occur $ primefactors n]

--Problem 38: Just a comparison between the two totients, no solutions required

--Problem 39: Prime range
primeRange :: Integer -> Integer -> [Integer]
primeRange ln hn = alt left right
           where half = div (ln + hn) 2
                 left = [l | l <- [(half+1),half..ln], isPrime l]
                 right = [r | r <- [half+2..hn], isPrime r]
                 alt [] ys = ys
                 alt xs [] = xs
                 alt (x:xs) (y:ys) = x : y : (alt xs ys)

--Problem 40: Goldbachs conjecture, print out 2 primes that equal even number
goldbach :: Integer -> (Integer,Integer)
goldbach n = head [(p1,p2) | (p1,p2) <- zipPrimes pr pr, p1 + p2 == n]
         where pr = primeRange 2 (n-2)
               zipPrimes _ [] = []
               zipPrimes xs ys = zip xs ys ++ zipPrimes xs (tail ys)

--Problem 41: Print a range of goldbach numbers in a given range
goldbachList :: Integer -> Integer -> [(Integer,Integer)]
goldbachList ln hn = [goldbach n | n <- [ln',(ln'+2)..hn]]
                 where ln' = if ln < 2 then 4 
                             else if mod ln 2 == 0 then ln
                             else ln + 1
