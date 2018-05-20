-- Finds the prime factors
fac :: Integer -> Integer -> [Integer]
fac n s
    | n == 1 = []
    | s == 2 = s' : fac n' s'
    | otherwise = s'' : fac n'' s''
    where s' = head [x | x <- [s..], mod n x == 0]
          s''= head [x | x <- [s,(s+2)..], mod n x == 0]
          n' = div n s'
          n'' = div n s''

-- Generates the arithmetic derivative
deriv :: [Integer] -> Integer -> Integer
deriv [x] _ = 1
deriv (x:y:zs) y'
      | null zs = y''
      | otherwise = deriv (x':zs) y''
      where x' = x*y
            y'' = x + y*y'

-- Combine the above
arith :: Integer -> Integer
arith n = deriv (fac n 2) 1

main = do
       let ex = [(x,arith x) | x <- [2..1000]]
       print ex
