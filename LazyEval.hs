fibs :: [Integer]
fibs = 0 : 1 : [x+y | (x,y) <- zip fibs (tail fibs)]

sqroot :: Double -> Double
sqroot a = val
         where val = elements iter
               iter = (iterate (\g -> (g + a/g)/2.0)) 1.0
               elements (x:xs)
                      | abs v == 0.0 = nx
                      | otherwise = elements xs
                      where v = nx - x
                            nx = head xs
