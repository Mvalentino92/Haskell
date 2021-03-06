{-This file contains any useful functions I'll be using a lot,
 -or need to use for class for exercises-}
import System.Random
import Debug.Trace
import Data.List

--Returns n amount of linearly spaced values between x and y
linspace :: Double -> Double -> Int -> [Double]
linspace x y n = x : [x + h * (fromIntegral i) | i <- [1..(n-1)]]
                 where h = (y-x)/(fromIntegral (n-1))

--Returns the numerical derivative using centered difference with step size
cdiff' :: (Double -> Double) -> Double -> Double -> Double
cdiff' f x h = ((f $ x + h) - (f $ x - h))/(2*h)

--Automatically assums an h of 1e-4
cdiff :: (Double -> Double) -> Double -> Double
cdiff f x = cdiff' f x 1e-4

--Approximates subsequent numerical derivatives
derivs' :: (Double -> Double) -> Double -> Double -> Int -> Double
derivs' f x h 1 = cdiff f x
derivs' f x h ord = (y1 - y0)/(x1 - x0)
               where x0 = x - h
                     x1 = x + h
                     y0 = derivs' f x0 h (ord - 1)
                     y1 = derivs' f x1 h (ord - 1)

--Automatically assumes an h of 1e-2
derivs :: (Double -> Double) -> Double -> Int -> Double
derivs f x ord = derivs' f x 1e-2 ord

--Taylor series
taylor :: (Double -> Double) -> Double -> Double -> Int -> Double
taylor f x x0 ord = (f x0) + sum (zipWith (/) (zipWith (*) pows primes) facs)
                    where ns = take ord [1..]
                          facs = scanl1 (*) ns
                          pows = map ((x-x0)**) ns
                          primes = [derivs f x0 i | i <- [1..ord]]

--Mclaurin series
mclaur :: (Double -> Double) -> Double -> Int -> Double
mclaur f x ord = taylor f x 0 ord

--TrapZ
trapz :: (Double -> Double) -> Double -> Double -> Int -> Double
trapz f a b n = sum xs - ((f a * (h/2)) + (f b * (h/2)))
                where xs = map (\x -> f x * h) (linspace a b n)
                      h = (b-a)/(fromIntegral n - 1)

--Simpsons
simpsons :: (Double -> Double) -> Double -> Double -> Int -> Double
simpsons f a b n = sum xs - ((f a * (h/3)) + (f b * (h/3)))
                   where xs = zipWith (*) (map f (linspace a b n)) (cycle [2*h/3,4*h/3])
                         h = (b-a)/(fromIntegral n - 1)

--Monte Carlo
monte :: (Double -> Double) -> Double -> Double -> Int -> StdGen -> StdGen -> Double
monte f a b n rx ry = area*((fst count/fromIntegral n) - (snd count/fromIntegral n))
                where coord = zip rxs rys --random coordinates
                      ymin = minimum xs --minimum function value (bottom of box)
                      ymax = maximum xs --maximum function value (top of box)
                      xs = map f (linspace a b n) --Get all the functions values spaced
                      rxs = map (\x -> x*(b-a) + a) (take n $ (randoms rx :: [Double])) --Random x
                      rys = map (\x -> x*(ymax-ymin) + ymin) (take n $ (randoms ry :: [Double])) --Random y
                      area = ((ymax-ymin)*(b-a)) 
                      count = foldr (\(x,y) (pc,nc) -> if y > 0 then 
                                                 if y < f x then (pc+1,nc) else (pc,nc)
                                                 else if y > f x then (pc,nc+1) else (pc,nc))
                                                 (0,0) coord
--Fixed point iteration
fpi :: (Double -> Double) -> Double -> Double
fpi f x = let x' = f x in if (abs $ x - x') < 1e-5 
                          then x' else fpi f x'

fpi' :: (Double -> Double) -> Double -> Int -> Double
fpi' f x iter = let x' = f x in if (abs $ x - x') < 1e-5 
                          then trace (show iter) x' else fpi' f x' (iter+1)


--Get numbers from binary
bin :: String -> Double
bin (x:xs) = sign * (number *  (1 + decimal))
         where sign = if x == '0' then 1 else -1
               exponent = take 11 xs
               number = 2^(sum [2^p | (i,p) <- zip exponent [10,9..], i == '1'] - 1023)
               mantissa = drop 11 xs
               decimal = sum [0.5**p | (i,p) <- zip mantissa [1..], i == '1']

-- Bisection: f(x) a b tolerance
bisec :: (Double -> Double) -> Double -> Double -> Double -> Double
bisec f a b tol 
          | (abs $ a - b) <= tol = x
          | otherwise = if fa*fx > 0 then bisec f x b tol 
                                     else bisec f a x tol
          where x = (a+b)/2
                fx = f x
                fa = f a

--Newton Rhapson: f(x) f'(x) guess tolerance
newrap :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
newrap f f' g tol = if (abs $ g - g') <= tol then g' else newrap f f' g' tol
          where g' = g - (f g/f' g)

newrap' :: (Double -> Double) -> Double -> Double -> Double
newrap' f g tol = if (abs $ g - g') <= tol then g' else newrap' f g' tol
          where g' = g - (f g/(cdiff f g))

-- Secant: f(x) p0 p1 tolerance
secant :: (Double -> Double) -> Double -> Double -> Double ->  Double
secant f p0 p1 tol
          | (abs $ p - p1) < tol = p
          | otherwise = secant f p1 p tol
          where q0 = f p0
                q1 = f p1
                p = p1 - q1*(p1-p0)/(q1-q0)

-- False position: f(x) p0 p1 tolerance
falpos :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
falpos f p0 p1 tol iter
         | (abs $ p - p1) < tol = trace (show iter) p
         | otherwise = if q * q1 < 0 then falpos f p1 p tol (iter + 1)
                                     else falpos f p p0 tol (iter + 1)
         where q0 = f p0
               q1 = f p1
               p = p1 - q1*(p1-p0)/(q1-q0)
               q = f p

--Evaluates a polynomial stored in a vector for a specified point
peval :: Num a => [a] -> a -> a
peval [] _ = 0
peval [c] _ = c
peval (c:cs) x = (c+) $ sum $ zipWith (*) cs [product $ replicate n x | n <- [1..]]

--Polynomial addition
padd :: Num a => [a] -> [a] -> [a]
padd [] [] = []
padd xs [] = xs
padd [] ys = ys
padd (x:xs) (y:ys) = (x+y) : padd xs ys

--Polynomial multiplication
pmult :: Num a => [a] -> [a] -> [a]
pmult [] _ = []
pmult (x:xs) ys = padd (map (*x) ys) (0:pmult xs ys)

--Polynomial differentiation
pdiff :: (Num a,Enum a) => [a] -> [a]
pdiff [] = []
pdiff (_:xs) = zipWith (*) xs [1..]

--Polynomial integration
pint :: (Fractional a,Enum a) => [a] -> [a]
pint [] = []
pint xs = 0 : zipWith (/) xs [1..]

--Infinitely rotates an array and stores the results 
rotall :: [a] -> [[a]]
rotall = scanl1 (\acc x -> tail acc ++ [head acc]) . repeat

--Calculates lagrange polynomial
lag' :: Fractional a => [[(a,a)]] -> [a]
lag' [] = []
lag' (p:ps) = padd poly $ lag' ps
     where p' = head p
           ps' = tail p
           terms = [[-xi,1] | (xi,yi) <- ps']
           numerator = foldr1 pmult terms
           denominator = product [fst p' - xi | (xi,yi) <- ps']
           poly = map (\x -> snd p' * x / denominator) numerator

--Lagrange wrapper
lag :: Fractional a => [(a,a)] -> [a]
lag ps = lag' $ take (length ps) (rotall ps)

--Cubic Spline
spline :: Fractional a => [(a,a)] -> [[a]]
spline ps 
         | length ps >= 4 = (lag $ take 4 ps) : (spline $ tail ps)
         | otherwise = []

--Finds the index of the of first value larger (for cubic spline)
findex :: (Fractional a, Ord a) => [(a,a)] -> a -> Int
findex ps t = head [i | (i,(x,y)) <- zip [0..] ps, x > t]

--Evalutes a cubic spline (Pass value, data points, and spline
splineval :: (Fractional a, Ord a) => a -> [(a,a)] -> [[a]] -> a
splineval t ps spl = sum $ zipWith (*) [0.25,0.50,0.25] [peval (spl !! i) t | i <- valid]
        where tdex = findex ps t
              three = [tdex-1,tdex-2,tdex-3]
              valid = take 3 $ cycle $ filter (\x -> x >= 0 && x < length spl) three

--Evaluates cubic spline, calculates spline for you (pass value and data points)
splineval' :: (Fractional a, Ord a) => a -> [(a,a)] -> a
splineval' t ps = splineval t ps $ spline ps

--FUNCTIONS FOR RREF-------------------------------------------------
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

--Actual rref function
rref :: (Fractional a,Eq a) => [[a]] -> [a]
rref = map last . reverse . bref . reverse . ref
-------------------------------------------------------------------------

--Constructs matrix for central difference approximations
cmat :: (Fractional a,Enum a) => Int -> [[a]]
cmat n = zipWith (\x y -> x ++ [y]) [map (^i) row | i <- [0..(n-1)]] col
       where bounds = fromIntegral $ div n 2
             col = 0 : 1 : replicate (n-2) 0.0
             row = [-bounds,(-bounds + 1)..bounds]

--Constructs matrix for forward difference
fmat :: (Fractional a,Enum a) => Int -> [[a]]
fmat n = zipWith (\x y -> x ++ [y]) [map (^i) row | i <- [0..(n-1)]] col
       where col = 0 : 1 : replicate (n-2) 0.0
             row = [0..fromIntegral n - 1]

--Constructs matrix for backwards difference
bmat :: (Fractional a,Enum a) => Int -> [[a]]
bmat n = zipWith (\x y -> x ++ [y]) [map (^i) row | i <- [0..(n-1)]] col
       where col = 0 : 1 : replicate (n-2) 0.0
             n' = fromIntegral (-(n-1))
             row = [n',n'+1..0]

--Cent/for/back diff of different orders (mat func ,y values, and spacing)
dord :: (Fractional a,Eq a,Enum a) => (Int -> [[a]]) -> [a] -> a -> a
dord f ys h = (1/h) * (sum $ zipWith (*) ys coefs)
         where mat = f $ length ys
               coefs = rref mat

--Takes at least 9 points, and returns the derivative for all points
dords :: (Fractional a, Eq a, Enum a) => [a] -> a -> [a]
dords ys h = (map (\ys' -> dord fmat ys' h) fwds) ++
             (map (\ys' -> dord cmat ys' h) ctrls) ++
             (map (\ys' -> dord bmat ys' h) bkwds)
      where strides = map (take 7) $
                      takeWhile ((>=7) . length) $ tails ys
            fwds = take 3 strides
            bkwds = drop (length strides - 3) strides 
            ctrls = strides

--Richardson Extrapolation
rextra :: [Double] -> Int -> Double
rextra [y] _ = y
rextra ys k = rextra [(a * y1 - y2)/(a-1) | (y1,y2) <- yss] (k+2)
     where yss = zip ys $ tail ys
           a = 2^k
