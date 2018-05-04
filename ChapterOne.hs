-- Example 1
double a = (a+a)*2

-- Example 2
sum' [] = 0
sum' (x:xs) = x + sum xs

-- Example 3
product' [] = 1
product' (x:xs) = x * product xs

-- Example 4
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
      where larger = [a | a <- xs, a >= x]
            smaller = [b | b <- xs, b < x]

factorial n = product' [1..n]

map' a b = a + b


