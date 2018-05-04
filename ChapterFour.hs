halve :: [a] -> ([a],[a])
halve [] = error "Empty List"
halve xs
      | mod len 2 /= 0 = error "Needs to be an even lengthed list"
      | otherwise = (take half xs, drop half xs)
      where half = (fromIntegral $ length xs) `div` 2
            len = length xs

third :: [a] -> a
third (_:_:x:xs) = x

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

luhnDouble :: Int -> Int
luhnDouble n 
           | double > 9 = double - 9
           | otherwise = double
           where double = n + n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (sum $ (map luhnDouble [a,c]) ++ [b,d]) `mod` 10 == 0
