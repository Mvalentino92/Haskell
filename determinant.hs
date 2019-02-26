det :: [[Double]] -> Double
det mat 
    | length mat == 2 = foldr1 (-) $ zipWith (*) (head mat) (reverse $ last mat)
    | otherwise = sum $ zipWith (*) coefs $ map det mat'
    where coefs = zipWith (*) (cycle [1,-1]) (head mat)
          l = length mat - 1
          mat' = stitch [[j | (index,j) <- zip [0..] i, index /= n] | n <- [0..l], i <- tail mat] l
          stitch [] _ = []
          stitch xss n = take n xss : stitch (drop n xss) n


det' :: [[Double]] -> Double
det' mat 
    | length mat == 2 = foldr1 (-) $ zipWith (*) (head mat) (reverse $ last mat)
    | otherwise = sum $ zipWith (*) coefs $ map det' mat'
    where coefs = zipWith (*) (cycle [1,-1]) (head mat)
          mat' = [map (\x -> take i x ++ drop (i+1) x) (tail mat) | i <- [0..length mat - 1]]
