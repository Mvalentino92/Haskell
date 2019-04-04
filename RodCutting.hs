--Solves the rod cutting problem
rod :: Int -> [Int] -> [Int] -> Int
rod n ps rs 
    | rl == n = last rs
    | otherwise = rod n ps rs'
    where rl = length rs
          half = div rl 2 + mod rl 2
          p = ps !! rl
          rs' = rs ++ [maximum $ p : take half (zipWith (+) rs $ reverse rs)]

