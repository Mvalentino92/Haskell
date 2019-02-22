import Data.List

relax :: [Double] -> Int -> Int ->  [[Double]]
relax ns start bound
         | start <= bound = ns' :  relax ns' (start + 1) bound
         | otherwise = []
           where neighbors = zip ns $ drop 2 ns
                 middle = map (\(x,y) -> (x+y)/2) neighbors
                 ns' = (head ns : middle) ++ [last ns]

relax' :: [Double] -> Int -> Int ->  [[Double]]
relax' ns start bound
         | start <= bound = ns' :  relax ns' (start + 1) bound
         | otherwise = []
           where neighbors = map (take 3) $ takeWhile ((>=3) . length) $ tails ns 
                 middle = map (\x -> (sum x)/3) neighbors
                 ns' = (head ns : middle) ++ [last ns]
