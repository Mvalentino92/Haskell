--Useful stat functions
import Data.List

--Largest value minus the smallest value
range :: [Int] -> Int
range xs = maximum xs - minimum xs

--Range/numberOfClasses
width :: Int -> [Int] -> Int
width n = ceiling . (flip (/) (fromIntegral n)) . fromIntegral . range

--Smallest value + largest value, divided by 2
midpoint :: [Int] -> Double
midpoint xs = ((realToFrac $ maximum xs) + (realToFrac $ minimum xs))/2.0

--For left bounds, start with smallest value, and add the width to it n number of times
--where n = number of classes
--For right bounds, they are (left bound + width - 1)
--Then count how many data values occur between the class bounds. 
--That's the frequency
freqTable :: Int -> [Int] -> [(Int,Int,Int)]
freqTable n xs = [(l,h,count l h) | l <- take n [min,(min + wth)..], let h = l + wth - 1]
             where wth = width n xs
                   min = minimum xs
                   count l h = sum [1 | x <- xs, x >= l && x <= h]

--Minus 0.5 from left bounds, and add 0.5 to right bounds
freqBoundary :: [(Int,Int,Int)] -> [(Double,Double,Int)]
freqBoundary = map (\(x,y,z) -> (realToFrac x - 0.5, realToFrac y + 0.5,z))

--Sum all the frequencies cumulatively
cumFreq :: Num a => [(a,a,Int)] -> [Int]
cumFreq = tail . scanl (\acc (x,y,z) -> acc + z) 0 

--Relative frequencies (Just pass it the length of the list (sum of frequencies) so it's easier!
--But it's each frequency divided by length of data (The sum will be 1)
relFreq :: Num a => [(a,a,Int)] -> [Double]
relFreq xs = map (\(x,y,z) -> (realToFrac z)/n) xs
      where n = realToFrac . last $ cumFreq xs
