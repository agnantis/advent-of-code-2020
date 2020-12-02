module AoC.Day1 where

import Control.Arrow ((&&&))

type Input = [Int]
type Output = Int

total :: Int
total = 2020

fstStar :: Input -> Output
fstStar = go
 where
  go [] = error "Unable to find a pair"
  go (x:xs) = case (safeHead $ filter (\y -> x + y == total) xs) of
                Just y -> x * y
                Nothing -> go xs

sndStar :: Input -> Output
sndStar = undefined

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


main :: IO  ()
main = do
  input <- fmap read . lines <$> readFile "src/input/day1" 
  print . (fstStar &&& sndStar) $ input
