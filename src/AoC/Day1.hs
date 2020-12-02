module AoC.Day1 where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)


type Input = [Int]
type Output = Int

total :: Int
total = 2020

error' :: a
error' = error "Unable to find an answer"

fstStar :: Input -> Output
fstStar = fromMaybe error'
        . fmap (uncurry (*))
        . helper total

sndStar :: Input -> Output
sndStar = go
 where
   go (x:xs) = case (helper (total - x) xs) of
                 Just (a, b) -> x * a * b
                 Nothing -> go xs
   go _ = error'

helper :: Int -> Input -> Maybe (Int, Int)
helper _ [] = Nothing
helper _ [_] = Nothing
helper i (x:xs) = case (safeHead . filter (\y -> x + y == i) $ xs) of
              Just y -> Just $ (x, y)
              Nothing -> helper i xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO  ()
main = do
  input <- fmap read . lines <$> readFile "src/input/day1" 
  print . (fstStar &&& sndStar) $ input
