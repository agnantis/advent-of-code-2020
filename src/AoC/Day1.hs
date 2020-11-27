module AoC.Day1 where

import Control.Arrow ((&&&))

type Input = [Int]
type Output = Int

fstStar :: Input -> Output
fstStar = undefined

sndStar :: Input -> Output
sndStar = undefined

main :: IO  ()
main = do
  input <- fmap read . lines <$> readFile "src/input/day1" 
  print . (fstStar &&& sndStar) $ input
