module AoC.Utils where

import Data.Map as M

orderPair :: Ord a => (a, a) -> (a, a)
orderPair (a, b)
  | a < b = (b, a)
  | otherwise = (a, b)

pairsCount :: Ord a => [(a,a)] -> [((a,a), Int)]
pairsCount = M.toList . go M.empty
 where
   go mp [] = mp
   go mp (x:xs) = go newM xs
    where
      newM = M.insertWith (+) (orderPair x) 1 mp


