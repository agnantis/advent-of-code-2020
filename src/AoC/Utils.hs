module AoC.Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

fromEither :: a -> Either b a -> a
fromEither a (Left _) = a
fromEither _ (Right x) = x

-- | 
-- >>> splitOn ',' "a,bc,d,e"
-- ["a", "bc", "d", "e"]
--
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn v xs = 
  let (ys, zs) = break (== v) xs
      zs' = if null zs then zs else tail zs
   in ys:(splitOn v zs')

error' :: a
error' = error "Unable to find an answer"

