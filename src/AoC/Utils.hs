module AoC.Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

fromEither :: a -> Either b a -> a
fromEither a (Left _) = a
fromEither _ (Right x) = x

error' :: a
error' = error "Unable to find an answer"

