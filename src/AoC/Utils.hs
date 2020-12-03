module AoC.Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

error' :: a
error' = error "Unable to find an answer"

