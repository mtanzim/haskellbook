module SafeHead where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (a:_) = Just a

