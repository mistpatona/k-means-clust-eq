module Explode where

import Data.List (unfoldr)

explode :: (a->Bool) -> [a] -> [[a]]
explode f xs = unfoldr cut (Just xs) 
   where 
    cut = fmap (mapSnd maybeTail . break f) -- fmap over Maybe functor
    maybeTail x = if null x then Nothing else Just (tail x)
    mapSnd f (a,b) = (a,f b)

