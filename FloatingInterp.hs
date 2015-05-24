module FloatingInterp ( getMapPoints,
	                    floatInterp
	                  )  where

-- interpolation 1D

import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative ( (<$>), (<*>) )
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (nubBy)

-- | interpolate from a small array of points
--  (two points is the main case)
fInterpolateMul :: (Eq a, Fractional a, Num b) => (a->b->b) -> [(a,b)] -> a -> Maybe b
fInterpolateMul _ [] _ = Nothing
fInterpolateMul _ [(_,p)] _ = Just p
fInterpolateMul mul [g1,g2] x = Just $ if x1==x2 then y1 else y
    where x1 = pStamp g1
          x2 = pStamp g2
          dx = x2 - x1    -- a special case is dx==0 but for floating it is not so simple
          y1 = pData g1
          y2 = pData g2
          dy = y2 - y1
          y = y1 + ( ((x - x1) / dx) `mul` dy) 
          pStamp = fst
          pData = snd
fInterpolateMul mul xs x = fInterpolateMul mul [head xs, last xs] x -- just in case

fInterpolate :: (Eq a, Fractional a) => [(a,a)] -> a -> Maybe a
fInterpolate = fInterpolateMul (*)

-- | return list of from 0 to 2 elements, 
--   closest to a given key
getMapPoints :: Ord k => Map k a -> k -> [(k,a)]
getMapPoints m x = fromMaybe [] r 
	where d = map uncurry [M.lookupLE, M.lookupGE] <*> [(x,m)]  
	      r = nubBy ((==) `on` fst) <$> sequence d 

-- | interpolate from a pack of points
floatInterp :: Map Double Double -> Double -> Maybe Double
floatInterp m x = fInterpolate (getMapPoints m x) x
	

{- EXAMPLE:
 sequence $ floatInterp  (Data.Map.fromList [(0,0),(20,0),(35,4),(75,4),(90,0),(100,0)]) <$> [0,30,35,70,77,99]  
 = Just [0.0,2.6666666666666665,4.0,4.0,3.466666666666667,0.0] 
    -}


