-- interpolation 1D

import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative ( (<$>), (<|>), liftA2 )
import Data.Function (on)
import Data.Maybe (fromMaybe)

data GenericPoint s d = GenericPoint { pStamp :: s, pData :: d }
     deriving Show
  -- s : independent axis, d : dependent axis
data IPoints point = NoPoints 
            | ExactPoint point
            | BetweenPoints point point
            deriving (Show,Eq)
type FPoint = GenericPoint Double Double
type FPoints = IPoints FPoint

fInterpolate :: FPoints -> Double -> Maybe Double
fInterpolate NoPoints _ = Nothing
fInterpolate (ExactPoint p) _ = Just $ pData p
fInterpolate (BetweenPoints g1 g2) x = Just $ if x1==x2 then y1 else y
    where x1 = pStamp g1
          x2 = pStamp g2
          dx = x2 - x1    -- a special case is dx==0 but for floating it is not so simple
          y1 = pData g1
          y2 = pData g2
          y = y1 + (x - x1) * (y2 - y1) / dx 

getMapPoints :: Ord k => Map k a -> k -> IPoints (GenericPoint k a)
getMapPoints m x = fromMaybe NoPoints r 
	where l = (ExactPoint . GenericPoint x) <$> M.lookup x m
	      lo = tupToPoint <$> M.lookupLE x m
	      hi = tupToPoint <$> M.lookupGE x m
	      tupToPoint (k,v) = GenericPoint k v
	      b = liftA2 BetweenPoints lo hi
	      b1 = if ((==) `on` (pStamp <$>)) lo hi 	-- harsh, but no (Eq a) restriction!
	      	    then fmap ExactPoint lo else b
	      r = l <|> b1 -- 1st is prefered to 2nd, all functor features in this function are around Maybe monad

floatInterp :: Map Double Double -> Double -> Maybe Double
floatInterp m x = fInterpolate (getMapPoints m x) x
	



