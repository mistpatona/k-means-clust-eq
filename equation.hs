{- iterative (f x = z) equation solution
   f is ascending  -}

import Data.Maybe (listToMaybe,fromMaybe)
-- import Control.Applicative ( (<$>) )

convergedAnswer :: ((Double,Double) -> Bool) -> [(Double,Double)] -> Maybe (Double,Double)
convergedAnswer closeEnough = listToMaybe . dropWhile (not . closeEnough)

someAnswer :: Double -> [(Double,Double)] -> Double
someAnswer delta = someAnswerWith (areWithin delta)

someAnswerWith :: ((Double,Double) -> Bool) -> [(Double,Double)] -> Double
someAnswerWith closeEnough xs = median ans
    where ans = fromMaybe (best xs) $ convergedAnswer closeEnough xs
          best = last -- a simple approach

areWithin :: Double -> (Double,Double) -> Bool
areWithin delta = (<=delta) . abs . (uncurry (-))

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

median = (/2) . (uncurry (+)) -- how to choose next x
-- f k = "is F(k) (above|below, take one) some (zero) level?"
genIter ::  ((Double,Double) -> Double)  -> (Double -> Bool) -> (Double,Double) -> (Double,Double) 
genIter xx f (x1,x2) = if good1 then (x1,x) else (x,x2)
    where x = xx (x1,x2)
          good1 = f x1 `xor` f x
          good2 = f x2 `xor` f x -- unused line, for symmetry :)

genIterMedian = genIter median

iterSolutions = iterate . genIterMedian 




