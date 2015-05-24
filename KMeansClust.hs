module KMeansClust(
        kmcCi
       ,kmcNms
       ,kmcClusters
       ,groupxs
       ,iterateCentroids
       ,packDistortion
       ,totalClusterDistortion
       ,KMeansClusteringSpace(..)
       ,KMeansClusteringConstansts(..)
       ,KMeansClusteringSource(..)
       ,SrcPoint(..)
       ,Centroid(..)
       ,unSrcPoint
       ,unCentroid
       ,convergeAllCentroids
       ,convergeCentroids
       ,indexBestBy
       )
        where
-- k-means clustering algorythm.

import Data.Maybe (fromJust,catMaybes)
import Data.List (sortBy,unfoldr)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M

data KMeansClusteringSpace d =
  KMeansClusteringSpace {   fAverage :: [d] -> d,
                            fDistance :: d -> d -> Double }

data KMeansClusteringConstansts =
   KMeansClusteringConstansts {
      kmcMaxIterations :: Int,
      kmcGenerateCentroidGroups :: Int,
      kmcSelectCentroidGroups :: Int
       } deriving (Eq,Show)

data KMeansClusteringSource d =
   KMeansClusteringSource {
    kmcSpace :: KMeansClusteringSpace d,
    kmcConst :: KMeansClusteringConstansts,
    kmcSrc :: [SrcPoint d] }

newtype SrcPoint d = SrcPoint d
    deriving (Eq,Show)
newtype Centroid d = Centroid d
    deriving (Eq,Show)

unSrcPoint (SrcPoint x) = x
unCentroid (Centroid x) = x

kmcCi :: (KMeansClusteringSource a) -> [Centroid a] -> [Int]
kmcCi k ms = ci dis xs (map unCentroid ms)
    where   dis = fDistance $ kmcSpace k
            xs  = map unSrcPoint $ kmcSrc k

{- using d as "distance" function,
   find an index of nearest m for each x   -}
ci :: Ord a => (b->b->a) -> [b] -> [b] -> [Int]
ci d xs ms = map (ci' d ms) xs
    where ci' d ms x = indMin $ map (d x) ms

{- a function to find new centroids:
   new centroid is an average of those x's
   for whom old centroid was the nearest (so, "best") one -}
kmcNms :: Ord i => (KMeansClusteringSource a) -> [i] -> [Centroid a]
kmcNms k is = map Centroid $ map ave $ groupxs is xs
    where   dis = fDistance fs
            xs  = map unSrcPoint $ kmcSrc k
            ave = fAverage fs
            fs = kmcSpace k

{- group input using centroids -}
kmcClusters :: (KMeansClusteringSource a) -> [Centroid a] -> [[SrcPoint a]]
kmcClusters k ms = groupxs (kmcCi k ms) xs
    where xs  = kmcSrc k



indMin :: Ord a => [a] -> Int
indMin = indexBestBy (<)

indexBestBy :: (a->a->Bool) -> [a] -> Int -- indexes start from zero, like in (!!)
indexBestBy _ [] = -1 -- oh shit!
indexBestBy fstIsBetter xs = snd $ foldr1 f' $ zip xs [0..]
    where f' x1 x2 = if ((fstIsBetter `on` fst) x1 x2) then x1 else x2

bestBy :: (a->a->Bool) -> [a] -> a
bestBy fstIsBetter = foldr1 f'
    where f' x1 x2 = if (fstIsBetter x1 x2) then x1 else x2

{- group second argument accordind to flags in first argument -}
groupxs :: Ord b => [b] -> [a] -> [[a]]
groupxs ms xs = map (map fst) $ unfoldr (t1m snd) p'
   where ps = zip xs ms
         p' = sortBy (compare `on` snd) ps

         t1m :: Eq b => (a->b) -> [a] -> Maybe ([a],[a])
         t1m _ [] = Nothing
         t1m f (x:xs) = Just $ span ( ((==) `on` f) x ) (x:xs)

-- ---------------------

iterateCentroids :: KMeansClusteringSource a -> [Centroid a] -> [[Centroid a]]
iterateCentroids k ms = iterate f ms
    where f = (kmcNms k) . (kmcCi k)

-- ---------------------

packDistortion :: KMeansClusteringSpace a -> [a] -> Double
packDistortion k xs = sum $ map (dist m) xs
    where  m = avr xs
           avr = fAverage k
           dist = fDistance k

totalClusterDistortion :: KMeansClusteringSpace a -> [[SrcPoint a]] -> Double
totalClusterDistortion k cs =
        sum $ coverZerosWithSmallest $ map (packDistortion k . map unSrcPoint) cs

coverZerosWithSmallest :: (Ord a, Num a) => [a] -> [a]
coverZerosWithSmallest xs = map f xs
   where isZero = (==0)
         smallest = minimum . filter (not . isZero)
         f x = if isZero x then smallest xs else x

------------------------

firstNeighbourMatch :: Eq a => [a] -> Maybe a
firstNeighbourMatch [] = Nothing
firstNeighbourMatch [x] = Nothing
firstNeighbourMatch (x:y:xs) = if x==y then Just x else firstNeighbourMatch (y:xs)

convergeAllCentroids :: Eq a => KMeansClusteringSource a -> [[Centroid a]] -> [[Centroid a]]
convergeAllCentroids k  = catMaybes . map (convergeCentroids k)

convergeCentroids :: Eq a => KMeansClusteringSource a -> [Centroid a] -> Maybe [Centroid a]
convergeCentroids k = (firstNeighbourMatch . take (kmcMaxIterations c) . iterateCentroids k)
   where c = kmcConst k



