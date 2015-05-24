module KMeansEqFlags
 (kmClusterizeFlags)
  where

-- k-means clustering algorythm.
-- applied to some objects
-- which are sets (lists) of Eq'able flags

--import Data.Maybe (fromJust,catMaybes)
import Data.List (sortBy,sort,subsequences,nub,unfoldr,intersect,(\\),intercalate)
import Data.Function (on)
import Data.Maybe (catMaybes,fromJust)
import KMeansClust

import System.Random
import SeqWork
import OnEqStrings

type D = [Double]

mkDictionary :: Ord a => [[a]] -> [a]
mkDictionary xs = mkDictionary' xs \\ commons
          where commons = foldr1 intersect xs
                mkDictionary' :: Ord a => [[a]] -> [a]
                mkDictionary' = sort . nub . concat

setToD :: Eq a => [a] -> [a] -> D
setToD dict [] = take (length dict) (repeat (-1.0)) -- a special case
setToD dict s = map cd dict
    where  cd c = if (c `elem` s) then 1.0 else 0.0

flagsToDs :: Ord a => [[a]] -> [D]
flagsToDs xs = map (setToD (mkDictionary xs)) xs

-- define main functions in our (being clusterized) space
-- squared distance :

euclidianSpace = KMeansClusteringSpace {
        fDistance = distance2,
        fAverage =  average }
        where
            distance2 :: D -> D -> Double
            distance2 x y = sum $ map (^2) $ zipWith (-) x y
            distance2' x y = sum $ zipWith (*) [1,1.05..] $ map sqr $ zipWith (-) x y
                        where sqr p = p * p
            -- sum :
            plus :: D -> D -> D
            plus = zipWith (+)
            -- scale by "Real" factor :
            scale :: Double -> D -> D
            scale k  = map (k *)
            -- ------------------------------
            total :: [D] -> D
            total = foldr1 plus
            average :: [D] -> D
            average xs = scale (1 / (fromIntegral $ length xs)) (total xs)
            -- -------------------------------

standardKmcConst = KMeansClusteringConstansts 10 999 5
{-
k1 = KMeansClusteringSource {
    kmcSpace = euclidianSpace,
    kmcConst = standardKmcConst,
    kmcSrc = map SrcPoint $ flagsToDs xs }
xs = words $ "abcd abcf abcg abch abcj abck qwe qwr qwt qwy qwu qwi pom pon pob pov poc pox poz 123 546 659 324 607 706 341 768 " ++
  "abc acd sdfgkjh fgkg hjf fhdjkgk sdfjhgsdf sdjhds sadds gfhsf lghjk fjhkdf dfkdfjdflsl dsl aslkaslk dlksdlsk dlsdkjsfhsdhgdfksd  aaddasasd dferwc"
-}

bestCentroids :: Eq a => KMeansClusteringSource a -> Int -> [[Centroid a]]
bestCentroids k n = sortBy (compare `on` totalClusterDistortion ks . conv . kmcCi k) $ convergeAllCentroids k $ prepareCentroidGroups k n
     where  conv = (flip groupxs) (kmcSrc k)
            ks = kmcSpace k

prepareCentroidGroups :: KMeansClusteringSource a -> Int -> [[Centroid a]]
prepareCentroidGroups k i = map (map Centroid) $ reverse $ sortBy (compare `on` clusterDistortion) $
        subGroups (map unSrcPoint $ kmcSrc k) i
        where clusterDistortion = packDistortion (kmcSpace k)

subGroups :: [a] -> Int -> [[a]]
subGroups xs i = if length xs < 10 then subs2all xs i
                                   else take (i*20) $ subs2R xs i
    where
        subs2all :: [a] -> Int -> [[a]]
        subs2all xs k = filter (\x -> (length x > 1)
                && (length x <= k) ) $ subsequences xs

        subs2R xs i = formNubSeqs i xs rs
            where   seed = 12  -- :))
                    rs = randomRs (0,length xs -1) (mkStdGen seed)

findNearestPair :: (a->a->Double) -> [a] -> (a,a)
findNearestPair d xs = head ss
    where xs' = zip [0..] xs
          ps = allOrdPairs 0 (length xs -1)
          ps' = map (\(p,q) -> (xs !! p,xs !! q)) ps
          ss = sortBy (compare `on` uncurry d) ps'

minInterclusterDistance :: KMeansClusteringSource a -> [Centroid a] -> Double
minInterclusterDistance k ms = f p q
    where (p,q) = findNearestPair f ms
          f = (fDistance $ kmcSpace k) `on` unCentroid

validityMeasure :: KMeansClusteringSource a -> [Centroid a] -> Double
validityMeasure k ms = intra / inter
    where   intra = (totalClusterDistortion ks . conv . kmcCi k) ms / fromIntegral n
            inter = minInterclusterDistance k ms
            src = kmcSrc k
            n = length src
            ks =  kmcSpace k
            conv = (flip groupxs) src

groupsFromCentroids :: KMeansClusteringSource a -> [Centroid a] -> [[SrcPoint a]]
groupsFromCentroids k ms = (flip groupxs) (kmcSrc k) $ kmcCi k ms

worstPack :: KMeansClusteringSpace a -> [[a]] -> Int
worstPack k = indexBestBy ( (>) `on` packDistortion k)

worstIndex :: KMeansClusteringSource a -> [Centroid a] -> Int
worstIndex k ms = worstPack (kmcSpace k) $ map (map unSrcPoint)  $ (flip groupxs) (kmcSrc k) $ kmcCi k ms

dropNth :: [a] -> Int -> [a]
dropNth xs n = take n xs ++ drop (n+1) xs

increaseCentroids :: Eq a => KMeansClusteringSource a -> [Centroid a] -> [Centroid a]
increaseCentroids k ms = dropNth ms i ++ newcs
    where i = worstIndex k ms
          newcs =  head $ bestCentroids (k {kmcSrc=k'}) 2
          k' = groupsFromCentroids k ms !! i

nextCentroidStep :: Eq a => KMeansClusteringSource a -> [Centroid a] -> [Centroid a]
nextCentroidStep k = fromJust . convergeCentroids k . increaseCentroids k

allCentroidSteps :: Eq a => KMeansClusteringSource a -> [[Centroid a]]
allCentroidSteps k = iterate (nextCentroidStep k) $ head $ bestCentroids k 2

chooseBestCentroids :: Eq a => KMeansClusteringSource a -> Int -> [Centroid a]
chooseBestCentroids k limit = head $ sortBy (compare `on` vm) $ take limit $ allCentroidSteps k
  where vm x = validityMeasure k x * clusterCountPreference (length x)
-- TODO: change maximum and minimum preference when element count is small
clusterCountPreference :: Int -> Double
clusterCountPreference n =  -- lower is better
   f n where f 1 = 1.3
             f 2 = 1
             f 3 = 1
             f 4 = 1
             f n = 1 + (fromIntegral n-5)*0.1


kmClusterizeFlags :: Ord a => [[a]] -> [[[a]]]
kmClusterizeFlags xs =
 (flip groupxs) xs $ kmcCi k $ chooseBestCentroids k c
 where
  c = min 1 {- 7 -} (length xs `div` 2)  -- maximum 7+1 clusters, but no more clusters than half elements number
  k = KMeansClusteringSource {
    kmcSpace = euclidianSpace,
    kmcConst = standardKmcConst,
    kmcSrc = map SrcPoint $ flagsToDs xs }


