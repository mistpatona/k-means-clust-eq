{-# LANGUAGE NoMonomorphismRestriction #-}

import NoComment
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import Data.Map (Map)
import FloatingInterp
import TableFromMaps (stringMapsToTable)
import RoughOrd
import Data.List (intercalate)
import Data.Maybe (fromMaybe,listToMaybe)
import SolveEquation
import System.Environment (getArgs)

helpStrings = [
  " -t  Top shoulder % (100)"
 ," -b  Bottom shoulder % (35)"
 ," -m  Multiplier (1.0) -- not implemented now" --if not given, must be calculated
 ," -r  Required resulting number (75% from total capacity)" 
 ," -B  Bottom start % (20)"
 ," -T  upper Threshold % (99.9)"
 ," -p  Precision % (1) -- not implemented" ]

defaultParams = [("t",100),("b",20),("m",1.0),("B",15),("T",99.9)]
realParams  = defaultParams

getMRequiredMax =  lookup "r" 
getMMedianModifier =  lookup "m" 

getMaxLoadAfterMultiply = maybe 1.0 (*0.01) . lookup "T" 
getMinLoadToMultiply = maybe 0.2 (*0.01) . lookup "B"
getTopShoulder =     maybe 1.0 (*0.01) . lookup "t"  
getBottomShoulder =  maybe 0.35 (*0.01) . lookup "b" 
expansionCurveSrc p =            [(0,0)
                                ,(getMinLoadToMultiply p,0)
                                ,(getBottomShoulder p,1)
                                ,(getTopShoulder p,1) 
                                ,(1.01,0)]

maxLoadAfterMultiply = getMaxLoadAfterMultiply realParams
maxLoadToMultiply = maxLoadAfterMultiply
minLoadToMultiply = getMinLoadToMultiply realParams

minimalMaxCap = 50 -- that is /26 in real
minimalAbsLoad = 15

expansionCurveData = M.fromList $ expansionCurveSrc realParams

data Unit = Unit { maxCap :: Double,
                   realLoad :: Double } 
                   deriving (Show,Eq,Ord)

canBeExtended :: Unit -> Bool
canBeExtended u = (maxCap u >= minimalMaxCap) &&
                  (realLoad u >= minimalAbsLoad) &&
                  (calcLimit u ~> 1)

calcLimit :: Unit -> Double
calcLimit (Unit c v) = if (c * minLoadToMultiply < v)&&(c * maxLoadToMultiply > v) 
      then c * maxLoadAfterMultiply / v else 1

loadUnit :: Unit -> Double 
loadUnit (Unit c v) = if ((c < 10) || (v < 5)) then 0 else v/c

elemSum :: (Ord k, Num a) => Map k a -> a
elemSum = sum . M.elems   

{- Pack of functions to work on Map k Unit -}
maxCaps = M.map maxCap 
realLoads = M.map realLoad 
sumReal = elemSum . realLoads -- Map -> Num
sumCap = elemSum . maxCaps -- Map -> Num 
multiplables = M.filter canBeExtended 
notMultiplables = M.filter (not . canBeExtended) 

putKoef :: Ord zz => Double -> Map zz Unit -> Map zz Double
putKoef kk d = M.map (createKoef2 kk) d

createKoef1 :: Double -> Unit -> Double
createKoef1 kk u =  min kk $ calcLimit u

createKoef2 :: Double -> Unit -> Double
createKoef2 kk u = min (calcLimit u) $ 1 + (kk-1) * k
  where k = fromMaybe 0 $ floatInterp expansionCurveData (loadUnit u)

multUnitBy :: Unit -> Double -> Double
multUnitBy (Unit c v) m = m*v

applyMult :: Ord k => Map k Double -> Map k Double -> Map k Double 
applyMult p d = M.differenceWith (\u m -> Just $ m*u) p d -- multUnitBy

getWrongMults :: Ord k => Map k Unit -> Map k Double -> [k]
getWrongMults p d = M.keys $ M.filter not $ M.intersectionWith checkLimit p d

checkLimit :: Unit -> Double -> Bool
checkLimit (Unit c v) m = m*v <= c
 
mkStatUnits :: Ord k => Map k Unit -> String ->  Double
mkStatUnits = flip mkStatUnits'
mkStatUnits' "srcSize"   = sumReal . multiplables
mkStatUnits' "nonexSize" = sumReal . notMultiplables
mkStatUnits' "srcCap"    = sumCap . multiplables 
mkStatUnits' "nonexCap"  = sumCap . notMultiplables 
 

mkMedianKoef :: Ord k => Double -> Map k Unit -> Double
mkMedianKoef reqmax d = (reqmax - mkStatUnits d "nonexSize") / mkStatUnits d "srcSize"
-- liftM2 (/)  (mkStatUnits' "dstSize") (mkStatUnits' "srcSize") -- ??????

mkStat :: Ord k => Map k Double -> String ->  Double
mkStat m "size" = fromIntegral $ M.size m
mkStat m "sum"  = sum $ M.elems m
mkStat m "max"  = maximum $ M.elems m
mkStat m "min"  = minimum $ M.elems m

mkStatistics :: Ord k => Map k Double -> (Int,Double,Double,Double)
mkStatistics m = (M.size m, sum e, minimum e, maximum e)
    where e = M.elems m 

(+-+) :: String -> String -> String
a +-+ b = a ++ " " ++ b

showWithSlashes :: Show a => String -> [a] -> String
showWithSlashes s = (s++) . intercalate "/" . map show

showReqStatitics :: Ord k => Map k Unit -> [String]
showReqStatitics m = zipWith (+-+) ["Expandables real/cap size:",
                                  "Non-expandables real/cap size:"] nums
   where nums = map (intercalate "/" . map (show . mkStatUnits m) . words )  
                   ["srcSize srcCap",
                    "nonexSize nonexCap"] 

showMapStatistics :: Ord k => Map k Double -> String
showMapStatistics m = "Size: " ++ show a ++ " Sum: " ++ show b ++ 
     " min: " ++ show c ++ " max: " ++ show d
    where (a,b,c,d) = mkStatistics m 

showMapContents :: (Ord k) => [Map k String] -> [String]
showMapContents = stringMapsToTable 

mkNamesMap :: (Ord k) => Map k a -> Map k k
mkNamesMap = M.mapWithKey const
          
commentOut :: [String] -> [String]
commentOut = map ("# " ++)

showRes :: Map String Unit -> [String]  
showRes src = 
	            "Input Stats - MAXLOAD:" 
            : showMapStatistics (maxCaps src)
            : "Input Stats - REALLOAD:" 
            : showMapStatistics (realLoads src)
            : []  
        
showRes' par src = 
   commentOut (showRes src ++ showReqStatitics src)  ++
   (  showMapContents (mkNamesMap src : map (M.map show) 
                [ maxCaps src, realLoads src, km, expandResults ])) ++
   commentOut [resultSumStat, discrepancyStat, koefStatsActual ] 
  where theoreticTotal = sumCap src
        requiredMax = maybe (theoreticTotal * 0.75) id $ getMRequiredMax par
        
        k = max 1 $ someAnswer 0.01 $ take 100 $ iterSolutions f (1,6)
        f x = mkExpandResultsSum src x < requiredMax
        km = putKoef k $ multiplables src
        expandResults = applyMult (realLoads src) km
        endSum = mkStat expandResults "sum"
        
        resultSumStat = showWithSlashes "Result sum/required: " [endSum, requiredMax] 
        discrepancyStat = showWithSlashes "Discrepancy,% : " [(endSum-requiredMax)*100/requiredMax]
        koefStatsActual = showWithSlashes "Actual koef: " [k]
              
mkExpandResultsSum src k = mkStat (applyMult (realLoads src) (putKoef k $ multiplables src)) "sum"

{- how to use:
  let's calculate sqrt 5 :
*SolveEquation> let f x = x*x < 5
*SolveEquation> someAnswer 0.01 $ iterSolutions f (1,5)
2.23828125
-}

{-  input file format:
  unit_name::String unit_max_capacity::Number unit_real_load::Number

  realMax  = sum (unit_real_load)
  requiredMax = given value

  output file format:
  unit_name unit_multiplier
-}

minWords :: Int -> String -> Bool
minWords n s = n <= length ( words s)

loadInput1 :: String -> (String, Unit)
loadInput1 s = (name,ds)
    where (name:ss) = words s
          ds = maybe (Unit 0 0) id $ readUnit ss

readUnit :: [String] -> Maybe Unit
readUnit [] = Nothing
readUnit [_] = Nothing
readUnit (p:q:[]) = return $ Unit (read p) (read q) 


loadInput = M.fromList . map loadInput1 . filter (minWords 3) 

proceed p = unlines . showRes' p . loadInput . noComments . lines 

oneArg :: IO (Maybe String)
oneArg = do
    args <- getArgs  
    return $ listToMaybe args

mkParams :: String -> [(String,Double)]
mkParams s = [("r",read s)]

main = do
      a <- oneArg
      let rParams = (fromMaybe [] $ fmap mkParams a)
      C.interact (C.pack . proceed rParams . C.unpack) 

