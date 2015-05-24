module TableFromMaps
    (stringMapsToTable) 
   where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (transpose)

-- show multiple maps of Strings as Strings

uniteMaps :: Ord k => a -> [Map k a] -> [[a]]
uniteMaps d ms = map (getByKeyWithDefault d ms) ks 
    where ks = M.keys $ M.unions ms

getByKeyWithDefault :: Ord k => a -> [Map k a] -> k -> [a]
getByKeyWithDefault def ms x = map f ms
    where f = fromMaybe def . M.lookup x

columnWidths :: [[[a]]] -> [Int]
columnWidths = map (maximum . map length) . transpose

extendWithSpacesTo :: Int -> String -> String
extendWithSpacesTo n s = s ++ replicate (n-l) ' '
   where l = length s

stringMapsToTable :: Ord k => [Map k String] -> [String]
stringMapsToTable ms = map (unwords . zipWith extendWithSpacesTo cw ) ss
    where ss = uniteMaps "_" ms
          cw = columnWidths ss