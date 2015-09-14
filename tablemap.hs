import qualified Data.Map as M
import Data.Map (Map)


-- show multiple maps of Strings as Strings

uniteMaps :: Ord k => a -> [Map k a] -> [[a]]
uniteMaps d ms = map (getByKeyWithDefault d ms) ks 
    where ks = M.keys $ M.unions ms

getByKeyWithDefault :: Ord k => a -> [Map k a] -> k -> [a]
getByKeyWithDefault def ms x = map f ms
    where f = M.findWithDefault def x
