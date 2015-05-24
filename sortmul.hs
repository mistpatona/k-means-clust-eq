import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import Data.Map (Map)
import NoComment

minWords :: Int -> String -> Bool
minWords n s = n <= length ( words s)

readOne :: [String] -> Maybe (String,Double)
readOne [] = Nothing
readOne [_] = Nothing
readOne (p:q:[]) = return (p,read q) 

showRes = map (\(s,x) -> s ++ " " ++ show x) . M.toList

loadInput = M.fromListWith max . maybe [] id . sequence . map (readOne . words) . filter (minWords 2) 

proceed = unlines . showRes . loadInput . noComments . lines

main = C.interact (C.pack . proceed . C.unpack)