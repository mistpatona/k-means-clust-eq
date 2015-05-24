module NoComment (noComments
	             ,isCommentLine) 
     where
{- part one: how to remove # comments and empty lines -}

isCommentLine :: String -> Bool
-- isCommentLine = not . all id . (flip map) [isEmpty ] -- , isCommentHead . strip]
isCommentLine s = isEmpty s || isCommentHead (strip s)

isEmpty :: String -> Bool
isEmpty = null . words

goodHead :: [a] -> Maybe a
-- goodHead = return  -- Must be some short monadic way ! 
goodHead [] = Nothing
goodHead (x:_) = Just x

isCommentHead :: String -> Bool
isCommentHead = (== Just '#') . goodHead

strip = unwords . words

noComments :: [String] -> [String]
noComments = filter (not . isCommentLine)