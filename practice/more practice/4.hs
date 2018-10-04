filterLimit :: (a -> Bool) -> Int -> [a] -> [a]
filterLimit f limit [] = []
filterLimit f limit (x:xs)
  | f x == True = x : filterLimit f limit xs
  | otherwise   = filterLimit f limit xs
