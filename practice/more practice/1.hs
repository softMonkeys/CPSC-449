percentLookup :: [a] -> Float -> a
percentLookup [] a = error "empty list"
percentLookup (x:xs) a
 | a < 0.0   = x
 | a > 100.0 = last (x:xs)
 | otherwise = (x:xs) !! (round((a / 100.0) * (fromIntegral (length (x:xs)))))
