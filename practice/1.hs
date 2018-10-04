getSequence :: Int -> [Int]
getSequence 1 = [1]
getSequence n
  | n `rem` 2 == 0 = n : (getSequence (n `div` 2))
  | otherwise      = n : (getSequence (3 * n + 1))

findSequences :: Int -> Int -> [Int]
findSequences 1 1 = [1]
findSequences num len
  | num > 0 = findSequences (num - 1) len
