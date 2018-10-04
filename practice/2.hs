getPreyListsHelper :: [(String, [String])] -> (String, String) -> [(String, [String])]
getPreyListsHelper [] (x, y)= []
getPreyListsHelper [(a, b)] (x, y) = [(x, [y])]
getPreyListsHelper ((predator1, prey1):(predator2, prey2):rest) (x, y)
  | predator1 == predator2 = (predator1, [prey1, prey2]):getPreyListsHelper rest
  | otherwise              = getPreyListsHelper ((predator2, prey2):rest)


getPreyLists :: [(String, String)] -> [(String, [String])]
getPreyLists values = foldl getPreyListsHelper [] values



--
--
-- getPreyLists :: [(String, String)] -> [(String, [String])]
-- getPreyLists [] = []
-- getPreyLists [(a, b)] = [(a, [b])]
-- getPreyLists ((predator1, prey1):(predator2, prey2):rest)
--   | predator1 == predator2 = (predator1, [prey1, prey2]):getPreyLists rest
--   | otherwise              = getPreyLists ((predator2, prey2):rest)
