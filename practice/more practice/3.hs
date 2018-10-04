pathLengthHelper :: Float -> Float -> Float
pathLengthHelper ele_2 ele_1 = (ele_2 - ele_1) ** 2

plus :: Float -> Float -> Float
plus a b = a + b

pathLength :: [(Float, Float)] -> Float
pathLength []          = 0.0
pathLength [(x, y)]    = 0.0
pathLength ((x, y):ps) = foldl(pathLengthHelper x y) plus []
