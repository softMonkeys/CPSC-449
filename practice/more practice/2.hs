wci :: Float -> Float -> Float
wci temp speed
  | temp > 10.0 || speed < 4.8 = temp
  | otherwise                 = 13.12 + 0.6215 * temp - 11.37 * speed ** 0.16 + 0.3965 * temp * speed ** 0.16
windChills :: [(Float, Float)] -> [Float]
windChills [] = []
windChills ((temp, speed):ps) = (wci temp speed) : windChills ps
