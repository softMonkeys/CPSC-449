--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
--
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Generate the tag for a rectangle with random color.  Replace the
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
-- determine which color to fill
  -- if < 0.0833 then fill the region with red
  | r < 0.0833 = (rs, dup ++ " fill=\"rgb(" ++ (show 255) ++ "," ++
                                               (show 0) ++ "," ++
                                               (show 0) ++ ")\" />\n")
  -- Else if r < 0.1667 then fill the region with skyblue
  | r < 0.1667 = (rs, dup ++ " fill=\"rgb(" ++ (show 135) ++ "," ++
                                               (show 206) ++ "," ++
                                               (show 250) ++ ")\" />\n")
  -- Else if r < 0.25 then fill the region with yellow
  | r < 0.25   = (rs, dup ++ " fill=\"rgb(" ++ (show 255) ++ "," ++
                                               (show 255) ++ "," ++
                                               (show 0) ++ ")\" />\n")
  -- Else fill the region with white
  | otherwise  = (rs, dup ++ " fill=\"rgb(" ++ (show 255) ++ "," ++
                                               (show 255) ++ "," ++
                                               (show 255) ++ ")\" />\n")
  where
    -- the code which duplicates in each guard
    dup = "<rect x=" ++ (show x) ++ " y=" ++ (show y) ++
                 " width=" ++ (show w) ++
                 " height=" ++ (show h) ++
                 " stroke=\"rgb(0, 0, 0)\"" ++
                 " stroke-width =" ++ (show (randomInt 6 7 r))


--
-- This "dicision" calls function "mondarian" recusively to generate pseudo-random
-- "art" in a Mondrian style
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   one:two:three:four:five:six:seven:eight:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
decision :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
decision x y w h (one:two:three:four:five:six:seven:eight:rs) -- divide the list of random floating numbers into variables
  -- If the region is wider than half the initial canvas size and the region is taller than half the initial canvas height:
  -- or, Else if the region is big enough to split both horizontally and vertically, and both a horizontal and
  -- vertical split are randomly selected:
  | w > (width `div` 2) && h > (height `div` 2) ||
    w > random_int_w_1 && h > random_int_h_1     =
                                                 -- Use recursion to split the region into 4 smaller regions (a vertical split
                                                 -- and a horizontal split) with both split locations chosen randomly.
                                                 (p4f, snd(mondrian x y w h (one:two:three:four:five:six:seven:eight:rs)) ++
                                                 p1s ++ p2s ++ p3s ++ p4s)
  -- Else if the region is wider than half the initial canvas size:
  -- or, Else if the region is wide enough to split horizontally, and a horizontal split is randomly selected:
  | w > (width `div` 2) ||  w > random_int_w_2   =
                                                 -- Use recursion to split the region into 2 smaller regions using a vertical
                                                 -- line with the split location chosen randomly.
                                                 (p6f, snd(mondrian x y w h (one:two:three:four:five:six:seven:eight:rs)) ++
                                                 p5s ++ p6s)
  -- Else if the region is taller than half the initial canvas size:
  -- or, Else if the region is tall enough to split vertically, a vertical split is randomly selected:
  | h > (height `div` 2) || h > random_int_h_3   =
                                                 -- Use recursion to split the region into 2 smaller regions using a horizontal
                                                 -- line with the split location chosen randomly.
                                                 (p8f, snd(mondrian x y w h (one:two:three:four:five:six:seven:eight:rs)) ++
                                                 p7s ++ p8s)
  -- Else: Fill the current region (randomly, either white or colored, and if colored, with a random determination of red, blue or yellow).
  | otherwise                                    = mondrian x y w h (one:two:three:four:five:six:seven:eight:rs)
  where
    new_w_1 = randomInt (round(fromIntegral w * 0.33)) (round(fromIntegral w * 0.67)) one
    new_h_1 = randomInt (round(fromIntegral h * 0.33)) (round(fromIntegral h * 0.67)) two
    new_w_2 = randomInt (round(fromIntegral w * 0.33)) (round(fromIntegral w * 0.67)) three
    new_h_3 = randomInt (round(fromIntegral h * 0.33)) (round(fromIntegral h * 0.67)) four
    random_int_w_1 = randomInt 120 (round(fromIntegral w * 1.5)) five
    random_int_h_1 = randomInt 120 (round(fromIntegral h * 1.5)) six
    random_int_w_2 = randomInt 120 (round(fromIntegral w * 1.5)) seven
    random_int_h_3 = randomInt 120 (round(fromIntegral h * 1.5)) eight
    (p1f, p1s) = decision x y new_w_1 new_h_1 rs
    (p2f, p2s) = decision (x + new_w_1) y (w - new_w_1) new_h_1 p1f
    (p3f, p3s) = decision x (y + new_h_1) new_w_1 (h - new_h_1) p2f
    (p4f, p4s) = decision (x + new_w_1) (y + new_h_1) (w - new_w_1) (h - new_h_1) p3f
    (p5f, p5s) = decision x y new_w_2 h rs
    (p6f, p6s) = decision (x + new_w_2) y (w - new_w_2) h p5f
    (p7f, p7s) = decision x y w new_h_3 rs
    (p8f, p8s) = decision x (y + new_h_3) w (h - new_h_3) p7f


--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  -- let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++
               "\" height=\"" ++ (show height) ++ "\">"
      -- mondrian has been changed to decision in order to make the modified version working
      image = snd (decision 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian_1.html" (prefix ++ image ++ suffix)
