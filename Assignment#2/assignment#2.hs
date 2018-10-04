--
-- CPSC 449 Assignment 2 Starter Code
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

--
-- Define your algebraic types for Part 1 here
--
-- The quad tree consists a leaf that contains a (Int, Int, Int) tuple, value for
-- (red(0 - 255), green(0 - 255), blue(0 - 255)) and the recursive case is an
-- internal node that has 4 children and no data is stored in the internal nodes
data QuadTree = Leaf (Int, Int, Int)
                | Node QuadTree QuadTree QuadTree QuadTree deriving Show
-- The data type which consists of a integer and a quad tree
-- Int: width(and height) of the image (since it is square) and the quad tree will
--      contain the color data for the image
data DrawImage = Square Int QuadTree deriving Show

--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int
make32Int a b c d = ((((fromIntegral a) * 256) +
                       (fromIntegral b) * 256) +
                       (fromIntegral c) * 256) +
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block.  Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
-- Insert your code here for Parts 2, 3 and 4 here
--

--
-- Part 2:
--
-- The helper function created to check if that all of the pixels are the same color
isHomogenousHelper :: Eq a => [a] -> Bool
isHomogenousHelper []     = True
isHomogenousHelper (x:[]) = True
isHomogenousHelper (x:xs)
  | x == head xs = isHomogenousHelper xs
  | otherwise    = False

-- The function first checks of the first row of the tree has the same color, if
-- true, compare the rest of the rows with the first row to see if they all have
-- the same value
isHomogenous :: [[(Int, Int, Int)]] -> Bool
isHomogenous image = (isHomogenousHelper (head image)) && (isHomogenousHelper image)

-- Function takes the first half of the 2-D array
firstHalf :: [a] -> [a]
firstHalf image = take ((length image) `div` 2) image

-- Function takes the second half of the 2-D array
secondHalf :: [a] -> [a]
secondHalf image = drop ((length image) `div` 2) image

createTreeHelper :: [[(Int, Int, Int)]] -> QuadTree
createTreeHelper image
  | isHomogenous image = Leaf (head (image !! 0))
  | otherwise = Node upperLeft upperRight lowerLeft lowerRight
  where
    upperLeft  = createTreeHelper (map firstHalf (firstHalf image))
    upperRight = createTreeHelper (map secondHalf (firstHalf image))
    lowerLeft  = createTreeHelper (map firstHalf (secondHalf image))
    lowerRight = createTreeHelper (map secondHalf (secondHalf image))

-- createTree function that takes a list representation of an image and stores
-- it in a quad tree image, the function will return error(and quit) if the
-- provided image is not a square.
createTree :: [[(Int, Int, Int)]] -> DrawImage
createTree image
  | (length image) /= (length (image !! 0)) = error "The image provided is not a square."
  | otherwise                               = Square (length image) (createTreeHelper image)

--
-- Part 3:
-- The function treeMap that maps a function over all of the nodes in a quad tree,
-- returning the modified quad tree as its result.
--
treeMap :: (QuadTree -> QuadTree) -> QuadTree -> QuadTree
treeMap f (Leaf (a, b, c)) = f (Leaf (a, b, c))
treeMap f (Node d e g h) = f (Node one two three four)
  where
    one   = treeMap f d
    two   = treeMap f e
    three = treeMap f g
    four  = treeMap f h

-- The grayscale function converts an image to grey scale(by averaging the red,
-- green and blue values in each leaf node and the nusing that average value for
-- all three color components)
grayscaleHelper :: QuadTree -> QuadTree
grayscaleHelper (Node a b c d) = Node a b c d
grayscaleHelper (Leaf (e, f, g)) = Leaf (average, average, average)
  where
    average = (e + f + g) `div` 3

grayscale :: DrawImage -> DrawImage
grayscale (Square width quadTree) = Square width (treeMap grayscaleHelper quadTree)

-- The mirror function that mirrors the image(left and right are swapped)
mirrorHelper :: QuadTree -> QuadTree
mirrorHelper (Leaf (a, b, c)) = Leaf (a, b, c)
mirrorHelper (Node d e f g) = Node e d g f

mirror :: DrawImage -> DrawImage
mirror (Square width quadTree) = Square width (treeMap mirrorHelper quadTree)

--
-- Part 4:
-- The function toHTML generates the HTML SVG tags necessary to rendar a quad
-- tree representation of an image in a browser.
--
toHTMLhelper :: Int -> Int -> Int -> QuadTree -> String
toHTMLhelper x y width (Leaf (a, b ,c)) = "<rect x=" ++ (show x) ++
                                          " y=" ++ (show y) ++
                                          " width=" ++ (show width) ++
                                          " height=" ++ (show width) ++
                                          " fill=\"rgb(" ++
                                          (show a) ++ "," ++
                                          (show b) ++ "," ++
                                          (show c) ++ ")\" stroke=\"None\" />\n"
toHTMLhelper x y width (Node quadTree1 quadTree2 quadTree3 quadTree4) =
  toHTMLhelper x y (width `div` 2) quadTree1 ++
  toHTMLhelper (x + (width `div` 2)) y (width `div` 2) quadTree2 ++
  toHTMLhelper x (y + (width `div` 2)) (width `div` 2) quadTree3 ++
  toHTMLhelper (x + (width `div` 2)) (y + (width `div` 2)) (width `div` 2) quadTree4

toHTML :: DrawImage -> String
toHTML (Square width quadTree) = "<html><head></head><body>\n" ++
                                 "<svg width=\"" ++ (show width) ++
                                 "\" height=\"" ++ (show width) ++ "\">" ++
                                 (toHTMLhelper 0 0 width quadTree) ++
                                 "</svg>\n</html>"

--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  input <- BS.readFile "Mondrian.png"

  -- image is the list representation of the image stored in the .png file
  let image = decodeImage input

  -- Convert the list representation of the image into a tree representation
  let qtree_image = createTree image

  -- Gray scale the tree representation of the image
  let gs = grayscale qtree_image

  -- Mirror the tree representation of the image
  let mi = mirror qtree_image

  -- Write the original, mirrored and grayscale images to quadtree.html
  -- writeFile "quadtree.html" "" -- take this line out and use the lines below
                               -- instead once you have your functions written

  writeFile "quadtree.html" ((toHTML qtree_image) ++ "<br><br><br>" ++
                             (toHTML gs) ++ "<br><br><br>" ++
                             (toHTML mi) ++ "<br><br><br>")
