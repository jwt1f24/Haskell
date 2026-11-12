import Data.Char (toUpper, ord, chr, isDigit, isUpper)
import Data.List (sortBy)

-- ASSESSMENT A1
filterChar :: String -> String
filterChar = filter (`elem` ['A'..'Z']) . map toUpper

shiftChar :: Int -> Char -> Char
shiftChar n c = chr (ord 'A' + ((ord c - ord 'A' + n) `mod` 26))

vigenere :: String -> (String -> String, String -> String)
vigenere key = (encrypt, decrypt)
 where
  procKey = if null (filterChar key) then "A" else filterChar key
  zipKey m = zip (filterChar m) (cycle procKey)
  encrypt m = map (\(p, k) -> shiftChar (ord k - ord 'A') p) (zipKey m)
  decrypt m = map (\(c, k) -> shiftChar (26 - (ord k - ord 'A')) c) (zipKey m)


-- ASSESSMENT A2
frequency :: Int -> String -> [[(Char, Int)]]
frequency n ct = map (sortBy comp . counts) (groups ct)
 where
  groups s = map (\k -> everyNth (drop k s)) [0..(n-1)]
  everyNth [] = []
  everyNth (x:xs) = x : everyNth (drop (n-1) xs)
  counts s = [(c, length (filter (== c) s)) | c <- ['A'..'Z'], c `elem` s]
  comp (c1, n1) (c2, n2) = compare n2 n1 <> compare c1 c2


-- ASSESSMENT A3
-- lines with 2 ends
type Path = ((Integer, Integer), (Integer, Integer))

maze1, maze2, maze3 :: [Path]
maze1 = []
maze2 = [ ((0,0), (0,3)), ((0,3), (3,3)), ((3,0), (3,3)) ]
maze3 = [ ((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), 
        ((4,2),(5,2)), ((2,1),(2,5)), ((1,5),(4,5)) ]

renderMaze :: [Path] -> [String]
renderMaze [] = []
renderMaze m = [[cell x y | x <- [0..maxX]] | y <- [0..maxY]]
 where
  -- define grid size
  maxX = maximum [x | ((x1, _), (x2, _)) <- m, x <- [x1, x2]]
  maxY = maximum [y | ((_, y1), (_, y2)) <- m, y <- [y1, y2]]
  
  -- output '#' if (x, y) exists in path, else space
  cell x y = if any (covers x y) m then '#' else ' '

  -- check if (x, y) exists in the grid path
  covers x y ((x1, y1), (x2, y2)) =
   x >= min x1 x2 && x <= max x1 x2 &&
   y >= min y1 y2 && y <= max y1 y2


-- ASSESSMENT A4
mazeA, mazeB, mazeC, mazeD, mazeE, mazeF :: [Path]
mazeA = []
mazeB = [ ((0,0), (0,3)) ]
mazeC = [ ((0,0), (0,3)), ((0,2), (2,2)) ]
mazeD = [ ((0,0), (0,3)), ((3,5), (5,5)) ]
mazeE = [ ((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), 
        ((4,2),(5,2)), ((2,1),(2,5)), ((1,5),(4,5)) ]
mazeF = [ ((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), 
        ((4,2),(5,2)), ((2,1),(2,5)), ((3,5),(5,5)) ]

-- check if 2 lines cross each other at any point
intersects :: Path -> Path -> Bool
intersects ((ax1, ay1), (ax2, ay2)) ((bx1, by1), (bx2, by2)) =
 let xOverlap = max (min ax1 ax2) (min bx1 bx2) <= min (max ax1 ax2) (max bx1 bx2)
     yOverlap = max (min ay1 ay2) (min by1 by2) <= min (max ay1 ay2) (max by1 by2)
 in xOverlap && yOverlap

-- check if maze is linked from any to other lines by hopping btwn connected lines
connected :: [Path] -> Bool
connected [] = True
connected (p:ps) = go [p] ps
 where
  go reached rest
   | null new = null rest
   | otherwise = go (reached ++ new) others
   where
    new = filter (\q -> any (intersects q) reached) rest
    others = filter (\q -> not (any (intersects q) reached)) rest


-- ASSESSMENT A5
hextonum :: Char -> Int
hextonum c
 | isDigit c = ord c - ord '0'
 | isUpper c = ord c - ord 'A' + 10
 | otherwise = ord c - ord 'a' + 10

numtohex :: Int -> Char
numtohex n
 | n < 10 = chr (ord '0' + n)
 | otherwise = chr (ord 'A' + (n - 10))












