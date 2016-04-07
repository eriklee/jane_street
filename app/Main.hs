{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import qualified Data.Vector as V
import Data.Maybe

main :: IO ()
main = mapM_ output solutions

type GridPosition = (Int, Int) -- (x,y), top left is (0,0)

data Grid a = Grid
            { width :: Int
            , height :: Int
            , cells :: V.Vector a
            } deriving (Show)

printGrid Grid{..} = go (V.toList cells)
  where
    go [] = putStrLn ""
    go cs = let (row, rems) = splitAt width cs in print row >> go rems

nextMoves :: GridPosition -> [GridPosition]
nextMoves (x,y) =
    [(x `xop` xd, y `yop` yd) | xd  <- [1,2],
                                yd  <- [1,2],
                                xop <- [(+),(-)],
                                yop <- [(+),(-)],
                                xd /= yd]

positionInGrid :: Grid a -> GridPosition -> Bool
positionInGrid Grid{..} (x,y) =
      x >= 0 && y >= 0 && x < width && y < height

notVisited :: (Eq a, Num a) => Grid a -> GridPosition -> Bool
notVisited g p = (== 0) $ getGridValue g p

getGridValue :: Grid a -> GridPosition -> a
getGridValue Grid{..} (x,y) = cells `V.unsafeIndex` (y * width + x)

validMoves :: (Eq a, Num a) => Grid a -> GridPosition -> [GridPosition]
validMoves grid = filter (notVisited grid) . filter (positionInGrid grid) . nextMoves

foldGridRow :: (b -> a -> b) -> b -> Grid a -> Int -> b
foldGridRow f init grid n = V.foldl' f init (getRowV grid n)

getRowV :: Grid a -> Int -> V.Vector a
getRowV Grid{..} n = V.unsafeSlice (n * height) width cells
--[ cells V.! x | x <- [n * height .. n * height + width - 1] ]

foldGridCol :: (b -> a -> b) -> b -> Grid a -> Int -> b
foldGridCol f init grid n = foldl f init (getCol grid n)

getCol :: Grid a -> Int -> [a]
getCol Grid{..} n = [ cells `V.unsafeIndex` (n + (x * width)) | x <- [0..height - 1] ]

initialGrid = Grid 8 8 $ V.fromList
    [ 0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 , 11 ,  0 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 ,  0 ,  0 , 14 ,  0 ,  0 ,
      0 ,  0 ,  8 ,  0 ,  0 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 ,  0 , 15 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
      0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ]

enumerateMoves :: (Grid Int -> Bool) -> Grid Int -> GridPosition -> (Int -> Maybe Int) -> Maybe Int -> [Grid Int]
--               starting grid   -> starting pos ->  next label   -> initial label -> grids
enumerateMoves filterF grid pos labelIter Nothing  = [grid]
enumerateMoves filterF grid pos labelIter (Just x) = concatMap mapFun nextGrids
    where
        mapFun (g, pos) = enumerateMoves filterF g pos labelIter nextLabel
        nextLabel = labelIter x
        nextGrids = filter (filterF . fst) $ map (\p -> (labelGridCell grid p x, p)) (validMoves grid pos)

labelGridCell :: Grid a -> GridPosition -> a -> Grid a
labelGridCell g@Grid{..} pos x = g { cells = cells `V.unsafeUpd` [(index, x)] }
    where index = gridIndex g pos

gridIndex :: Grid a -> GridPosition -> Int
gridIndex Grid{..} (x,y) = x + (width * y)

vecIndexToPos :: Grid a -> Int -> GridPosition
vecIndexToPos Grid{..} n = (n `rem` width, n `div` height)

lowerGrids :: [Grid Int]
lowerGrids = enumerateMoves (const True) initialGrid (2,4) (\x -> if x == 1 then Nothing else Just (x - 1)) (Just 7)

-- Takes a grid and tries to find knight moves from the 8 square that will be compatible with the 11
eightTo11 :: Grid Int -> [Grid Int]
eightTo11 g = filter compatibleWith11 potentials
  where
    potentials = enumerateMoves (const True)g (2,4) (\x -> if x == 10 then Nothing else Just (x + 1)) (Just 9)
    get10Pos g = vecIndexToPos g . fromJust $ 10 `V.elemIndex` cells g
    compatibleWith11 g = eleven `elem` nextMoves (get10Pos g)
    eleven = (3,2)

elevenTo14 :: Grid Int -> [Grid Int]
elevenTo14 g = filter compatibleWith14 potentials
  where
    potentials = enumerateMoves (const True) g (3,2) (\x -> if x == 13 then Nothing else Just (x + 1)) (Just 12)
    get13Pos g = vecIndexToPos g . fromJust $ 13 `V.elemIndex` cells g
    compatibleWith14 g = fourteen `elem` nextMoves (get13Pos g)
    fourteen = (5,3)

sixteenTo28 :: Grid Int -> [Grid Int]
sixteenTo28 g = potentials
  where
    potentials = enumerateMoves filterBadSums g (4,5) (\x -> if x == 28 then Nothing else Just (x + 1)) (Just 16)

rowSums, colSums :: [Int]
rowSums = [10, 34, 108, 67, 63, 84, 24, 16]
colSums = [7, 14, 72, 66, 102, 90, 42, 13]

getGridRowSums g = map (V.foldl1' (+) . getRowV g) [0..7]
getGridColSums g = map (sum . getCol g) [0..7]

filterBadSums g = and (zipWith (<=) (getGridRowSums g) rowSums) &&
                  and (zipWith (<=) (getGridColSums g) colSums)

-- applies to lower and requires we have at least the first column finished
lowFilter g = head (getGridColSums g) == 7

elevenFilter g = head (getGridRowSums g) == 10 && take 2 (getGridColSums g) == [7, 14]

solutions = filter lowFilter lowerGrids >>= filter elevenFilter . eightTo11 >>= filter elevenFilter . elevenTo14 >>= sixteenTo28

maxProduct g = maximum $ map (V.foldl' nonZeroProd 1 . getRowV g) [0..7] ++
                         map (foldl nonZeroProd 1 . getCol g) [0..7]

nonZeroProd acc 0 = acc
nonZeroProd acc n = acc * n
-- The final grid obtained from the above. Takes a minute or two to find. (max = 19675656)
finalGrid = Grid 8 8 $ V.fromList . concat $
   [[ 0 , 0 , 10, 0 ,  0 , 0 , 0 , 0]
   ,[ 0 , 0 , 0 , 22,  0 , 12, 0 , 0]
   ,[ 0 , 9 , 28, 11,  26, 21, 0 , 13]
   ,[ 0 , 0 , 1 , 4 ,  23, 14, 25, 0]
   ,[ 0 , 5 , 8 , 27,  20, 3 , 0 , 0]
   ,[ 7  ,0 , 19, 2 ,  15, 24, 17, 0]
   ,[ 0 , 0 , 6 , 0 ,  18, 0 , 0 , 0]
   ,[ 0 , 0 , 0 , 0 ,  0 , 16, 0 , 0]]

output g = do
    putStrLn "Solution found:"
    print $ "Max product: " ++ show (maxProduct g)
    printGrid g
