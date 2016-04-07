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

notVisited :: Grid (Maybe a) -> GridPosition -> Bool
notVisited g p = isNothing $ getGridValue g p

getGridValue :: Grid a -> GridPosition -> a
getGridValue Grid{..} (x,y) = cells V.! (y * width + x)

validMoves :: Grid (Maybe a) -> GridPosition -> [GridPosition]
validMoves grid = filter (notVisited grid) . filter (positionInGrid grid) . nextMoves

foldGridRow :: (b -> a -> b) -> b -> Grid a -> Int -> b
foldGridRow f init grid n = foldl f init (getRow grid n)

getRow :: Grid a -> Int -> [a]
getRow Grid{..} n = [ cells V.! x | x <- [n * height .. n * height + width - 1] ]

foldGridCol :: (b -> a -> b) -> b -> Grid a -> Int -> b
foldGridCol f init grid n = foldl f init (getCol grid n)

getCol :: Grid a -> Int -> [a]
getCol Grid{..} n = [ cells V.! (n + (x * width)) | x <- [0..height - 1] ]

initialGrid = Grid 8 8 $ V.fromList
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Just 11, Nothing, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Nothing, Nothing, Just 14, Nothing, Nothing,
     Nothing, Nothing, Just  8, Nothing, Nothing, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Nothing, Just 15, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
     Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

enumerateMoves :: Grid (Maybe Int) -> GridPosition -> (Int -> Maybe Int) -> Maybe Int -> [Grid (Maybe Int)]
--               starting grid   -> starting pos ->  next label   -> initial label -> grids
enumerateMoves grid pos labelIter Nothing  = [grid]
enumerateMoves grid pos labelIter (Just x) = concatMap mapFun nextGrids
    where
        mapFun (g, pos) = enumerateMoves g pos labelIter nextLabel
        nextLabel = labelIter x
        nextGrids = map (\p -> (labelGridCell grid p (Just x), p)) (validMoves grid pos)

labelGridCell :: Grid a -> GridPosition -> a -> Grid a
labelGridCell g@Grid{..} pos x = g { cells = cells V.// [(index, x)] }
    where index = gridIndex g pos

gridIndex :: Grid a -> GridPosition -> Int
gridIndex Grid{..} (x,y) = x + (width * y)

vecIndexToPos :: Grid a -> Int -> GridPosition
vecIndexToPos Grid{..} n = (n `rem` width, n `div` height)

lowerGrids :: [Grid (Maybe Int)]
lowerGrids = enumerateMoves initialGrid (2,4) (\x -> if x == 1 then Nothing else Just (x - 1)) (Just 7)

-- Takes a grid and tries to find knight moves from the 8 square that will be compatible with the 11
eightTo11 :: Grid (Maybe Int) -> [Grid (Maybe Int)]
eightTo11 g = filter compatibleWith11 potentials
  where
    potentials = enumerateMoves g (2,4) (\x -> if x == 10 then Nothing else Just (x + 1)) (Just 9)
    get10Pos g = vecIndexToPos g . fromJust $ Just 10 `V.elemIndex` cells g
    compatibleWith11 g = eleven `elem` nextMoves (get10Pos g)
    eleven = (3,2)

elevenTo14 :: Grid (Maybe Int) -> [Grid (Maybe Int)]
elevenTo14 g = filter compatibleWith14 potentials
  where
    potentials = enumerateMoves g (3,2) (\x -> if x == 13 then Nothing else Just (x + 1)) (Just 12)
    get13Pos g = vecIndexToPos g . fromJust $ Just 13 `V.elemIndex` cells g
    compatibleWith14 g = fourteen `elem` nextMoves (get13Pos g)
    fourteen = (5,3)

sixteenTo20 :: Grid (Maybe Int) -> [Grid (Maybe Int)]
sixteenTo20 g = potentials
  where
    potentials = enumerateMoves g (4,5) (\x -> if x == 20 then Nothing else Just (x + 1)) (Just 16)

twentyOneTo24 :: Grid (Maybe Int) -> [Grid (Maybe Int)]
twentyOneTo24 g = potentials
  where
    starting = getGridCell g 20
    potentials = enumerateMoves g starting (\x -> if x == 24 then Nothing else Just (x + 1)) (Just 21)

twentyFiveTo28 :: Grid (Maybe Int) -> [Grid (Maybe Int)]
twentyFiveTo28 g = potentials
  where
    starting = getGridCell g 24
    potentials = enumerateMoves g starting (\x -> if x == 28 then Nothing else Just (x + 1)) (Just 25)

getGridCell :: Grid (Maybe Int) -> Int -> (Int, Int)
getGridCell Grid{..} val = idxToPos idx width
  where
    idx = fromJust $ V.elemIndex (Just val) cells

idxToPos n w = n `quotRem` w

rowSums, colSums :: [Int]
rowSums = [10, 34, 108, 67, 63, 84, 24, 16]
colSums = [7, 14, 72, 66, 102, 90, 42, 13]

getGridRowSums g = map (sum . catMaybes . getRow g) [0..7]
getGridColSums g = map (sum . catMaybes . getCol g) [0..7]

filterBadSums g = and (zipWith (<=) (getGridRowSums g) rowSums) &&
                  and (zipWith (<=) (getGridColSums g) colSums)

-- applies to lower and requires we have at least the first column finished
lowFilter g = head (getGridColSums g) == 7

elevenFilter g = head (getGridRowSums g) == 10 && take 2 (getGridColSums g) == [7, 14]

solutions = filter lowFilter lowerGrids >>= filter elevenFilter . eightTo11 >>= filter elevenFilter . elevenTo14 >>= filter filterBadSums . sixteenTo20 >>= filter filterBadSums . twentyOneTo24 >>= filter filterBadSums . twentyFiveTo28

maxProduct g = maximum $ map (product . catMaybes . getRow g) [0..7] ++
                         map (product . catMaybes . getCol g) [0..7]

-- The final grid obtained from the above. Takes a minute or two to find. (max = 19675656)
finalGrid = Grid 8 8 $ V.fromList . concat $
   [[Nothing ,Nothing ,Just 10, Nothing, Nothing, Nothing, Nothing, Nothing]
   ,[Nothing ,Nothing ,Nothing, Just 22, Nothing, Just 12, Nothing, Nothing]
   ,[Nothing , Just 9 ,Just 28, Just 11, Just 26, Just 21, Nothing, Just 13]
   ,[Nothing ,Nothing , Just 1,  Just 4, Just 23, Just 14, Just 25, Nothing]
   ,[Nothing , Just 5 , Just 8, Just 27, Just 20, Just 3 , Nothing, Nothing]
   ,[Just 7  ,Nothing ,Just 19,  Just 2, Just 15, Just 24, Just 17, Nothing]
   ,[Nothing ,Nothing , Just 6, Nothing, Just 18, Nothing, Nothing, Nothing]
   ,[Nothing ,Nothing ,Nothing, Nothing, Nothing, Just 16, Nothing, Nothing]]

output g = do
    putStrLn "Solution found:"
    print $ "Max product: " ++ show (maxProduct g)
    printGrid g
