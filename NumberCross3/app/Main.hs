module Main where

import Data.SBV
import Data.List (transpose)
import Data.Maybe (fromJust)

main :: IO ()
main = numberCross jsPuzzle

type Puzzle = (Int, [SInt8] -> Board)
type Board = [Row]
type Row = [SInt8]

check :: [SInt8] -> SBool
check grp = allDifferent nonZeros &&& oneNeg
    where
        -- This looks a bit silly, but we can't do a filter and we don't want the 0s to look
        -- identical to allDifferent, so we have to be a bit more clever!
        nonZeros  = map (\(v,i) -> ite (v .== 0) (v,i) (abs v,0)) $ zip grp ([1..] :: [SInt8])
        smallest  = symMin grp
        oneNeg    = bAnd $ (smallest .< 0) : map (.>= 0) allButMin
        -- This is another thing which would be a lot more obvious as a filter, but...
        allButMin = map (\n -> ite (n .== smallest) 0 n) grp

symMin :: [SInt8] -> SInt8
symMin = foldl smin 10

boardValid :: Board -> SBool
boardValid rows = bAnd $ literal sizesOk
                       : rowsValid rows
                       : rowTakeChecks rows
                       : colsValid columns
                       : colTakeChecks columns
                       : map check (rows ++ columns)
    where
        sizesOk = length rows == 10 && all (\r -> length r == 10) rows
        columns = transpose rows

rowsValid :: [Row] -> SBool
rowsValid rs = bAnd $ zipWith (.==) [24+14,10+25,0+19,41,23+6,-8+14,39,15+17,17+22,8+10] (map sum rs)

rowTakeChecks :: [Row] -> SBool
rowTakeChecks rs = bAnd $ zipWith takeCheck rs
                            [(3,24),(3,10),(3,0),(9,41),(4,23),(5,-8),(10,39),(6,15),(6,17),(4,8)]

-- Given a row/column, takes number off the top and checks to see if the sum is correct
-- Just the row/column totals were underconstrained, so this allows for the full crossword-style
-- checks, at least in the given puzzle where there are at most 2 runs per row/column
takeCheck :: [SInt8] -> (Int, SInt8) -> SBool
takeCheck r (number, expectedSum) = sum (take number r) .== expectedSum

colsValid :: [Row] -> SBool
colsValid cs = bAnd $ zipWith (.==) [17+1,6+21,35+4,30,8+4,19+7,32,13+22,30+9,8+30] (map sum cs)

colTakeChecks :: [Row] -> SBool
colTakeChecks rs = bAnd $ zipWith takeCheck rs
                            [(5,17),(5,6),(5,35),(10,30),(4,8),(5,19),(7,32),(4,13),(4,30),(2,8)]

numberValid :: SInt8 -> SBool
numberValid n = n ./= 0 &&& n `inRange` (-9,9)

numberCross :: Puzzle -> IO ()
numberCross p@(i, f) = do
    putStrLn "Solving..."
    model <- getModel `fmap` sat ((\v -> (boardValid . f $ v) &&& bAnd (map numberValid v))  `fmap` mkExistVars i)
    case model of
        Right sln -> dispSolution p sln
        Left m -> putStrLn $ "Unsolvable puzzle: " ++ m

dispSolution :: Puzzle -> (Bool, [Int8]) -> IO ()
dispSolution (i, f) (_, fs)
  | lmod /= i = error $ "Impossible! Backend solver returned " ++ show lmod ++ " values, was expecting: " ++ show i
  | otherwise      =
                do putStrLn "Final board:"
                   mapM_ printRow final
                   putStrLn $ "Valid Check: " ++ show (boardValid final)
                   putStrLn "Done."
  where lmod = length fs
        final = f (map literal fs)
        printRow r = putStr "   " >> mapM_ (\x -> putStr (show (fromJust (unliteral x)) ++ " ")) r >> putStrLn ""

jsPuzzle :: Puzzle
jsPuzzle = (78, f)
    where
        f [a0, a1, a2,             a6, a7, a8, a9,
           b0, b1, b2,     b4, b5, b6, b7, b8, b9,
           c0, c1, c2,     c4, c5, c6, c7, c8,
           d0, d1, d2, d3, d4, d5, d6, d7, d8,
           e0, e1, e2, e3,     e5, e6,
                       f3, f4,     f6, f7, f8, f9,
               g1, g2, g3, g4, g5, g6, g7, g8, g9,
               h1, h2, h3, h4, h5,     h7, h8, h9,
           i0, i1, i2, i3, i4, i5,     i7, i8, i9,
           j0, j1, j2, j3,             j7, j8, j9] =

           [[a0, a1, a2,  0,  0,  0, a6, a7, a8, a9],
            [b0, b1, b2,  0, b4, b5, b6, b7, b8, b9],
            [c0, c1, c2,  0, c4, c5, c6, c7, c8,  0],
            [d0, d1, d2, d3, d4, d5, d6, d7, d8,  0],
            [e0, e1, e2, e3,  0, e5, e6,  0,  0,  0],
            [ 0,  0,  0, f3, f4,  0, f6, f7, f8, f9],
            [ 0, g1, g2, g3, g4, g5, g6, g7, g8, g9],
            [ 0, h1, h2, h3, h4, h5,  0, h7, h8, h9],
            [i0, i1, i2, i3, i4, i5,  0, i7, i8, i9],
            [j0, j1, j2, j3,  0,  0,  0, j7, j8, j9]]
