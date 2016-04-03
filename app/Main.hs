{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import qualified Data.Vector as V
import Data.Maybe

main :: IO ()
main = mapM_ printInfo runReal

printInfo :: State -> IO ()
printInfo State{..} = do
    let value = product $ map snd $ fixPathConstraints die path
    putStrLn $ "Total Value: " ++ show value
    putStrLn $ "Total Length: " ++ show (length path)
    print (reverse path)

fixPathConstraints die = map fixElem
  where
    fixElem (p, GVValue val) = (p, val)
    fixElem (p, GVConstraint face) = (p, fromMaybe 9 (getDieFaceValue die face))

runDFS :: GridPosition -> State -> [State]
--        end position -> InitialState -> FinalStates
runDFS end state = go [state]
  where
    go [] = []
    go (x:xs) = if isFinished end x
                  then x : go (doMoves x ++ xs)
                  else go (doMoves x ++ xs)

isFinished end State{..} = getPosFromPath path == end

--- Data
runReal = runDFS (11,11) realState
runAnswer = runDFS (11,11) $ State realGrid answerDie [((0,0), GVValue 1)]

testDie = Die DfA DfC [(DfA, 1), (DfB, 2), (DfC, 3), (DfD, 4)]
testGrid = Grid 2 2 (V.fromList [Just 1,Just 2,Nothing,Just 3])
testState = State testGrid testDie [((0,0), GVValue 1)]

bigTestGrid = Grid 5 5 (V.fromList (Just <$> [3,4,1,7,5, 1,2,4,3,5, 2,4,3,6,2, 9,5,7,2,3, 5,8,3,4,1]))
bigTestDie = Die DfA DfC [(DfA, 3),(DfB, 4), (DfC, 4), (DfD, 8), (DfE, 5), (DfF, 1)]
bigTestState = State bigTestGrid bigTestDie [((0,0), GVValue 3)]

bigTestDieEmpties = Die DfA DfC [(DfA, 3),(DfB, 4), (DfC, 4), (DfE, 5), (DfF, 1)]
bigTestGridWEmpties = Grid 5 5 (V.fromList
  [Just 3,Just 4, Just 1, Just 7,Just 5,
   Just 1,Just 2, Just 4, Just 3,Just 5,
   Just 2,Just 4,Nothing, Just 6,Just 2,
   Just 9,Nothing,Just 7, Just 2,Just 3,
   Just 5,Just 8, Just 3, Just 4,Just 1])
realGrid = Grid 12 12 (V.fromList
  [ Just 1, Just 5, Just 4, Just 4, Just 6, Just 1, Just 1, Just 4, Just 1, Just 3, Just 7, Just 5,
    Just 3,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 1,
    Just 4,Nothing, Just 6, Just 4, Just 1, Just 8, Just 1, Just 4, Just 2, Just 1,Nothing, Just 3,
    Just 7,Nothing, Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 1,Nothing, Just 2,
    Just 1,Nothing, Just 1,Nothing, Just 6, Just 1, Just 6, Just 2,Nothing, Just 2,Nothing, Just 1,
    Just 8,Nothing, Just 4,Nothing, Just 1,Nothing,Nothing, Just 8,Nothing, Just 3,Nothing, Just 5,
    Just 4,Nothing, Just 2,Nothing, Just 5,Nothing,Nothing, Just 3,Nothing, Just 5,Nothing, Just 2,
    Just 8,Nothing, Just 5,Nothing, Just 1, Just 1, Just 2, Just 3,Nothing, Just 4,Nothing, Just 6,
    Just 6,Nothing, Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 3,Nothing, Just 6,
    Just 3,Nothing, Just 6, Just 3, Just 6, Just 5, Just 4, Just 3, Just 4, Just 5,Nothing, Just 1,
    Just 6,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 3,
    Just 2, Just 1, Just 6, Just 6, Just 4, Just 5, Just 2, Just 1, Just 1, Just 1, Just 7, Just 1])

realDie = Die DfA DfC [(DfA, 1)]
answerDie = Die DfA DfC [(DfA, 1), (DfB, 7), (DfC, 3), (DfD, 8), (DfE, 6), (DfF, 6)]
realState = State realGrid realDie [((0,0), GVValue 1)]

--- Die Definitions
type Value = Integer -- (1-9), but will be multiplied together at the end
data Direction = North | South | East | West deriving (Show, Eq)

data DieFace = DfA | DfB | DfC | DfD | DfE | DfF deriving (Show, Eq)
{--     -----
 -      | C |
 -  -------------
 -  | B | A | D |
 -  -------------
 -      | E |
 -      -----
 -      | F |
 -      -----
--}
data Die = Die
            { topFace :: DieFace -- If this is A in the die above
            , northFace :: DieFace  -- Then this is C (gives the orientation of the die)
            , faceValues :: [(DieFace, Value)]
            } deriving (Show)

addFaceValueToDie :: Die -> Maybe Value -> Die
addFaceValueToDie d@Die{..} Nothing  = d
addFaceValueToDie d@Die{..} (Just x) = d { faceValues = faceValues' }
  where
    faceValues' = case getDieValue d of
                    Nothing -> (topFace, x) : faceValues
                    Just _  -> faceValues

getDieValue :: Die -> Maybe Value
getDieValue d@Die{..} = getDieFaceValue d topFace

getDieFaceValue :: Die -> DieFace -> Maybe Value
getDieFaceValue Die{..} face =
    case filter (\v -> face == fst v) faceValues of
        [] -> Nothing
        [(top, val)] -> Just val

roll :: Die -> Direction -> Die
roll die dir = let (top, north) = roll' dir (topFace die) (northFace die) in
                die { topFace = top, northFace = north }

roll' :: Direction -> DieFace -> DieFace -> (DieFace, DieFace)
--       direction -> top face -> north face -> (top', north')
roll' North top north = (opp north, top)
roll' West  top north = let rtop = dieRight' top north in (opp rtop, north)
roll' South top north = (north, opp top)
roll' East  top north = (dieRight' top north, north)

dieRight' DfA DfB = DfE
dieRight' DfA DfC = DfB
dieRight' DfA DfD = DfC
dieRight' DfA DfE = DfD

dieRight' DfB DfA = DfC
dieRight' DfB DfC = DfF
dieRight' DfB DfE = DfA
dieRight' DfB DfF = DfE

dieRight' DfC DfA = DfD
dieRight' DfC DfB = DfA
dieRight' DfC DfD = DfF
dieRight' DfC DfF = DfB

dieRight' DfD DfA = DfE
dieRight' DfD DfC = DfA
dieRight' DfD DfE = DfF
dieRight' DfD DfF = DfC

dieRight' DfE DfA = DfB
dieRight' DfE DfB = DfF
dieRight' DfE DfD = DfA
dieRight' DfE DfF = DfD

dieRight' DfF DfB = DfC
dieRight' DfF DfC = DfD
dieRight' DfF DfD = DfE
dieRight' DfF DfE = DfB

opp :: DieFace -> DieFace
opp DfA = DfF
opp DfB = DfD
opp DfC = DfE
opp DfD = DfB
opp DfE = DfC
opp DfF = DfA

type GridPosition = (Int, Int) -- (x,y), top left is (0,0)
data GridValue = GVValue Value | GVConstraint DieFace
    deriving (Show)

data Grid = Grid
            { width :: Int
            , height :: Int
            , cells :: V.Vector (Maybe Value)
            } deriving (Show)

printGrid Grid{..} = go (V.toList cells)
  where
    go [] = putStrLn ""
    go cs = let (row, rems) = splitAt width cs in print row >> go rems

type Path = [(GridPosition, GridValue)]

getPosFromPath = fst . head

data State = State
            { grid :: Grid
            , die :: Die
            , path :: Path
            } deriving (Show)

doMoves :: State -> [State]
doMoves s@State{..} =
  let
    pos = getPosFromPath path
    -- check that grid doesn't block move
    dirs = gridDirections grid pos
    -- check that path doesn't block move (i.e repeating a space)
    dirs' = filter (pathAllows path) dirs
    -- check that the die allows a move
    dirs'' = filter (dieAllows grid pos die) dirs' in
  -- actually move/rotate die, add to path
  map (moveDie s) dirs''

moveDie :: State -> Direction -> State
moveDie s@State{..} dir = s{die=die'', path=path'}
  where
    pos = getPosFromPath path
    pos' = changePos pos dir
    die' = roll die dir
    gv = getGridCell grid pos'
    die'' = addFaceValueToDie die' gv
    path' = addMoveToPath pos' die'' path

addMoveToPath :: GridPosition -> Die -> Path -> Path
addMoveToPath pos die path = (pos, valOrFace die) : path

valOrFace :: Die -> GridValue
valOrFace d@Die{..} =
    case getDieValue d of
        Nothing  -> GVConstraint topFace
        Just val -> GVValue val

gridDirections :: Grid -> GridPosition -> [Direction]
gridDirections grid (x,y) = catMaybes [checkN, checkS, checkE, checkW]
    where
        checkN = if y > 0 then Just North else Nothing
        checkW = if x > 0 then Just West else Nothing
        checkS = if y < (height grid - 1) then Just South else Nothing
        checkE = if x < (width grid - 1) then Just East else Nothing

pathAllows :: Path -> Direction -> Bool
pathAllows path dir = pos' `notElem` (fst <$> path)
  where
    pos = getPosFromPath path
    pos' = changePos pos dir

changePos (x,y) North = (x, y-1)
changePos (x,y) South = (x, y+1)
changePos (x,y) East  = (x+1, y)
changePos (x,y) West  = (x-1, y)

-- Either one of the values can be Nothing, or they must be equal
dieAllows :: Grid -> GridPosition -> Die -> Direction -> Bool
dieAllows grid pos die dir = isNothing dv || isNothing gc || dv == gc
  where
    pos' = changePos pos dir
    die' = roll die dir
    dv   = getDieValue die'
    gc   = getGridCell grid pos'

getGridCell :: Grid -> GridPosition -> Maybe Value
getGridCell Grid{..} (x,y) = cells V.! (x + y * width)
