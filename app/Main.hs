{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main
(main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Maybe
import GHC.Generics
import Control.Monad (join)
import Control.Parallel.Strategies

main :: IO ()
--main = do profiler
main = do
    let paths = runReal
    --let bestPath = maximumBy (comparing getValue) paths
    --printInfo bestPath
    mapM_ printInfo paths

profiler = mapM_ printInfo $ runDFS (7,7) $ State bigProfilingGrid profilingDie [((0,0), GVValue 6)] (UGrid 8 8 (VU.replicate 64 False))

profilingDie = Die DfA DfC $ FV (Just 6) Nothing Nothing Nothing Nothing Nothing

printInfo :: State -> IO ()
printInfo s@State{..} = do
    let value = getValue s
    putStrLn $ "Total Value: " ++ show value
    putStrLn $ "Total Length: " ++ show (length path)
    print die
    print (reverse path)

getValue :: State -> Integer
getValue State{..} = product $ map (fromIntegral . snd) $ fixPathConstraints die path

fixPathConstraints :: Die -> [(t, GridValue)] -> [(t, Value)]
fixPathConstraints die = map fixElem
  where
    fixElem (p, GVValue val) = (p, val)
    fixElem (p, GVConstraint face) = (p, fromMaybe 9 (getDieFaceValue die face))

runDFS :: GridPosition -> State -> [State]
--        end position -> InitialState -> FinalStates
runDFS end state = go [state]
  where
    go [] = []
    go (x:xs) = if isFinished x
                  then x : go xs
                  else go (doMoves x ++ xs)
    isFinished State{..} = getUGridCell pathGrid end

--- Data
runReal :: [State]
runReal = let startingStates = concatMap doMoves (concatMap doMoves (doMoves realState)) in join (parMap rdeepseq (runDFS (11,11)) startingStates)
--runAnswer = runDFS (11,11) $ State realGrid answerDie [((0,0), GVValue 1)]
runTest = runDFS (4,6) danielState

realGrid :: Grid (Maybe Value)
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

danielGrid :: Grid (Maybe Value)
danielGrid = Grid 5 7 (V.fromList
  [ Just 3 , Just 4, Just 1, Just 7, Just 5,
    Just 1 , Just 2,Nothing, Just 3, Just 5,
    Nothing,Nothing,Nothing,Nothing,Nothing,
    Nothing,Nothing,Nothing,Nothing,Nothing,
    Nothing, Just 4, Just 3,Nothing, Just 2,
    Nothing, Just 5,Nothing, Just 2, Just 3,
    Just 5 ,Nothing,Nothing, Just 4, Just 1])
danielDie :: Die
danielDie = Die DfA DfC $ FV (Just 3) Nothing Nothing Nothing Nothing Nothing
danielState :: State
danielState = State danielGrid danielDie [((0,0), GVValue 3)] $ mkPathGrid (5,7)

mkPathGrid (w,h) = UGrid w h $ (VU.replicate (w*h) False) `VU.unsafeUpd` [(0,True)]

bigProfilingGrid :: Grid (Maybe Value)
bigProfilingGrid = Grid 8 8 (V.fromList
  [ Just 6, Just 4, Just 1, Just 8, Just 1, Just 4, Just 2, Just 1,
    Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 1,
    Just 1,Nothing, Just 6, Just 1, Just 6, Just 2,Nothing, Just 2,
    Just 4,Nothing, Just 1,Nothing,Nothing, Just 8,Nothing, Just 3,
    Just 2,Nothing, Just 5,Nothing,Nothing, Just 3,Nothing, Just 5,
    Just 5,Nothing, Just 1, Just 1, Just 2, Just 3,Nothing, Just 4,
    Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Just 3,
    Just 6, Just 3, Just 6, Just 5, Just 4, Just 3, Just 4, Just 5])

realDie :: Die
realDie = Die DfA DfC $ FV (Just 1) Nothing Nothing Nothing Nothing Nothing
--answerDie = Die DfA DfC [(DfA, 1), (DfB, 7), (DfC, 3), (DfD, 8), (DfE, 6), (DfF, 6)]
realState :: State
realState = State realGrid realDie [((0,0), GVValue 1)] $ mkPathGrid (12,12)


--- Die Definitions
type Value = Int -- (1-9), but will be multiplied together at the end
data Direction = North | South | East | West deriving (Show, Eq, Generic, NFData)

data DieFace = DfA | DfB | DfC | DfD | DfE | DfF deriving (Show, Eq, Generic, NFData)
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
            , faceValues :: FaceValues
            } deriving (Show, Generic, NFData)

data FaceValues = FV {
      fvA :: Maybe Value
    , fvB :: Maybe Value
    , fvC :: Maybe Value
    , fvD :: Maybe Value
    , fvE :: Maybe Value
    , fvF :: Maybe Value
    } deriving (Show, Generic, NFData)

addFaceValueToDie :: Die -> Maybe Value -> Die
addFaceValueToDie d@Die{..} Nothing  = d
addFaceValueToDie d@Die{..} (Just x) = d { faceValues = faceValues' }
  where
    faceValues' = case getDieValue d of
                    Nothing -> updateFV faceValues topFace x
                    Just _  -> faceValues

updateFV :: FaceValues -> DieFace -> Value -> FaceValues
updateFV fv@FV{..} DfA x = fv { fvA = Just x }
updateFV fv@FV{..} DfB x = fv { fvB = Just x }
updateFV fv@FV{..} DfC x = fv { fvC = Just x }
updateFV fv@FV{..} DfD x = fv { fvD = Just x }
updateFV fv@FV{..} DfE x = fv { fvE = Just x }
updateFV fv@FV{..} DfF x = fv { fvF = Just x }


getDieValue :: Die -> Maybe Value
getDieValue d@Die{..} = getDieFaceValue d topFace

getDieFaceValue :: Die -> DieFace -> Maybe Value
getDieFaceValue Die{..} = getDieFaceValue' faceValues
  where
    getDieFaceValue' (FV x _ _ _ _ _) DfA = x
    getDieFaceValue' (FV _ x _ _ _ _) DfB = x
    getDieFaceValue' (FV _ _ x _ _ _) DfC = x
    getDieFaceValue' (FV _ _ _ x _ _) DfD = x
    getDieFaceValue' (FV _ _ _ _ x _) DfE = x
    getDieFaceValue' (FV _ _ _ _ _ x) DfF = x

roll :: Die -> Direction -> Die
roll die dir = let (top, north) = roll' dir (topFace die) (northFace die) in
                die { topFace = top, northFace = north }

roll' :: Direction -> DieFace -> DieFace -> (DieFace, DieFace)
--       direction -> top face -> north face -> (top', north')
roll' North top north = (opp north, top)
roll' West  top north = let rtop = dieRight' top north in (opp rtop, north)
roll' South top north = (north, opp top)
roll' East  top north = (dieRight' top north, north)

dieRight' :: DieFace -> DieFace -> DieFace
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
    deriving (Show, Generic, NFData)

data Grid a = Grid
            { width :: Int
            , height :: Int
            , cells :: V.Vector a
            } deriving (Show, Generic, NFData)

data UnboxedGrid a = UGrid
            { uwidth :: Int
            , uheight :: Int
            , ucells :: VU.Vector a
            } deriving (Show, Generic, NFData)
{--
printGrid Grid{..} = go (V.toList cells)
  where
    go [] = putStrLn ""
    go cs = let (row, rems) = splitAt width cs in print row >> go rems
    --}

type Path = [(GridPosition, GridValue)]

getPosFromPath :: Path -> GridPosition
getPosFromPath = fst . head

data State = State
            { grid :: Grid (Maybe Value)
            , die :: Die
            , path :: Path
            , pathGrid :: UnboxedGrid Bool
            } deriving (Show, Generic, NFData)

doMoves :: State -> [State]
doMoves s@State{..} =
  let
    pos = getPosFromPath path
    -- check that grid doesn't block move
    dirs = gridDirections grid pos
    -- check that path doesn't block move (i.e repeating a space)
    dirs'' = filter (dieAllows grid pos die) . filter (pathAllows pathGrid pos) $ dirs in
    -- check that the die allows a move
    --dirs'' = filter (dieAllows grid pos die) dirs' in
  -- actually move/rotate die, add to path
  map (moveDie s) dirs''

moveDie :: State -> Direction -> State
moveDie s@State{..} dir = s{die=die'', path=path', pathGrid=pathGrid'}
  where
    pos = getPosFromPath path
    pos' = changePos pos dir
    die' = roll die dir
    gv = getGridCell grid pos'
    die'' = addFaceValueToDie die' gv
    path' = addMoveToPath pos' die'' path
    pathGrid' = addMoveToPathGrid pathGrid pos'

addMoveToPath :: GridPosition -> Die -> Path -> Path
addMoveToPath pos die path = (pos, valOrFace die) : path

addMoveToPathGrid :: UnboxedGrid Bool -> GridPosition -> UnboxedGrid Bool
addMoveToPathGrid path@UGrid{..} pos = path { ucells = ucells `VU.unsafeUpd` [(getUGridIndex path pos, True)] }

valOrFace :: Die -> GridValue
valOrFace d@Die{..} =
    case getDieValue d of
        Nothing  -> GVConstraint topFace
        Just val -> GVValue val

gridDirections :: Grid a -> GridPosition -> [Direction]
gridDirections grid (x,y) = join [checkN, checkS, checkE, checkW]
    where
        checkN = if y > 0 then [North] else []
        checkW = if x > 0 then [West] else []
        checkS = if y < (height grid - 1) then [South] else []
        checkE = if x < (width grid - 1) then [East] else []

pathAllows :: UnboxedGrid Bool -> GridPosition -> Direction -> Bool
pathAllows pathGrid pos dir = not $ getUGridCell pathGrid pos'
  where
    pos' = changePos pos dir

changePos :: GridPosition -> Direction -> GridPosition
changePos (x,y) North = (x, y-1)
changePos (x,y) South = (x, y+1)
changePos (x,y) East  = (x+1, y)
changePos (x,y) West  = (x-1, y)

-- Either one of the values can be Nothing, or they must be equal
dieAllows :: Grid (Maybe Value) -> GridPosition -> Die -> Direction -> Bool
dieAllows grid pos die dir = isNothing dv || isNothing gc || dv == gc
  where
    pos' = changePos pos dir
    die' = roll die dir
    dv   = getDieValue die'
    gc   = getGridCell grid pos'

getGridCell :: Grid a -> GridPosition -> a
getGridCell g@Grid{..} pos = cells `V.unsafeIndex` getGridIndex g pos

getUGridCell :: (VU.Unbox a) => UnboxedGrid a -> GridPosition -> a
getUGridCell g@UGrid{..} pos = ucells `VU.unsafeIndex` getUGridIndex g pos

getGridIndex :: Grid a -> (Int, Int) -> Int
getGridIndex Grid{..} (x,y) = x + y * width

getUGridIndex :: UnboxedGrid a -> (Int, Int) -> Int
getUGridIndex UGrid{..} (x,y) = x + y * uwidth
