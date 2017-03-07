module Main where

import Data.List ((\\))
import Data.Maybe (fromJust)

main :: IO ()
main = print $ take 3 $ filter ((== Leaf B) . snd) $ map (\n -> (n, pruneGameTree . buildGameTree $ (newGame n))) [1..200]

data Player = A | B deriving (Eq, Show)
data Game = Game {
                last_number :: !Int,
                last_player :: !Player,
                target :: !Int
                } deriving (Eq, Show)

data GameTree = GameTree Game [GameTree] | Leaf Player deriving (Eq, Show)

newGame :: Int -> Game
newGame tar = Game 0 B tar

xWinningGame :: Player -> Game
xWinningGame p = Game 0 p 0

winner :: Game -> Maybe Player
winner game =
    case target game == 0 of
        True -> Just (last_player game)
        False -> Nothing

possible_moves :: Game -> [Int]
possible_moves game = [1..(min (target game) 9)] \\ [last_number game]

iterateGame :: Game -> [Game]
iterateGame game | target game == 0 = []
iterateGame game =
    let moves = possible_moves game
        target_num = target game
        next_player = switch $ last_player game
    in
        case  map (\n -> Game n next_player (target_num - n)) moves of
            [] -> [xWinningGame (last_player game)]
            next_gs -> next_gs


switch A = B
switch B = A

buildGameTree :: Game -> GameTree
buildGameTree g | winner g == Nothing = GameTree g (map buildGameTree (iterateGame g))
buildGameTree g = Leaf (fromJust $ winner g)

pruneGameTree :: GameTree -> GameTree
pruneGameTree (Leaf p) = Leaf p
pruneGameTree (GameTree g succs) =
    let
        succs' = map pruneGameTree succs
        player = switch $ last_player g
    in
        case any (== Leaf player) succs' of
            True -> Leaf player
            False -> Leaf $ last_player g
