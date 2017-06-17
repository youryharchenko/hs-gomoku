{-# LANGUAGE DeriveGeneric #-}
module Gomoku
    (
    Game,
    calculate
    ) where

import GHC.Generics

data Game = Game
  { game :: [[Int]]
  , status :: Maybe String
  } deriving (Show, Generic)

data Net = Net
  { allSlots :: [Slot]
  , activeSlots :: [[Slot]]
  , allPoints :: [Point]
  , emptyPoints :: [Point]
  , steps :: [[Int]]
  } deriving (Show)

data Point  = Point
  { x :: Int
  , y :: Int
  , rp :: [Int]
  , sp :: Int
  } deriving (Show)

data Slot = Slot
  { scpX :: Int
  , scpY :: Int
  , d :: Int
  , rs :: Int
  , ss :: Int
  } deriving (Show)

calculate :: Game -> Game
calculate oldGame = newGame $ newNet oldGame

newNet :: Game -> Net
newNet oldGame = applySteps initNet $ game oldGame

applySteps :: Net -> [[Int]] -> Net
applySteps = foldl applyStep

applyStep :: Net -> [Int] -> Net
applyStep net step = Net
  { steps = steps net ++ [step]
  , allPoints = allPoints net
  , emptyPoints = emptyPoints net
  , allSlots = allSlots net
  , activeSlots = activeSlots net}


initNet :: Net
initNet = Net
  { steps = []
  , allPoints = fillAllPoints
  , emptyPoints = fillAllPoints
  , allSlots = fillAllSlots
  , activeSlots = [fillAllSlots, [], []]}
  where
    fillAllPoints = [Point{x = i, y = j, rp = [countSlotsForPoint i j, 0, 0], sp = 0} | i <- [0..14], j <- [0..14]]
    fillAllSlots = [Slot {scpX = i, scpY = j, d = k, rs = 0, ss = 0} | i <- [0..14], j <- [0..14], k <- [0..3], isScpValid i j k]
    countSlotsForPoint x y = length $ createSlotsForPoint x y

isScpValid :: Int -> Int -> Int -> Bool
isScpValid x y d
  | d == 0 && y > 1 && y < 13 = True
  | d == 1 && x > 1 && x < 13 = True
  | d == 2 && (x > 1 && y < 13) && (x < 13 && y > 1) = True
  | d == 3 && (x > 1 && y > 1) && (x < 13 && y < 13) = True
  | otherwise = False

createSlotsForPoint :: Int -> Int -> [Slot]
createSlotsForPoint x y =
  [Slot{scpX = x, scpY = y + i, d = 0, rs = 0, ss = 0} | i <- [-2..2], isScpValid x (y + i) 0] ++
  [Slot{scpX = x + i, scpY = y, d = 1, rs = 0, ss = 0} | i <- [-2..2], isScpValid (x + i) y 1] ++
  [Slot{scpX = x + i, scpY = y + i, d = 2, rs = 0, ss = 0} | i <- [-2..2], isScpValid (x + i) (y + i) 2] ++
  [Slot{scpX = x + i, scpY = y - i, d = 3, rs = 0, ss = 0} | i <- [-2..2], isScpValid (x + i) (y - i) 3]

newGame :: Net -> Game
newGame net
  | checkOver net = Game {game = steps net, status = Just "over"}
  | checkOver $ nextNet net nextStep = Game {game = steps net ++ [nextStep], status = Just "over"}
  | otherwise = Game {game = steps net ++ [nextStep], status = Just "play"}
  where nextStep = calcStep net

checkOver :: Net -> Bool
checkOver net = checkWin net || checkDraw net

checkWin :: Net -> Bool
checkWin net = not (null [s | s <- a1, rs s == 5]) || not (null [s | s <- a2, rs s == 5])
  where (_: a1 : a2: _) = activeSlots net

checkDraw :: Net -> Bool
checkDraw net = null a0 && null a1 && null a2
  where (a0 : a1 : a2: _) = activeSlots net

calcStep :: Net -> [Int]
calcStep net = [0,0]

nextNet :: Net -> [Int] -> Net
nextNet = applyStep
