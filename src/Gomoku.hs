{-# LANGUAGE DeriveGeneric #-}
module Gomoku
    (
    Game,
    calculate
    ) where

import GHC.Generics
import System.Random

data Game = Game
  { game :: [[Int]]
  , status :: Maybe String
  } deriving (Show, Generic)

data Net = Net
  { allSlots :: [Slot]
  , allPoints :: [Point]
  , union :: [Union]
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

data Union = Union
  { point :: Point
  ,  slot :: Slot
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
  , allPoints = nextPoints
  , allSlots = nextSlots
  , union = nextUnion}
  where
    nextPoints = nextAllPoints net step
    nextSlots = nextAllSlots net step
    nextUnion = [Union {point = p, slot = s} | p <- nextPoints, s <- nextSlots, isPointInSlot p s]

nextAllPoints :: Net -> [Int] -> [Point]
nextAllPoints net step = [if not (x p == px && y p == py) then p else Point {x = px, y = py, rp = rp p, sp = c} | p <- points]
  where
    px = head step
    py = last step
    points = allPoints net
    c = 1 + mod (length $ steps net) 2

nextAllSlots :: Net -> [Int] -> [Slot]
nextAllSlots net step = [nextSlot s sx sy | s <- slots]
  where
    sx = head step
    sy = last step
    slots = allSlots net
    c = 1 + mod (length $ steps net) 2
    nextSlot slot x y
      | inSlot && st == 0 = Slot{scpX = scpx, scpY = scpy, d = sd, rs = 1, ss = c}
      | inSlot && st == c = Slot{scpX = scpx, scpY = scpy, d = sd, rs = srs + 1, ss = st}
      | inSlot && st < 3 = Slot{scpX = scpx, scpY = scpy, d = sd, rs = -1, ss = 3}
      | otherwise = slot
      where
        inSlot = isPointInSlot Point{x = x, y = y, rp = [0, 0, 0], sp = 0} slot
        st = ss slot
        scpx = scpX slot
        scpy = scpY slot
        sd = d slot
        srs = rs slot


initNet :: Net
initNet = Net
  { steps = []
  , allPoints = fillAllPoints
  , allSlots = fillAllSlots
  , union = [Union {point = p, slot = s} | p <- fillAllPoints, s <- fillAllSlots, isPointInSlot p s]}
  where
    fillAllPoints = [Point{x = i, y = j, rp = [countSlotsForPoint i j, 0, 0], sp = 0} | i <- [0..14], j <- [0..14]]
    fillAllSlots = [Slot {scpX = i, scpY = j, d = k, rs = 0, ss = 0} | i <- [0..14], j <- [0..14], k <- [0..3], isScpValid i j k]
    countSlotsForPoint x y = length $ createSlotsForPoint x y

isPointInSlot :: Point -> Slot -> Bool
isPointInSlot point slot
  | ds == 0 && dx == 0 && dy < 3 = True
  | ds == 1 && dy == 0 && dx < 3 = True
  | (ds == 2 || ds == 3) && dx == dy && dx < 3 = True
  | otherwise = False
  where
    ds = d slot
    xs = scpX slot
    ys = scpY slot
    xp = x point
    yp = y point
    dx = abs (xp - xs)
    dy = abs (yp - ys)

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

activeSlots :: Net -> [[Slot]]
activeSlots net = [slots0, slots1, slots2]
  where
    alls = allSlots net
    slots0 = [s | s <- alls, ss s == 0]
    slots1 = [s | s <- alls, ss s == 1]
    slots2 = [s | s <- alls, ss s == 2]

nextNet :: Net -> [Int] -> Net
nextNet = applyStep

pick :: [[Int]] -> [Int]
pick xs = step
  where
    (randNumber, newGen) = randomR (0, length xs - 1) (mkStdGen $ length xs) :: (Int, StdGen)
    step = head $ drop randNumber xs

calcStep :: Net -> [Int]
calcStep net
  | not (null maxRate) = pick maxRate
  where
    c = 1 + mod (length $ steps net) 2
    maxRate = calcPointMaxRate net c

calcPointMaxRate :: Net -> Int -> [[Int]]
calcPointMaxRate net c = [[0,1], [2,3], [10, 12]]
