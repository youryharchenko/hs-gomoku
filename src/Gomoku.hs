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
  , seed :: Int
  } deriving (Show)

data Point  = Point
  { x :: Int
  , y :: Int
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


calculate :: Integer -> Game -> Game
calculate seed oldGame = newGame $ newNet randNumber oldGame
  where
    (randNumber, newGen) = randomR (0, 255) (mkStdGen $ fromInteger seed) :: (Int, StdGen)

newNet :: Int -> Game -> Net
newNet seed oldGame = applySteps (initNet seed) $ game oldGame

applySteps :: Net -> [[Int]] -> Net
applySteps = foldl applyStep

applyStep :: Net -> [Int] -> Net
applyStep net step = Net
  { steps = steps net ++ [step]
  , allPoints = nextPoints
  , allSlots = nextSlots
  , union = nextUnion
  , seed = seed net}
  where
    nextPoints = nextAllPoints net step
    nextSlots = nextAllSlots net step
    nextUnion = [Union {point = p, slot = s} | p <- nextPoints, s <- nextSlots, isPointInSlot p s]

nextAllPoints :: Net -> [Int] -> [Point]
nextAllPoints net step@[px, py] = [if not (x p == px && y p == py) then p else Point {x = px, y = py, sp = c} | p <- points]
  where
    points = allPoints net
    c = 1 + mod (length $ steps net) 2

nextAllSlots :: Net -> [Int] -> [Slot]
nextAllSlots net step@[sx, sy] = [nextSlot s sx sy | s <- slots]
  where
    slots = allSlots net
    c = 1 + mod (length $ steps net) 2
    nextSlot slot x y
      | inSlot && st == 0 = Slot{scpX = scpx, scpY = scpy, d = sd, rs = 1, ss = c}
      | inSlot && st == c = Slot{scpX = scpx, scpY = scpy, d = sd, rs = srs + 1, ss = st}
      | inSlot && st < 3 = Slot{scpX = scpx, scpY = scpy, d = sd, rs = -1, ss = 3}
      | otherwise = slot
      where
        inSlot = isPointInSlot Point{x = x, y = y, sp = 0} slot
        st = ss slot
        scpx = scpX slot
        scpy = scpY slot
        sd = d slot
        srs = rs slot


initNet :: Int -> Net
initNet seed = Net
  { steps = []
  , allPoints = fillAllPoints
  , allSlots = fillAllSlots
  , union = [Union {point = p, slot = s} | p <- fillAllPoints, s <- fillAllSlots, isPointInSlot p s]
  , seed = seed}
  where
    fillAllPoints = [Point{x = i, y = j, sp = 0} | i <- [0..14], j <- [0..14]]
    fillAllSlots = [Slot {scpX = i, scpY = j, d = k, rs = 0, ss = 0} | i <- [0..14], j <- [0..14], k <- [0..3], isScpValid i j k]

isPointInSlot :: Point -> Slot -> Bool
isPointInSlot point slot
  | ds == 0 && dx == 0 && abs dy < 3 = True
  | ds == 1 && dy == 0 && abs dx < 3 = True
  | ds == 2 && dx == dy && abs dx < 3 = True
  | ds == 3 && dx == -dy && abs dx < 3 = True
  | otherwise = False
  where
    ds = d slot
    xs = scpX slot
    ys = scpY slot
    xp = x point
    yp = y point
    dx = xp - xs
    dy = yp - ys

isScpValid :: Int -> Int -> Int -> Bool
isScpValid x y d
  | d == 0 && y > 1 && y < 13 = True
  | d == 1 && x > 1 && x < 13 = True
  | d == 2 && (x > 1 && y < 13) && (x < 13 && y > 1) = True
  | d == 3 && (x > 1 && y > 1) && (x < 13 && y < 13) = True
  | otherwise = False

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

emptyPoints :: Net -> [Point]
emptyPoints net = [p | p <- points, sp p == 0]
  where points = allPoints net

nextNet :: Net -> [Int] -> Net
nextNet = applyStep

pick :: Int -> [[Int]] -> [Int]
pick seed xs = step
  where
    (randNumber, newGen) = randomR (0, length xs - 1) (mkStdGen seed) :: (Int, StdGen)
    step = head $ drop randNumber xs

calcStep :: Net -> [Int]
calcStep net
  | not (null slot4c) = slot4c
  | not (null slot4ac) = slot4ac
  | not (null findX) = pick sd findX
  | not (null maxRate) = pick sd maxRate
  where
    c = 1 + mod (length $ steps net) 2
    slot4c = findSlot4 net c
    slot4ac = findSlot4 net (ac c)
    findX = findPointX net c
    maxRate = calcPointMaxRate net c
    sd = seed net


findSlot4 :: Net -> Int -> [Int]
findSlot4 net c
  | null points = []
  | otherwise = head points
  where
    points = [[x (point u), y (point u)] | u <- union net, ss (slot u) == c, rs (slot u) == 4, sp (point u) == 0]

findPointX :: Net -> Int -> [[Int]]
findPointX net c
  | not (null findXc21) = findXc21
  | not (null findXa21) = findXa21
  | not (null findXc15) = findXc15
  | not (null findXa15) = findXa15
  | not (null findXc14) = findXc14
  | not (null findXa14) = findXa14
  | not (null findXc13) = findXc13
  | not (null findXa13) = findXa13
  | not (null findXc12) = findXc12
  | not (null findXa12) = findXa12
  | not (null findXc11) = findXc11
  | not (null findXa11) = findXa11
  | not (null findXc010) = findXc010
  | not (null findXa010) = findXa010
  | not (null findXc09) = findXc09
  | not (null findXa09) = findXa09
  | not (null findXc08) = findXc08
  | not (null findXa08) = findXa08
  | not (null findXc07) = findXc07
  | not (null findXa07) = findXa07
  | otherwise = []
  where
    findXc21 = findX c 2 1
    findXa21 = findX (ac c) 2 1
    findXc15 = findX c 1 5
    findXa15 = findX (ac c) 1 5
    findXc14 = findX c 1 4
    findXa14 = findX (ac c) 1 4
    findXc13 = findX c 1 3
    findXa13 = findX (ac c) 1 3
    findXc12 = findX c 1 2
    findXa12 = findX (ac c) 1 2
    findXc11 = findX c 1 1
    findXa11 = findX (ac c) 1 1
    findXc010 = findX c 0 10
    findXa010 = findX (ac c) 0 10
    findXc09 = findX c 0 9
    findXa09 = findX (ac c) 0 9
    findXc08 = findX c 0 8
    findXa08 = findX (ac c) 0 8
    findXc07 = findX c 0 7
    findXa07 = findX (ac c) 0 7
    findX c r b = [[x p, y p] | (r, p) <- a, r == m, r > b]
      where
        a = [(xPoint net p c r, p) | p <- emptyPoints net]
        m = maximum [r | (r, _) <- a]


xPoint :: Net -> Point -> Int -> Int -> Int
xPoint net p c r = sum xList
  where
    xList = [1 | s <- pointSlots, ss s == c, rs s > r]
    pointSlots = [slot u | u <- union net, x (point u) == x p, y (point u) == y p]

calcPointMaxRate :: Net -> Int -> [[Int]]
calcPointMaxRate net c = [[x p, y p] | (r, p) <- a, r == m]
  where
    a = [(ratePoint net p c, p) | p <- emptyPoints net]
    m = maximum [r | (r, _) <- a]


ratePoint :: Net -> Point -> Int -> Int
ratePoint net p c = sum rList
  where
    rList = [calc s | s <- pointSlots]
    pointSlots = [slot u | u <- union net, x (point u) == x p, y (point u) == y p]
    calc s
      | ss s == 0 = 1
      | ss s == 3 = 0
      | otherwise = (1 + rs s) * (1 + rs s)

ac :: Int -> Int
ac c = 3 - c
