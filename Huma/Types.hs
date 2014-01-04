module Huma.Types where

import Data.Map (Map)
import Data.Sequence (Seq)

type Width = Float
type BallId = Int
type Index = Int
type Tick = Int
data BallColor = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Eq, Show, Ord)
data Point = Point Float Float deriving (Eq, Show)
data Line = Line Point Point
data IndexedPoint = IndexedPoint Index Point deriving (Eq, Show)

-- a regular ball in Zuma
data Ball = Ball BallId Width BallColor deriving (Eq, Ord, Show)
-- a contiguous segment which a ball may traverse
data Path = PointPath [IndexedPoint] deriving (Eq, Show)
-- A collection of paths that feed into each other
data Way = Way [Path] deriving (Eq, Show)
data Chain = Chain [Ball] deriving (Eq, Show)
data Position = Position Ball IndexedPoint deriving (Eq, Show)
data Positions = PositionMap (Map Ball Position) deriving (Eq, Show)
-- A Transit describes a chain on a certain "Way"
data Transit = Transit Chain Way Positions deriving (Eq, Show)
data Match = Match [Ball]

data BallGenerator = SequentialGenerator Index Width deriving (Show, Eq)
type Angle = Float
-- represents balls flying over the screen
type FreeBall = (Ball, Point, Point, Angle)
type BallQueue = Seq Ball
data GameState = GameState BallGenerator [Transit] 
  [FreeBall] BallQueue
  deriving (Show, Eq)
type GameTick = (GameState, Tick)
type BallPt = (Ball, Point)
type Distance = Float
data Collision = Collision FreeBall Position Distance deriving (Eq, Show)

-- list of game ticks, in order of descending time
data Game = Game [GameTick]
