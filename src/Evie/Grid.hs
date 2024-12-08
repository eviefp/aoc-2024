module Evie.Grid
  ( Grid(..)
  , Point (..)
  , Direction (..)
  , move
  , reverse
  , mkPoint
  , getValue
  , points
  ) where

import Helpers qualified
import Prelude hiding (reverse)

data Grid a = Grid
  { grid :: [[a]]
  }
  deriving stock (Show)

data Point = Point
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving stock (Enum, Bounded)

move :: Grid a -> Direction -> Point -> Maybe Point
move grid dir Point {..} =
  mkPoint grid $
{- FOURMOLU_DISABLE -}
    case dir of
      North     -> Point  x      (y - 1)
      NorthEast -> Point (x + 1) (y - 1)
      East      -> Point (x + 1)  y
      SouthEast -> Point (x + 1) (y + 1)
      South     -> Point  x      (y + 1)
      SouthWest -> Point (x - 1) (y + 1)
      West      -> Point (x - 1)  y
      NorthWest -> Point (x - 1) (y - 1)
{- FOURMOLU_ENABLE -}

reverse :: Direction -> Direction
reverse = \case
{- FOURMOLU_DISABLE -}
  North     -> South
  NorthEast -> SouthWest
  East      -> West
  SouthEast -> NorthWest
  South     -> North
  SouthWest -> NorthEast
  West      -> East
  NorthWest -> SouthEast
{- FOURMOLU_ENABLE -}

mkPoint :: Grid a -> Point -> Maybe Point
mkPoint Grid {..} Point {..} = do
  let rows = length grid
  columns <- length <$> Helpers.safeHead grid
  if x >= columns || y >= rows || x < 0 || y < 0
    then Nothing
    else Just Point {..}

getValue :: Grid a -> Point -> Maybe a
getValue Grid {..} Point {..} = go grid x y
  where
    go :: forall a. [[a]] -> Int -> Int -> Maybe a
    go grid' x' y' = 
      case (grid', x', y') of
        (((a:_):_), 0, 0) -> Just a
        (((_:as):_), _, 0) -> go [as] (x'-1) 0
        (_:as, _, _) -> go as x' (y'-1)
        _ -> Nothing

points :: Grid a -> [Point]
points Grid {..} =
  let rows = length grid
  in case length <$> Helpers.safeHead grid of
    Nothing -> []
    Just columns -> maybe [] id . traverse (mkPoint $ Grid grid) $ getPoints rows columns
  where
    getPoints :: Int -> Int -> [Point]
    getPoints rows columns = [Point x y | x <- [0..columns-1], y <- [0 .. rows-1]]
