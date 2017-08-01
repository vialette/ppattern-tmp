{-|
Module      : Data.Algorithm.PPattern.Geometry.Point
Description : Simple 2D point
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Geometry.Point
(
  -- * The @Point@ type
  Point(..)

  -- * Constructing
, mk

 -- * Accessing
, xCoord
, yCoord

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateXCoord'
, updateYCoord
, updateYCoord'
)
where

  newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

  {-|
    Make a point from x and y coordinates.
  -}
  mk :: Int -> Int -> Point
  mk x y = Point (x, y)

  {-|
    Transform a point to a pair.
  -}
  toTuple :: Point -> (Int, Int)
  toTuple (Point (x, y)) = (x, y)

  {-|
    Get x-coordinate of a point.
  -}
  xCoord :: Point -> Int
  xCoord (Point (x, _)) = x

  {-|
    Get y-coordinate of a point.
  -}
  yCoord :: Point -> Int
  yCoord (Point (_, y)) = y

  {-|
    Make a new point from a point by updating its x-ccordinate.
  -}
  updateXCoord :: Int -> Point -> Point
  updateXCoord x' (Point (_, y)) = mk x' y

  {-|
    Make a new point from a point by function-updating its x-ccordinate.
  -}
  updateXCoord' :: (Int -> Int) -> Point -> Point
  updateXCoord' f (Point (x, y)) = mk x' y
    where
      x' = f x

  {-|
    Make a new point from a point by updating its y-ccordinate.
  -}
  updateYCoord :: Int -> Point -> Point
  updateYCoord y' (Point (x, _)) = mk x y'

  {-|
    Make a new point from a point by function-updating its y-ccordinate.
  -}
  updateYCoord' :: (Int -> Int) -> Point -> Point
  updateYCoord' f (Point (x, y)) = mk x y'
    where
      y' = f y
