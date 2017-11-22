module Shape(Point(Point), Shape(Circle, Line, Polygon), VectorDrawing(height, shapes, VectorDrawing, width)) where

import Bizzlelude

import Color(Color)

newtype Point = Point (Double, Double) deriving (Eq, Show)

data Shape
  = Circle  Point Int Color Color Int
  | Line Point Point Color Int
  | Polygon [Point] Color Color
  deriving (Eq, Show)

data VectorDrawing
  = VectorDrawing {
     width :: Double
  , height :: Double
  , shapes :: [Shape]
  } deriving (Eq, Show)
