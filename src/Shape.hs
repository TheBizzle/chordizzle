module Shape(Point(Point), Shape(Circle, Line, Polygon)) where

import Bizzlelude

import Color(Color)

newtype Point = Point (Double, Double) deriving (Eq, Show)

data Shape
  = Circle  Point Int Color Color Int
  | Line Point Point Color Int
  | Polygon [Point] Color Color
  deriving (Eq, Show)
