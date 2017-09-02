module Shape(Point(Point), Shape(Circle, Line, Polygon)) where

import Bizzlelude

import Color(Color(Color))

newtype Point = Point (Double, Double) deriving (Eq, Show)

data Shape
  = Circle  Point Int Color Int Color
  | Line Point Point Color Int
  | Polygon [Point] Color Color
