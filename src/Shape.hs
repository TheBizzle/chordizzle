module Shape(Point(Point), Shape(Circle, Line, Polygon)) where

import Bizzlelude

import Color(Color(Color))

newtype Point = Point (Double, Double) deriving (Eq, Show)

data Shape
  = Circle  Point Int Color Int Color -- TODO: Maj7 color outlines are inverted!
  | Line Point Point Color Int
  | Polygon [Point] Color Color
