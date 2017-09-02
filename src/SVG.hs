module SVG(serialize) where

import Bizzlelude

import Color(Color(Color))
import Shape(Shape(Circle, Line, Polygon), Point(Point))

serializePoint :: Point -> Text
serializePoint (Point (x, y)) = (showText x) <> "," <> (showText y) <> " "

serializeColor :: Color -> Text
serializeColor (Color (r, g, b)) = "rgb(" <> (showText r) <> "," <> (showText g) <> "," <> (showText b) <> ");"

serializeShape :: Shape -> Text
serializeShape (Circle (Point (x, y)) radius fillColor borderWidth borderColor) = "<circle cx=\"" <> (showText x) <> "\" cy=\"" <> (showText y) <> "\" r=\"" <> (showText radius) <> "\" stroke=\"" <> (serializeColor borderColor) <> "\" stroke-width=\"" <> (showText borderWidth) <> "\" fill=\"" <> (serializeColor fillColor) <> "\" />"
serializeShape (Line (Point (x1, y1)) (Point (x2, y2)) color width)             = "<line x1=\"" <> (showText x1) <> "\" y1=\"" <> (showText y1) <> "\" x2=\"" <> (showText x2) <> "\" y2=\"" <> (showText y2) <> "\" style=\"stroke:" <> (serializeColor color) <> " stroke-width:" <> (showText width) <> "\" />"
serializeShape (Polygon ps inColor borderColor)                                 = "<polygon points=\"" <> (foldMap serializePoint ps) <> "\" style=\"fill:" <> (serializeColor inColor) <> " stroke:" <> (serializeColor borderColor) <> " stroke-width:2\" />"

serialize :: [Shape] -> Text
serialize p = "<svg xmlns=\"http://www.w3.org/2000/svg\">" <> (foldMap serializeShape p) <> "</svg>"
