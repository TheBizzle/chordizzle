module Color(black, blue, Color(Color), crimson, cyan, dpurple, gray, green, lpurple, pink, red, rose, white, yellow) where

import Bizzlelude

newtype Color = Color (Int, Int, Int) deriving (Eq, Show)

black   = Color (  0,   0,   0)
blue    = Color (  0,   0, 217)
crimson = Color (127,   0,   0)
cyan    = Color (  0, 255, 255)
dpurple = Color (128,   0, 255)
gray    = Color (168, 168, 183)
green   = Color (  0, 255,  33)
lpurple = Color (185, 115, 255)
pink    = Color (255,  76, 255)
red     = Color (255,   0,   0)
rose    = Color (255, 115, 115)
white   = Color (255, 255, 255)
yellow  = Color (255, 255,   0)
