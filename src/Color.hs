module Color(black, blue, Color(Color), crimson, cyan, dpurple, gray, green, lpurple, pink, red, rose, white) where

import Bizzlelude

newtype Color = Color (Int, Int, Int) deriving (Eq, Show)

black   = Color (  0,   0,   0)
dpurple = Color (128,   0, 255)
lpurple = Color (185, 115, 255)
green   = Color (  0, 255,  33)
crimson = Color (127,   0,   0)
red     = Color (255,   0,   0)
rose    = Color (255, 115, 115)
cyan    = Color (  0, 255, 255)
pink    = Color (255,  76, 255)
blue    = Color (  0,   0, 217)
gray    = Color (168, 168, 183)
white   = Color (255, 255, 255)
