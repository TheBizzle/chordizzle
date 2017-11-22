module Chords(processChords) where

import Bizzlelude

import Color(black, blue, Color, crimson, cyan, dpurple, gray, green, lpurple, pink, red, rose, white, yellow)
import Shape(Point(Point), Shape(Circle, Line, Polygon))
import SVG(serialize)
import Theory(Chord, chordToList, Note, Solfege(Do, Ra, Re, Me, Mi, Fa, Se, Sol, Le, La, Te, Ti), StringChord, StringNote, toStringChord)

import qualified Data.List as List
import qualified Data.Text as Text

processChords :: Text -> [(Text, Text)]
processChords = lines >>> (List.filter isChordLine) >>> (map $ extractData >>> makePairing)
  where
    isChordLine    = Text.head >>> (/= ' ')
    extractData xs = (a, extractFromParens b, extractFromParens c)
      where
        [a, b, c]         = Text.splitOn " " xs
        extractFromParens = (Text.drop 1) >>> Text.reverse >>> (Text.drop 1) >>> Text.reverse
    makePairing (a, _, c) = (a, c |> (asString >>> importChord >>> toStringChord >>> draw))

importChord :: String -> Chord
importChord [e, a, d, g, b, h] = (charToNote e, charToNote a, charToNote d, charToNote g, charToNote b, charToNote h)
importChord x                  = error $ "Invalid chord string: " <> showText x

draw :: StringChord -> Text
draw chord = serialize $ [background] <> [gridBox] <> strings <> frets <> dots
  where
    fullWidth  = 500
    fullHeight = 620
    background = Polygon (map Point [(0, 0), (fullWidth, 0), (fullWidth, fullHeight), (0, fullHeight)]) white white
    width      = fullWidth * 0.8
    height     = width / 5 * 7
    stringGap  =  width / 5
    fretGap    = height / 5
    gridBox    = Polygon (map shiftIt [(0, 0), (width, 0), (width, height), (0, height)]) white black
    strings    = map (\x -> Line (shiftIt (x, 0)) (shiftIt (x, height)) black 3) [stringGap, (2 * stringGap) ..  width]
    frets      = map (\y -> Line (shiftIt (0, y)) (shiftIt (width,  y)) black 3) [  fretGap, (2 *   fretGap) .. height]
    dots       = fst $ foldr genCircle ([], 0) (chord |> (chordToList >>> List.reverse))

    shiftIt :: (Double, Double) -> Point
    shiftIt = (+ (fullWidth * 0.1)) *** (+ (fullHeight * 3 / 62)) >>> Point

    genCircle :: StringNote -> ([Shape], Int) -> ([Shape], Int)
    genCircle (Nothing      ) (notes, stringNum) = (notes,          stringNum + 1)
    genCircle (Just (n, sol)) (notes, stringNum) = ((note : notes), stringNum + 1)
      where
        y           = ((fromIntegral n) * fretGap) + (fretGap / 2)
        notePoint   = shiftIt ((fromIntegral stringNum) * width / 5, y)
        radius      = floor $ stringGap / 2.5
        color       = solfegeColor sol
        borderColor = if color == white || color == yellow then black else color
        borderWidth = floor $ (fromIntegral radius) / (4 :: Double)
        note        = Circle notePoint radius color borderColor borderWidth

charToNote :: Char -> Note
charToNote 'a' = Just Do
charToNote 'b' = Just Ra
charToNote 'c' = Just Re
charToNote 'd' = Just Me
charToNote 'e' = Just Mi
charToNote 'f' = Just Fa
charToNote 'g' = Just Se
charToNote 'h' = Just Sol
charToNote 'i' = Just Le
charToNote 'j' = Just La
charToNote 'k' = Just Te
charToNote 'l' = Just Ti
charToNote '-' = Nothing
charToNote z   = error $ "Invalid note character: " <> showText z

solfegeColor :: Solfege -> Color
solfegeColor Do  = green
solfegeColor Ra  = yellow
solfegeColor Re  = cyan
solfegeColor Me  = dpurple
solfegeColor Mi  = lpurple
solfegeColor Fa  = pink
solfegeColor Se  = crimson
solfegeColor Sol = red
solfegeColor Le  = rose
solfegeColor La  = blue
solfegeColor Te  = gray
solfegeColor Ti  = white
