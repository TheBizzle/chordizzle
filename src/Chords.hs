module Chords(processChords) where

import Bizzlelude

import qualified Data.List    as List
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO

data Solfege
  = Do
  | Ra
  | Re
  | Me
  | Mi
  | Fa
  | Se
  | Sol
  | Le
  | La
  | Te
  | Ti
  deriving (Show)

data GuitarString
  = E | A | D | G | B | H

type Note        = Maybe Solfege
type StringNote  = Maybe (Int, Solfege)
type Chord       = (Note, Note, Note, Note, Note, Note)
type StringChord = (StringNote, StringNote, StringNote, StringNote, StringNote, StringNote)
type SVG         = Text

importChord :: String -> Chord
importChord [e, a, d, g, b, h] = (charToNote e, charToNote a, charToNote d, charToNote g, charToNote b, charToNote h)
importChord x                  = error $ "Invalid chord string: " <> showText x

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

toStringChord :: Chord -> StringChord
toStringChord (e, a, d, g, b, h) = leftAlignChord (eNote, aNote, dNote, gNote, bNote, hNote)
  where
    eNote = toStringNote e E Nothing
    aNote = toStringNote a A eNote
    dNote = toStringNote d D aNote
    gNote = toStringNote g G dNote
    bNote = toStringNote b B gNote
    hNote = toStringNote h H bNote

toStringNote :: Note -> GuitarString -> StringNote -> StringNote
toStringNote Nothing  _ _                      = Nothing
toStringNote (Just x) _ Nothing                = Just (7, x)
toStringNote (Just x) g (Just (fret, solfege)) = Just (mySol, x)
  where
    mySol           = fret + ((difference x solfege) - (stringToBoost g))
    stringToBoost B = 4
    stringToBoost _ = 5

difference a b  = semiA - semiB + (if semiA < semiB then 12 else 0)
  where
    semiA = semitone a
    semiB = semitone b
    semitone Do  = 0
    semitone Ra  = 1
    semitone Re  = 2
    semitone Me  = 3
    semitone Mi  = 4
    semitone Fa  = 5
    semitone Se  = 6
    semitone Sol = 7
    semitone Le  = 8
    semitone La  = 9
    semitone Te  = 10
    semitone Ti  = 11

chordToList :: StringChord -> [StringNote]
chordToList (e, a, d, g, b, h) = [e, a, d, g, b, h]

listToChord :: [StringNote] -> StringChord
listToChord [e, a, d, g, b, h] = (e, a, d, g, b, h)

leftAlignChord :: StringChord -> StringChord
leftAlignChord chord = chord |> (chordToList >>> (map $ map $ first tamp) >>> listToChord)
  where
    tamp = chord |> (chordToList >>> catMaybes >>> (map fst) >>> minimum >>> subtract)

-- Box is 50 wide and 70 tall
-- Vertical   lines at: 10, 20, 30, 40
-- Horizontal lines at: 14, 28, 42, 56
-- Don't forget that major sevenths need a black border around the white circles!
-- Circles have a radius of 9, go on the centers of the vertical lines, and at the midpoints between horizontal lines
draw :: StringChord -> SVG
draw chord = serializeSVG $ [background] <> [gridBox] <> strings <> frets <> (fst dots)
  where
    background = Polygon (map (uncurry Point) [(0, 0), (600, 0), (600, 800), (0, 800)]) white white
    width      = 400
    height     = width / 50 * 70
    stringGap  =  width / 5
    fretGap    = height / 5
    gridBox    = Polygon (map shiftIt [(0, 0), (width, 0), (width, height), (0, height)]) white black
    strings    = map (\x -> Line (shiftIt (x, 0)) (shiftIt (x, height)) black 3) [stringGap, (2 * stringGap) ..  width]
    frets      = map (\y -> Line (shiftIt (0, y)) (shiftIt (width,  y)) black 3) [  fretGap, (2 *   fretGap) .. height]
    dots       = foldr genCircle ([], 0) (chord |> (chordToList >>> List.reverse))

    shiftIt :: (Double, Double) -> Point
    shiftIt = (+ 20) *** (+ 10) >>> (uncurry Point)

    genCircle :: StringNote -> ([Shape], Int) -> ([Shape], Int)
    genCircle (Nothing      ) (notes, stringNum) = (notes,          stringNum + 1)
    genCircle (Just (n, sol)) (notes, stringNum) = ((note : notes), stringNum + 1)
      where
        note  = Circle (shiftIt ((fromIntegral stringNum) * width / 5, y)) 9 color 2 (if color == white then black else color)
        color = solfegeColor sol
        y     = ((fromIntegral n) * fretGap) + (fretGap / 2)

processChords :: Text -> [(Text, SVG)]
processChords = lines >>> (List.filter isChordLine) >>> (map $ extractData >>> makeSVG)
  where
    isChordLine       = Text.head >>> (/= ' ')
    extractData xs    = (a, extractFromParens b, extractFromParens c)
      where
        [a, b, c]         = Text.splitOn " " xs
        extractFromParens = (Text.drop 1) >>> Text.reverse >>> (Text.drop 1) >>> Text.reverse
    makeSVG (a, _, c) = (a, c |> (asString >>> importChord >>> toStringChord >>> draw))

type Color = (Int, Int, Int)

black   = (  0,   0,   0) -- Line
dpurple = (128,   0, 255) -- Minor 3
lpurple = (185, 115, 255) -- Major 3
green   = (  0, 255,  33) -- Perfect 1
crimson = (127,   0,   0) -- Diminished 5
red     = (255,   0,   0) -- Perfect 5
rose    = (255, 115, 115) -- Augmented 5
cyan    = (  0, 255, 255) -- Major 2
pink    = (255,  76, 255) -- Perfect 4
blue    = (  0,   0, 217) -- Major 6
gray    = (168, 168, 183) -- Minor 7
white   = (255, 255, 255) -- Major 7

solfegeColor :: Solfege -> Color
solfegeColor Do  = green
solfegeColor Ra  = error "Nerp"
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

data Point
  = Point Double Double

data Shape
  = Circle  Point Int Color Int Color
  | Polygon [Point] Color Color
  | Line Point Point Color Int

serializePoint :: Point -> Text
serializePoint (Point x y) = (showText x) <> "," <> (showText y) <> " "

serializeColor :: Color -> Text
serializeColor (r, g, b) = "rgb(" <> (showText r) <> "," <> (showText g) <> "," <> (showText b) <> ");"

serializeShape :: Shape -> SVG
serializeShape (Polygon ps inColor borderColor)                              = "<polygon points=\"" <> (foldMap serializePoint ps) <> "\" style=\"fill:" <> (serializeColor inColor) <> " stroke:" <> (serializeColor borderColor) <> " stroke-width:2\" />"
serializeShape (Circle (Point x y) radius borderColor borderWidth fillColor) = "<circle cx=\"" <> (showText x) <> "\" cy=\"" <> (showText y) <> "\" r=\"" <> (showText radius) <> "\" stroke=\"" <> (serializeColor borderColor) <> "\" stroke-width=\"" <> (showText borderWidth) <> "\" fill=\"" <> (serializeColor fillColor) <> "\" />"
serializeShape (Line (Point x1 y1) (Point x2 y2) color width)                = "<line x1=\"" <> (showText x1) <> "\" y1=\"" <> (showText y1) <> "\" x2=\"" <> (showText x2) <> "\" y2=\"" <> (showText y2) <> "\" style=\"stroke:" <> (serializeColor color) <> " stroke-width:" <> (showText width) <> "\" />"

serializeSVG :: [Shape] -> SVG
serializeSVG p = "<svg xmlns=\"http://www.w3.org/2000/svg\">" <> (foldMap serializeShape p) <> "</svg>"
