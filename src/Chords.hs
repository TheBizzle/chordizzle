module Chords(processChords) where

import Bizzlelude

import Color(black, blue, Color, crimson, cyan, dpurple, gray, green, lpurple, pink, red, rose, white)
import Shape(Point(Point), Shape(Circle, Line, Polygon))
import SVG(serialize)

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

draw :: StringChord -> Text
draw chord = serialize $ [background] <> [gridBox] <> strings <> frets <> (fst dots)
  where
    background = Polygon (map Point [(0, 0), (500, 0), (500, 620), (0, 620)]) white white
    width      = 400
    height     = width / 50 * 70
    stringGap  =  width / 5
    fretGap    = height / 5
    gridBox    = Polygon (map shiftIt [(0, 0), (width, 0), (width, height), (0, height)]) white black
    strings    = map (\x -> Line (shiftIt (x, 0)) (shiftIt (x, height)) black 3) [stringGap, (2 * stringGap) ..  width]
    frets      = map (\y -> Line (shiftIt (0, y)) (shiftIt (width,  y)) black 3) [  fretGap, (2 *   fretGap) .. height]
    dots       = foldr genCircle ([], 0) (chord |> (chordToList >>> List.reverse))

    shiftIt :: (Double, Double) -> Point
    shiftIt = (+ 50) *** (+ 30) >>> Point

    genCircle :: StringNote -> ([Shape], Int) -> ([Shape], Int)
    genCircle (Nothing      ) (notes, stringNum) = (notes,          stringNum + 1)
    genCircle (Just (n, sol)) (notes, stringNum) = ((note : notes), stringNum + 1)
      where
        y           = ((fromIntegral n) * fretGap) + (fretGap / 2)
        notePoint   = shiftIt ((fromIntegral stringNum) * width / 5, y)
        radius      = floor $ stringGap / 2.5
        color       = solfegeColor sol
        borderColor = if color == white then black else color
        borderWidth = floor $ (fromIntegral radius) / 4
        note        = Circle notePoint radius color borderWidth borderColor

processChords :: Text -> [(Text, Text)]
processChords = lines >>> (List.filter isChordLine) >>> (map $ extractData >>> makeSVG)
  where
    isChordLine       = Text.head >>> (/= ' ')
    extractData xs    = (a, extractFromParens b, extractFromParens c)
      where
        [a, b, c]         = Text.splitOn " " xs
        extractFromParens = (Text.drop 1) >>> Text.reverse >>> (Text.drop 1) >>> Text.reverse
    makeSVG (a, _, c) = (a, c |> (asString >>> importChord >>> toStringChord >>> draw))

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
