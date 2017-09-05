module Theory(Chord, chordToList, listToChord, Note, Solfege(Do, Ra, Re, Me, Mi, Fa, Se, Sol, Le, La, Te, Ti), StringChord, StringNote, toStringChord) where

import Bizzlelude

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

chordToList :: StringChord -> [StringNote]
chordToList (e, a, d, g, b, h) = [e, a, d, g, b, h]

listToChord :: [StringNote] -> StringChord
listToChord [e, a, d, g, b, h] = (e, a, d, g, b, h)

toStringChord :: Chord -> StringChord
toStringChord (e, a, d, g, b, h) = leftAlignChord (eNote, aNote, dNote, gNote, bNote, hNote)
  where
    eNote = toStringNote e E Nothing
    aNote = toStringNote a A eNote
    dNote = toStringNote d D aNote
    gNote = toStringNote g G dNote
    bNote = toStringNote b B gNote
    hNote = toStringNote h H bNote

leftAlignChord :: StringChord -> StringChord
leftAlignChord chord = chord |> (chordToList >>> (map $ map $ first tamp) >>> listToChord)
  where
    tamp = chord |> (chordToList >>> catMaybes >>> (map fst) >>> minimum >>> subtract)

toStringNote :: Note -> GuitarString -> StringNote -> StringNote
toStringNote Nothing  _ _                      = Nothing
toStringNote (Just x) _ Nothing                = Just (7, x)
toStringNote (Just x) g (Just (fret, solfege)) = Just (mySol, x)
  where
    mySol           = fret + ((difference x solfege) - (stringToBoost g))
    stringToBoost B = 4
    stringToBoost _ = 5

difference :: Solfege -> Solfege -> Int
difference a b = semiA - semiB + (if semiA < semiB then 12 else 0)
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
