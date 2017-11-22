{-# LANGUAGE TemplateHaskell #-}
module ChordData(chordData) where

import Bizzlelude

import Data.FileEmbed(embedFile)
import Data.Text.Encoding(decodeUtf8)

chordData :: Text
chordData = decodeUtf8 $(embedFile "chords.txt")
