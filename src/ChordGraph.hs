module ChordGraph(buildGraph, Chord(Chord)) where

import Bizzlelude

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

data Placeholder = Placeholder { chord :: Chord, neighbors :: Set Chord }

data ChordGraph = ChordGraph { formNeighbors :: Map Chord (Set Chord), voicingNeighbors :: Map Chord (Set Chord) }

data Chord = Chord { form :: Text, identifier :: Text } deriving (Eq, Ord)

buildGraph :: [Text] -> ChordGraph
buildGraph = toPlaceholders >>> toGraph

toPlaceholders :: [Text] -> [Placeholder]
toPlaceholders = flip helper []
  where
    helper []    acc = acc
    helper (h:t) acc = helper leftovers $ (Placeholder (toChord h) neighbors):acc
      where
        (chunk, leftovers) = List.span (Text.isPrefixOf "  ") t
        neighbors          = chunk |> ((map $ Text.strip >>> toChord) >>> Set.fromList)
        toChord            = (Text.breakOn "-") >>> (uncurry Chord)

toGraph :: [Placeholder] -> ChordGraph
toGraph = (toFormies &&& toVoicies) >>> (uncurry ChordGraph)
  where
    toVoicies = (map $ \(Placeholder c ns) -> (c, ns)) >>> Map.fromList
    toFormies = ((map chord) >>> (scalaGroupBy form) >>> (map funkyMap) >>> concat >>> Map.fromList)
      where
        funkyMap (_, xs) = map (id &&& (setWithoutMe xs)) xs
          where setWithoutMe xs x = xs |> (Set.fromList >>> Set.delete x)
