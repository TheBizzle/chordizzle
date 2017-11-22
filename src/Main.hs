{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import Bizzlelude

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Chords(buildVDMap)

import ChordData(chordData)

import Color(Color(Color))

import Data.List((!!), elemIndex, head)

import Reflex.Dom(blank, constDyn, domEvent, dyn, el, El, elAttr', elDynAttrNS', Event, EventName(Keydown), ffilter, foldDyn, KeyCode, keyCodeLookup, leftmost, mainWidget, MonadWidget, Reflex, text)

import Shape(Point(Point), Shape(Circle, Line, Rectangle), VectorDrawing(height, shapes, width))

import Web.KeyCode(Key(ArrowLeft, ArrowRight))

main :: IO ()
main =
  mainWidget $ el "div" $ mdo

    let backwardsEvent = keypressOf ArrowLeft  elem
    let forwardsEvent  = keypressOf ArrowRight elem
    dynamo <- foldDyn (cycleChord chords) (head chords) $ leftmost [forwardsEvent $> Forwards, backwardsEvent $> Backwards]

    let svgAttrs = Map.fromList [("height", fromFirstChord height), ("width", fromFirstChord width)]
    let svgChild = dyn $ fmap (snd >>> shapes >>> (flip forM renderShape)) dynamo
    let svgElem  = svg "svg" svgAttrs svgChild
    let children = svgElem >> (dyn $ fmap (fst >>> text) dynamo)
    (elem, _) <- elAttr' "div" (Map.fromList [("tabindex", "1")]) children
    blank
  where
    chords           = chordData |> (buildVDMap >>> Map.toList)
    fromFirstChord f = chords |> (head >>> snd >>> f >>> showText)


keypressOf :: Reflex t => Key -> El t -> Event t KeyCode
keypressOf key elem = ffilter (keyCodeLookup >>> (== key)) $ domEvent Keydown elem

svg :: MonadWidget t m => Text -> (Map.Map Text Text) -> m a -> m (El t)
svg elementTag attrs child = fst <$> (elDynAttrNS' (Just "http://www.w3.org/2000/svg") elementTag (constDyn attrs) child)

data Direction
  = Backwards
  | Forwards

cycleChord :: [(Text, VectorDrawing)] -> Direction -> (Text, VectorDrawing) -> (Text, VectorDrawing)
cycleChord chords dir x = chords !! newIndex
  where
    index    = fromMaybe 0 $ elemIndex x chords
    op       = case dir of Forwards  -> (+ 1)
                           Backwards -> (subtract 1)
    newIndex = ((length chords) + (op index)) `mod` (length chords)

serializeColor :: Color -> Text
serializeColor (Color (r, g, b)) = "rgb(" <> (showText r) <> "," <> (showText g) <> "," <> (showText b) <> ")"

renderShape :: MonadWidget t m => Shape -> m (El t)

renderShape (Circle (Point (cx, cy)) radius fillColor borderColor thickness) =
  svg "circle" attrs blank
    where attrs = Map.fromList [ ("cx",           showText cx)
                               , ("cy",           showText cy)
                               , ("r",            showText radius)
                               , ("stroke",       serializeColor borderColor)
                               , ("stroke-width", showText thickness)
                               , ("fill",         serializeColor fillColor)
                               ]

renderShape (Rectangle (Point (x1, y1)) (Point (x2, y2)) fillColor borderColor thickness) =
  svg "rect" attrs blank
    where attrs = Map.fromList [ ("x",            showText x1)
                               , ("y",            showText y1)
                               , ("height",       showText $ y2 - y1)
                               , ("width",        showText $ x2 - x1)
                               , ("fill",         serializeColor fillColor)
                               , ("stroke",       serializeColor borderColor)
                               , ("stroke-width", showText thickness)
                               ]

renderShape (Line (Point (x1, y1)) (Point (x2, y2)) color thickness) =
  svg "line" attrs blank
    where attrs = Map.fromList [ ("x1",           showText x1)
                               , ("y1",           showText y1)
                               , ("x2",           showText x2)
                               , ("y2",           showText y2)
                               , ("stroke",       serializeColor color)
                               , ("stroke-width", showText thickness)
                               ]
