module ImageViewer(viewFile) where

import Bizzlelude

import Data.Functor(($>))

import Graphics.UI.Gtk(AttrOp((:=)), containerChild, imageNewFromFile, initGUI, mainGUI, mainQuit, onDestroy, onKeyPress, set, widgetShowAll, windowNew, windowResizable)
import Graphics.UI.Gtk.Gdk.Events(Event(eventKeyChar, Key))

viewFile :: FilePath -> IO ()
viewFile path =
  do
    image  <- imageNewFromFile path
    _      <- initGUI
    window <- windowNew
    set window [containerChild := image, windowResizable := False]
    window `onKeyPress` handleKeyPress
    window `onDestroy`  mainQuit
    widgetShowAll window
    mainGUI
  where
    handleKeyPress (Key { eventKeyChar = Just 'q' }) = mainQuit $> True
    handleKeyPress _                                 = return False
