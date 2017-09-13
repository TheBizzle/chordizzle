module ImageViewer(viewFile) where

import Bizzlelude

import Data.IORef(modifyIORef, newIORef, readIORef)
import Data.List(filter, head, init, last, tail)
import Data.Functor(($>))

import qualified Data.Text as Text

import Graphics.UI.Gtk(AttrOp((:=)), castToImage, containerChild, get, imageNew, imageNewFromFile, imageSetFromFile, initGUI, mainGUI, mainQuit, onDestroy, onKeyPress, set, widgetShowAll, widgetDestroy, windowNew, windowResizable)
import Graphics.UI.Gtk.Gdk.Events(Event(eventKeyName, Key))

import System.Directory(doesFileExist, getDirectoryContents)

viewFile :: FilePath -> IO ()
viewFile filepath =
  do

    _      <- initGUI
    window <- windowNew
    image  <- imageNew
    set window [containerChild := image, windowResizable := False]

    --                   contents                  absolutes                               just files                   just SVGs
    svgs <- filepath |> (getDirectoryContents >=> ((map $ \x -> filepath <> "/" <> x) >>> (filterM doesFileExist)) <$> (filter $ asText >>> (Text.isSuffixOf ".svg")))
    let fileZipper = fromList svgs
    imageSetFromFile image $ focus fileZipper
    ior <- newIORef fileZipper

    window `onKeyPress` (handleKeyPress image ior)
    window `onDestroy`  mainQuit

    widgetShowAll window
    mainGUI

  where
    handleKeyPress     _   _ (Key { eventKeyName = "Escape" }) = mainQuit $> True
    handleKeyPress image ior (Key { eventKeyName =   "Left" }) = cycleZipper image ior leftwards
    handleKeyPress image ior (Key { eventKeyName =  "Right" }) = cycleZipper image ior rightwards
    handleKeyPress    _    _                                 _ = return False
    cycleZipper image ior direction = (modifyIORef ior direction) >> ((focus <$> (readIORef ior)) >>= (imageSetFromFile image)) $> False

data CircularZipper a
  = CircularZipper {
      lefties  :: [a]
    , focus    ::  a
    , righties :: [a]
    }

fromList :: [a] -> CircularZipper a
fromList (h:t) = CircularZipper [] h t
fromList _     = error "Cannot make zipper from empty list"

leftwards :: CircularZipper a -> CircularZipper a
leftwards cz@(CircularZipper    [] x     []) = cz
leftwards    (CircularZipper    [] x rights) = CircularZipper (x:(init rights)) (last rights) []
leftwards    (CircularZipper lefts x rights) = CircularZipper (   init lefts  ) (last  lefts) (x:rights)

rightwards :: CircularZipper a -> CircularZipper a
rightwards cz@(CircularZipper    [] x     []) = cz
rightwards    (CircularZipper lefts x     []) = CircularZipper             [] (head  lefts) ((tail  lefts) <> [x])
rightwards    (CircularZipper lefts x rights) = CircularZipper (lefts <> [x]) (head rights) ( tail rights        )
