module Main where

import Bizzlelude

import Chords(processChords)
import ImageViewer(viewFile)

import System.Directory(createDirectoryIfMissing)

import qualified Data.Text.IO as TIO

main :: IO ()
main =
  do
    nameSVGPairs <- map processChords $ TIO.readFile "chords.txt"
    createDirectoryIfMissing True "output"
    forM_ nameSVGPairs $ (first $ \x -> asPath $ "output/" <> x <> ".svg") >>> uncurry TIO.writeFile
    viewFile $ asPath $ "output"
    TIO.putStrLn "We done did it!"
