module Main where

import RIO
import qualified RIO.Text as T
import qualified Data.Text.IO as T
import Data.List (intersperse,repeat,cycle)
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Numeric.Sampling

platitudes :: [T.Text]
platitudes = [
    "Proud/pride"
  , "Uncharted territory"
  , "Resilience/ resilient"
  , "Alludes to 2016 election"
  , "Fortitude"
  , "Literary reference"
  , "Benefits of a Brown Education"
  , "Robust network of alumni" 
  , "Brown's support for students"
  , "Historic moment"
  ]

grid :: Int -> Diagram B
grid n = vlines <> hlines <> background
  where
    vlines = center $ vsep 1 $ take (n+1) $ repeat $ hrule (fromIntegral n)
    hlines = center $ hsep 1 $ take (n+1) $ repeat $ vrule (fromIntegral n)
    background = square $ fromIntegral (n+2)

bingo :: [T.Text] -> Diagram B
bingo squaretexts = (grid 5) <> renderedtexts
  where
    points = [p2 (i, j) | i <- [-2..2], j <- [-2..2]]
    renderedtexts = position $ zip points $ map (fontSize 3 . text . T.unpack) $ cycle squaretexts

main :: IO ()
main = mainWith $ bingo platitudes
