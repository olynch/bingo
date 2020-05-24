module Main where

import RIO
import System.Process
import System.Directory
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified RIO.Text as T
import qualified Data.Text.IO as T
import Data.List (intersperse,repeat,cycle,foldl1)
import Data.Matrix
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Graphicx
import qualified Data.Vector as V

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
  , "Typically"
  , "Acknowledge"
  , "Journey"
  , "Potential"
  , "Providence"
  , "Recognize"
  , "Accomplishment"
  , "Now more than ever"
  , "The current context"
  , "Incredible"
  , "Unique"
  , "Very unique"
  , "Unprecedented"
  , "Brunonian"
  , "What should have been"
  , "Wherever you may be"
  , "Radically different"
  , "As a community"
  , "Adapt"
  , "Transform"
  , "Expectations"
  , "2021"
  , "Alludes to 2020 election"
  , "Class of 2020"
  , "Prepared"
  , "Thanks faculty"
  , "Thanks parents"
  , "Open curriculum"
  , "Global pandemic"
  , "Forefront of research"
  , "Forefront of teaching"
  , "Scholars"
  , "Namedrops famous alumni"
  , "COVID-19"
  , "Van-Winckle Gate"
  , "Hearts and Souls"
  ]

sample :: (PrimMonad m) => Int -> [a] -> Gen (PrimState m) -> m [a]
sample n xs gen = do
  let v = V.fromList xs
  shuffled <- uniformShuffle v gen
  return $ take n $ V.toList shuffled

main :: IO ()
main = do
  gen <- createSystemRandom
  setCurrentDirectory "cards"
  forM_ [1..50] $ \i -> do
    cells1 <- sample 24 platitudes gen
    cells2 <- sample 24 platitudes gen
    let file = "card" ++ (show i) ++ ".tex"
    execLaTeXT (bingo cells1 cells2) >>= renderFile file
    callProcess "xelatex" [file]

bingo :: Monad m => [T.Text] -> [T.Text] -> LaTeXT m ()
bingo cells1 cells2 = do
  documentclass [] "article"
  usepackage [] "fontspec"
  usepackage ["margin=0.5cm"] "geometry"
  usepackage [] "tikz"
  usepackage [] "graphicx"
  fromLaTeX $ TeXComm "setmainfont" [ FixArg "DejaVu Sans", OptArg $ TeXRaw "SizeFeatures={Size=9}"]
  document $ do
    centering
    fromLaTeX $ TeXComm "parbox" [OptArg "c", OptArg "0cm", OptArg "c", FixArg "0cm",
                                  FixArg $ includegraphics [IGWidth (Cm 14.6), IGHeight (Cm 12.5)] "../brownbingo" ]
    let withcenter1 = (take 12 cells1) ++ ["Brown"] ++ (drop 12 cells1)
    plainMatrix $ fromList 5 5 $ withcenter1
    lnbk
    vspace (Cm 1)
    fromLaTeX $ TeXComm "parbox" [OptArg "c", OptArg "0cm", OptArg "c", FixArg "0cm",
                                  FixArg $ includegraphics [IGWidth (Cm 14.6), IGHeight (Cm 12.5)] "../brownbingo" ]
    let withcenter2 = (take 12 cells2) ++ ["Brown"] ++ (drop 12 cells2)
    plainMatrix $ fromList 5 5 $ withcenter2

plainMatrix :: (LaTeXC l, Texy a) => Matrix a -> l
plainMatrix m = tabular Nothing spec $ mconcat
  [ hline
  , mconcat $ fmap (
      \i -> mconcat [ foldl1 (Text.LaTeX.&) $ fmap (\j -> boxify $ texy (m ! (i,j))) [1 .. ncols m]
                    , lnbk
                    , hline
                    ]) [1..nrows m]
  ]
  where
    spec = VerticalLine : intersperse VerticalLine (replicate (ncols m) LeftColumn) ++ [VerticalLine]
    boxify x = fromLaTeX $
      TeXComm "parbox" [ OptArg "c", OptArg "2.5cm", OptArg "c", FixArg "2.5cm", FixArg $ center x ]
