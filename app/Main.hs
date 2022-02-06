{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad.Random (RandT, StdGen, mkStdGen, runRandT)
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable (for_)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.VectorSpace (VectorSpace ((*^)), (^+^))
import Debug.Trace (trace)
import qualified Flows as F
import Generate
import Graphics.Rendering.Cairo
import qualified LSystems as L
import qualified Numeric.Noise.Perlin as P

main :: IO ()
main = do
  print "Start"
  let seed = 10
      stdGen = mkStdGen seed
      width = 1000
      height = 1000
      scaleAmount = 1

      scaledWidth = round $ fromIntegral width * scaleAmount
      scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  let world = World width height seed scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runRandT stdGen
    $ do
      cairo $ scale scaleAmount scaleAmount
      F.sketch

  putStrLn "Generating art..."
  surfaceWriteToPNG surface "out/out.png"
