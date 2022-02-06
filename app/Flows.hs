{-# LANGUAGE RecordWildCards #-}

module Flows where

import Control.Arrow ((&&&))
import Control.Monad.Random (MonadRandom (getRandomR), RandT, StdGen, mkStdGen, runRandT, uniform)
import Control.Monad.Reader
import Data.Colour
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB
import Data.Foldable (for_)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.VectorSpace (VectorSpace ((*^)), (^+^))
import Debug.Trace (trace)
import Generate
import Graphics.Rendering.Cairo hiding (Matrix, scale)
import Numeric.LinearAlgebra hiding (scale)
import qualified Numeric.Noise.Perlin as P
import Prelude hiding ((<>))

data Particle = P {pCoords :: Vector Double, pColor :: AlphaColour Double, pSize :: Double} deriving (Show)

rot :: Double -> Matrix Double
rot phi = matrix 2 [cos phi, - sin phi, sin phi, cos phi]

stretch :: Double -> Matrix Double
stretch k = matrix 2 [k, 0, 0, 1]

scale :: Double -> Matrix Double
scale k = matrix 2 [k, 0, 0, k]

squeeze :: Double -> Matrix Double
squeeze k = matrix 2 [k, 0, 0, 1 / k]

moveParticle :: Matrix Double -> Particle -> Particle
moveParticle field particle =
  let nextCoords = pCoords particle + scale 0.001 #> field #> pCoords particle
      nextParticle = particle {pCoords = nextCoords}
   in nextParticle

moveParticles :: Matrix Double -> [Particle] -> [Particle]
moveParticles field = map (moveParticle field)

englishVermillion' :: Double -> AlphaColour Double
englishVermillion' = withOpacity (sRGB24 123 123 10)

teaGreen' :: Double -> AlphaColour Double
teaGreen' = withOpacity (sRGB24 23 23 110)

vividTangerine' :: Double -> AlphaColour Double
vividTangerine' = withOpacity (sRGB24 23 123 110)

darkGunmetal' :: Double -> AlphaColour Double
darkGunmetal' = withOpacity (sRGB24 123 123 110)

initParticle :: Generate Particle
initParticle = do
  (width, height) <- getSize
  x <- getRandomR (-1, 1)
  y <- getRandomR (-1, 1)
  let coords = vector [x, y]
  color <-
    uniform
      [ teaGreen' 0.9,
        vividTangerine' 0.9,
        englishVermillion' 0.9,
        darkGunmetal' 0.9
      ]
  return $ P {pCoords = coords, pSize = 1, pColor = color}

initParticles :: Generate [Particle]
initParticles = mapM (const initParticle) [0 .. 1500]

particles :: Generate [Particle]
particles = do
  initial <- initParticles
  let flow = squeeze 5 <> rot 30
      iterations = iterate (moveParticles flow) initial
  return $ concat $ take 3000 iterations

drawParticle :: Particle -> Generate ()
drawParticle particle = do
  (width, height) <- getSize
  let [mx, my] = toList $ pCoords particle
      (x, y) = (tra width (-1, 1) mx, tra height (-1, 1) my)
      c = pColor particle
      a = alphaChannel c
  cairo $ setSourceRGBA 1 1 0 0.2
  cairo $ arc x y (pSize particle) 0 (2 * pi)
  cairo fill

drawScene :: Generate ()
drawScene = do
  ps <- particles
  forM_ ps drawParticle

sketch :: Generate ()
sketch = do
  cairo bg
  drawScene

tra :: Double -> (Double, Double) -> Double -> Double
tra width (xMin, xMax) x = (x - xMin) / (xMax - xMin) * width
