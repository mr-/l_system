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

type Flow = Vector Double -> Vector Double

type Color = (Double, Double, Double, Double)

data Particle = P {pCoords :: Vector Double, pColor :: Color, pSize :: Double} deriving (Show)

screen = (-1.5, 1.5)

nrOfParticles = 1000

nrOfIterations = 1000

flow :: Flow
flow v =
  let [x, y] = toList v
      phi = sin (exp (x * y))
   in (squeeze 1 <> rot phi) #> v

rot :: Double -> Matrix Double
rot phi = matrix 2 [cos phi, - sin phi, sin phi, cos phi]

stretch :: Double -> Matrix Double
stretch k = matrix 2 [k, 0, 0, 1]

scale :: Double -> Matrix Double
scale k = matrix 2 [k, 0, 0, k]

squeeze :: Double -> Matrix Double
squeeze k = matrix 2 [k, 0, 0, 1 / k]

moveParticle :: Flow -> Particle -> Particle
moveParticle flow particle =
  let nextCoords = pCoords particle + scale 0.001 #> flow (pCoords particle)
      nextParticle = particle {pCoords = nextCoords}
   in nextParticle

moveParticles :: Flow -> [Particle] -> [Particle]
moveParticles field = map (moveParticle field)

teaGreen' :: Color
teaGreen' = (81, 0.25, 0.94, 0.3)

vividTangerine' :: Color
vividTangerine' = (11, 0.40, 0.92, 0.3)

englishVermillion' :: Color
englishVermillion' = (355, 0.68, 0.84, 0.3)

darkGunmetal' :: Color
darkGunmetal' = (170, 0.30, 0.16, 0.4)

initParticle :: Generate Particle
initParticle = do
  (width, height) <- getSize
  x <- getRandomR screen
  y <- getRandomR screen
  let coords = vector [x, y]
  color <-
    uniform
      [ teaGreen',
        vividTangerine',
        englishVermillion',
        darkGunmetal'
      ]
  return $ P {pCoords = coords, pSize = 0.5, pColor = color}

initParticles :: Generate [Particle]
initParticles = mapM (const initParticle) [0 .. nrOfParticles]

particles :: Generate [Particle]
particles = do
  initial <- initParticles
  let iterations = iterate (moveParticles flow) initial
  return $ concat $ take nrOfIterations iterations

drawParticle :: Particle -> Generate ()
drawParticle particle = do
  (width, height) <- getSize
  let [mx, my] = toList $ pCoords particle
      (x, y) = (tra width screen mx, tra height screen my)
      (r, g, b, a) = pColor particle
  cairo $ setSourceRGBA r g b a
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
