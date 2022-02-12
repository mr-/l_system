{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Flows where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Random (MonadRandom (getRandomR), RandT, StdGen, mkStdGen, runRandT, uniform)
import Control.Monad.Reader
import Control.Parallel.Strategies (NFData, parMap, rdeepseq, rparWith)
import Data.Colour
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB
import Data.Foldable (for_)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.VectorSpace (VectorSpace ((*^)), (^+^))
import Debug.Trace (trace)
import GHC.Generics
import Generate
import Graphics.Rendering.Cairo hiding (Matrix, scale)
import Numeric.LinearAlgebra hiding (scale)
import qualified Numeric.Noise.Perlin as P
import Prelude hiding ((<>))

type Flow = Vector Double -> Vector Double

type Color = (Double, Double, Double, Double)

data Particle = P {pCoords :: Vector Double, pColor :: Color, pSize :: Double} deriving (Show, Generic)

instance NFData Particle where rnf = genericRnf

screen = (-1, 1)

nrOfParticles = 2000

nrOfIterations = 5000

-- possibly don't use these coordinates, but move them to a grid?
flow :: Flow
flow v =
  let [x, y] = toList v
      phi = 2 * sin (x * y)
      flowed = (squeeze 1 <> rot phi) #> v
      normed = flowed / scalar (norm_2 flowed)
   in normed

flow2 :: Flow
flow2 v =
  let [x, y] = toList v
      phi = 2 * sin (exp (x * y))
      flowed = (squeeze 1 <> rot phi) #> v
      normed = flowed / scalar (norm_2 flowed)
   in normed

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

-- moveParticles field = parMap (rparWith rdeepseq) (moveParticle field)

teaGreen' :: Color
teaGreen' = (81, 0.25, 0.94, 0.5)

vividTangerine' :: Color
vividTangerine' = (11, 0.40, 0.92, 0.5)

englishVermillion' :: Color
englishVermillion' = (355, 0.68, 0.84, 0.5)

darkGunmetal' :: Color
darkGunmetal' = (170, 0.30, 0.16, 0.5)

noise :: Vector Double -> Generate Double
noise v = do
  perlinSeed <- fromIntegral <$> asks worldSeed
  let [x, y] = toList v
      perlinOctaves = 5
      perlinScale = 0.1
      perlinPersistance = 0.5
      perlinNoise =
        P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
      res = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5

  return res

byNoise :: Vector Double -> [a] -> Generate a
byNoise v options =
  do
    n <- noise v
    let i = round (n * fromIntegral (length options)) `mod` length options

    return $ options !! i

initParticle :: Generate Particle
initParticle = do
  (width, height) <- getSize
  x <- getRandomR screen
  y <- getRandomR screen
  let coords = vector [x, y]
      palette =
        [ teaGreen',
          vividTangerine',
          englishVermillion',
          darkGunmetal'
        ]
  color <- byNoise coords palette
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
