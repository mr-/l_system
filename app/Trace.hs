{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flows where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Monad.Random (MonadRandom (getRandom, getRandomR), RandT, StdGen, mkStdGen, runRandT)
import Control.Monad.Reader
import Control.Parallel.Strategies (NFData, parMap, rdeepseq, rparWith)
import Data.Colour
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.SRGB
import Data.Foldable (for_)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.Random (RVar, RVarT, runRVar, uniform)
import Data.Random.Distribution.Triangular (floatingTriangular)
import Data.Random.Distribution.Uniform (doubleUniform, floatUniform)
import qualified Data.Set as S
import Data.VectorSpace (VectorSpace ((*^)), (^+^))
import Data.Word (Word32)
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

nrOfParticles = 20

nrOfIterations = 20

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

-- https://stackoverflow.com/questions/13936502/sampling-a-normal-distribution-using-control-monad-monadrandom?rq=1
random :: forall m s. (MonadRandom m) => RVar s -> m s
random f = do
  r <- runRVar f (getRandom :: m Word32)
  let x = 1
  -- r <- runRVar (floatingTriangular a (a + (b - a) / 2) b) (getRandom :: m Word32)
  return r

initParticle :: Generate Particle
initParticle = do
  let (w, h) = screen
      -- distribution = doubleUniform w h
      distribution = floatingTriangular w (w + (h - w) / 2) h
  x <- random distribution
  y <- random distribution
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

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ a = return [a]
iterateM n f a = do
  r <- f a
  (a :) <$> iterateM (n -1) f r

particleTraces :: Generate [[Particle]]
particleTraces = do
  initial <- initParticles
  traces <- last <$> iterateM nrOfIterations (moveParticles flow) (map (\x -> Cont [x]) initial)
  return $ map fromTrace traces

moveParticle :: Flow -> Particle -> Generate Particle
moveParticle flow particle = do
  r <- getRandomR (0.05, 0.1)
  let nextCoords = pCoords particle + scale r #> flow (pCoords particle)
      nextParticle = particle {pCoords = nextCoords}
  return nextParticle

data Trace = Cont [Particle] | Stop [Particle]

coordSet :: [[Particle]] -> S.Set (Vector Double)
coordSet =
  foldr (\x -> S.union (S.fromList $ map pCoords x)) S.empty

fromTrace :: Trace -> [Particle]
fromTrace (Cont t) = t
fromTrace (Stop t) = t

dist :: Vector Double -> Vector Double -> Double
dist v w = norm_2 (v - w)

advanceTrace :: Flow -> S.Set (Vector Double) -> Trace -> Generate Trace
advanceTrace field _ s@(Stop t) = return s
advanceTrace field cSet (Cont t@(x : xs)) = do
  n <- moveParticle field x
  let toCompare = cSet `S.difference` coordSet [t]
      close = S.filter (\h -> dist (pCoords n) h < (2*40/1000)) cSet
      stop = not $ S.null close
  return $ if stop then Stop (n : t) else Cont (n : t)
advanceTrace _ _ _ = undefined

moveParticles :: Flow -> [Trace] -> Generate [Trace]
moveParticles field traces =
  let cSet = coordSet (map fromTrace traces)
   in mapM (advanceTrace field cSet) traces

-- moveParticles field = parMap (rparWith rdeepseq) (moveParticle field)

drawParticle :: Particle -> Generate ()
drawParticle particle = do
  (width, height) <- getSize
  let [mx, my] = toList $ pCoords particle
      (x, y) = (tra width screen mx, tra height screen my)
      (r, g, b, a) = pColor particle
  -- cairo $ setSourceRGBA r g b a
  cairo $ setSourceRGBA 0 0 0 0.2
  cairo $ arc x y (40) 0 (2 * pi)
  cairo fill

drawScene :: Generate ()
drawScene = do
  ps <- concat <$> particleTraces
  forM_ ps drawParticle

sketch :: Generate ()
sketch = do
  cairo bg
  drawScene

tra :: Double -> (Double, Double) -> Double -> Double
tra width (xMin, xMax) x = (x - xMin) / (xMax - xMin) * width
