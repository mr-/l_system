{-# LANGUAGE RecordWildCards #-}

module Generate where

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
import Graphics.Rendering.Cairo
import qualified Numeric.Noise.Perlin as P

data World = World
  { worldWidth :: Int,
    worldHeight :: Int,
    worldSeed :: Int,
    worldScale :: Double
  }

type Generate a = RandT StdGen (ReaderT World Render) a

cairo :: Render a -> Generate a
cairo = lift . lift

bg :: Render ()
bg = do
  setSourceRGBA 255 255 255 1
  rectangle 0 0 1000 1000
  fill

renderPath :: [(Double, Double)] -> Render ()
renderPath ((x, y) : vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let (x', y') = v in lineTo x' y'
renderPath [] = pure ()

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where
    RGB {..} = hsv h s v

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

getSize :: Generate (Double, Double)
getSize = do
  (width, height) <- asks (worldWidth &&& worldHeight)
  return (fromIntegral width, fromIntegral height)
