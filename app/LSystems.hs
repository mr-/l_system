{-# LANGUAGE RecordWildCards #-}

module LSystems where

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
import Generate
import Graphics.Rendering.Cairo
import qualified Numeric.Noise.Perlin as P

drawScene :: Generate ()
drawScene = do
  (width, height) <- getSize
  pPoints <- mapM perlinPos $ doThing 7
  let points = scaleTo width height pPoints
  cairo $ renderPath points
  cairo $ englishVermillion 1 *> stroke

perlinPos :: Pos -> Generate Pos
perlinPos p@(x, y) = do
  perlinSeed <- asks (fromIntegral . worldSeed)
  let perlinOctaves = 5
      perlinScale = 2
      perlinPersistance = 0.5
      perlinNoise = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
      noise = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed)

  return $ p ^+^ (noise / 2, noise / 2)

sketch :: Generate ()
sketch = do
  cairo bg
  drawScene

type Tokens = [String]

type Token = String

type Rules = Map Token Tokens

apply :: Rules -> Token -> Tokens
apply rules token = rules Map.! token

applyAll :: Rules -> Tokens -> Tokens
applyAll rules = concatMap (apply rules)

generate :: Rules -> Token -> [Tokens]
generate rules initial = iterate (applyAll rules) [initial]

sierpinski =
  Map.fromList
    [ ("x", ["y", "x", "y"]),
      ("y", ["x", "-y", "-x"]),
      ("-x", ["-y", "x", "y"]),
      ("-y", ["-x", "-y", "-x"])
    ]

type Orientation = Int

type Pos = (Double, Double)

type Path = Map Pos Pos

data Command = Rotate Int | Advance Int deriving (Show)

toTurtle :: Tokens -> [Command]
toTurtle = concatMap (\x -> toTurtle' x : [Advance 1])
  where
    angle = 60
    toTurtle' "-x" = Rotate (- angle)
    toTurtle' "-y" = Rotate (- angle)
    toTurtle' "x" = Rotate angle
    toTurtle' "y" = Rotate angle
    toTurtle' x = error $ show ("toTurtle'", x)

toPos :: Pos -> Orientation -> [Command] -> [(Pos, Orientation)]
toPos pos orientation commands = scanl (flip toPos') (pos, orientation) commands

toPos' :: Command -> (Pos, Orientation) -> (Pos, Orientation)
toPos' (Advance i) (pos, orientation) = (pos ^+^ rot orientation, orientation)
  where
    rad phi = fromIntegral phi / 360 * 2 * pi
    rot phi = (cos $ rad phi, sin $ rad phi)
toPos' (Rotate i) (pos, orientation) = (pos, (orientation + i) `mod` 360)

doThing :: Int -> [Pos]
doThing i =
  let tokens = generate sierpinski "x" !! i
      turtles = toTurtle tokens
      path = toPos (0, 0) 0 turtles
   in map fst path

translate' :: Pos -> [Pos] -> [Pos]
translate' pos ps = map (^+^ pos) ps

scaleTo :: Double -> Double -> [Pos] -> [Pos]
scaleTo width height ps =
  let xs = map fst ps
      (minX, maxX) = (minimum xs, maximum xs)
      pWidth = maxX - minX

      ys = map snd ps
      (minY, maxY) = (minimum ys, maximum ys)
      pHeight = maxY - minY
      tr (x, y) = ((x - minX) / pWidth * width, (y - minY) / pHeight * height)
   in map tr ps
