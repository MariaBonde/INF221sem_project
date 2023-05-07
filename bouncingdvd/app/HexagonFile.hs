module HexagonFile where

import Control.Monad.Reader
import Graphics.Gloss
import Graphics.Gloss.Data.Color

data HexagonalState = Hexagon
  { currentColors :: [Color],
    currentIndex :: Int,
    timer :: Float
  }

colors :: [Color]
colors = [violet, aquamarine, chartreuse, orange, dim red, rose, magenta, violet, azure]

makeColors :: [Color] -> [Color]
makeColors [] = []
makeColors [x] = darkerColor (addColors x azure) 3 : makeColors []
makeColors (x : y : xs) = darkerColor x 2 : addColors x y : darkerColor y 3 : makeColors (y : xs)

updateHexState :: Float -> HexagonalState -> HexagonalState
updateHexState elapsedTime hexState =
  if newTimer >= newColorTime
    then
      hexState
        { currentIndex = (currentIndex hexState + 1) `mod` length newcolors,
          currentColors = take 2 $ drop (currentIndex hexState) $ cycle newcolors,
          timer = 0
        }
    else hexState {timer = newTimer}
  where
    newTimer = timer hexState + elapsedTime
    newcolors = makeColors colors
    newColorTime = 0.8

hexagon :: Float -> Picture
hexagon sideLength = polygon vertices
  where
    angleStep = 2 * pi / 6
    angles = [i * angleStep | i <- [0 .. 5]]
    vertices = [(sideLength * cos angle, sideLength * sin angle) | angle <- angles]

darkerColor :: Color -> Int -> Color
darkerColor color 0 = color
darkerColor color n = darkerColor (dim color) (n - 1)

hexagonGrid :: (Int, Int) -> Reader HexagonalState Picture
hexagonGrid (width, height) = do
  currentcolors <- asks currentColors
  return $
    pictures
      [ translate (xOffset + x * horizontalSpacing) (yOffset + y * verticalSpacing) $
          color (if even $ round (x + y) then head currentcolors else currentcolors !! 1) $
            hexagon sideLength
        | x <- [0 .. cols - 1],
          y <- [0 .. rows - 1]
      ]
  where
    rows = fromIntegral height / sideLength
    cols = fromIntegral width / sideLength
    sideLength = 50
    yOffset = -rows * sideLength * 0.5
    xOffset = -cols * sideLength * 0.75
    verticalSpacing = sqrt 3 * sideLength
    horizontalSpacing = 2 * sideLength

initialHexState :: HexagonalState
initialHexState = Hexagon {currentColors = [rose, violet], currentIndex = 1, timer = 0}