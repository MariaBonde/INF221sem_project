module BouncingBall where

import Control.Monad.Reader
import Graphics.Gloss

data BallState = Ball
  { ballPos :: (Float, Float),
    ballVel :: (Float, Float),
    ballRadius :: Float
  }

renderBalls :: Reader BallState Picture
renderBalls = do
  radius <- asks ballRadius
  (x, y) <- asks ballPos
  return $ translate x y $ color cyan $ circleSolid radius

updateBalls :: (Int, Int) -> Float -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds randomnum balls =
  balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where
    (x, y) = ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') =
      caseOfOutsideBounds x vx ((fromIntegral screenW / 2) - ballRadius balls)
    (y', vy') =
      caseOfOutsideBounds y vy ((fromIntegral screenH / 2) - ballRadius balls)
    caseOfOutsideBounds :: Float -> Float -> Float -> (Float, Float)
    caseOfOutsideBounds h dh max'
      | h' > max' = (max', -dh * randomnum)
      | h' < -max' = (-max', -dh * randomnum)
      | otherwise = (h', dh)
      where
        h' = h + seconds * dh

initialBallState :: BallState
initialBallState =
  Ball {ballPos = (500, -300), ballVel = (100, 220), ballRadius = 50}