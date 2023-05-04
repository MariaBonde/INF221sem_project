module BouncingBall where 
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Debug.Trace

data BallState = Ball {
  ballPos :: (Float, Float),
  ballVel :: (Float, Float),
  ballRadius :: Float
}

renderBalls :: BallState -> Picture
renderBalls balls = translate x y $ color cyan $ circleSolid (ballRadius balls)
  where 
    (x, y) = ballPos balls

updateBalls :: (Int, Int) -> Float -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds randomnum balls = balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where 
    (x, y) =  ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') = clip x vx (((fromIntegral screenW)/2)-(ballRadius balls))
    (y', vy') = clip y vy (((fromIntegral screenH)/2)-(ballRadius balls))
    clip h dh max
          | h' > max = (max, -dh*randomnum)
          | h' < -max= (-max, -dh*randomnum)
          | otherwise = (h', dh)
          where h' = h + seconds*dh 

initialBallState :: BallState
initialBallState = Ball {ballPos = (500, -300), ballVel = (100, 220), ballRadius = 50}
