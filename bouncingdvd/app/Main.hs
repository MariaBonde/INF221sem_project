module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Control.Monad.State 
import Control.Monad.Random


data Screensaver = DVDlogo | BouncingBalls | Fibonacci

data AppState = AppState {
  screensaver :: Maybe Screensaver, 
  dvdLogoState :: LogoState,
  fibVisState :: FibVisState,
  bouncingBallState :: BallState,
  screenSize :: (Int, Int)
  }

offset,fps :: Int 
background :: Color
offset = 100 
background = black 
fps = 60

----------------- DVD Logo ------------------
colors :: [FilePath]
colors = ["app/logos/04dvdlogo-red.png", "app/logos/09dvdlogo-lightblue.png", "app/logos/02dvdlogo-green.png", "app/logos/03dvdlogo-pink.png", "app/logos/05dvdlogo-yellow.png", "app/logos/01dvdlogo-purple.png", "app/logos/08dvdlogo-orange.png", "app/logos/07dvdlogo-darkblue.png"]


data LogoState = Logo {
  logoPos :: (Float, Float), 
  logoVel :: (Float, Float),
  logoWidth :: Float, logoHeight :: Float,
  fileNum:: Int,
  logoFile :: FilePath} 

initialLogoState :: LogoState
initialLogoState = Logo {logoPos = (0, 0), logoVel = (100,100), logoWidth = 185.0, logoHeight = 82.0, fileNum = 0, logoFile ="app/logos/04dvdlogo-red.png"}

nextColor :: LogoState -> LogoState
nextColor logo = logo {fileNum = num', logoFile = colors !! num'} 
  where 
    num = fileNum logo
    num' = if num < length colors - 1 then num + 1 else 0

png :: FilePath -> Picture
png fname = maybe (color white(text "PNG ERROR")) id (unsafePerformIO $ loadJuicyPNG fname)

renderDVD :: LogoState -> Picture 
renderDVD logo = translate x y $ png (logoFile logo)
  where 
    (x, y) = logoPos logo

updateDVD :: (Int, Int) -> Float -> LogoState -> LogoState
updateDVD s seconds = changeDir s . moveLogo seconds  

moveLogo :: Float -> LogoState -> LogoState
moveLogo seconds logo = logo { logoPos = (x', y')}
  where 
    (x,y) = logoPos logo
    (vx,vy) = logoVel logo

    x' = x + vx * seconds
    y' = y + vy * seconds

changeDir :: (Int, Int) -> LogoState -> LogoState
changeDir s logo = newState {logoVel = (vx', vy')}
  where  
    (width, height) = s
    logofile = logoFile logo
    (x, y) = logoPos logo
    (vx, vy) = logoVel logo
    vx' = if withinBounds x width (logoWidth logo) --x + (logoWidth logo)/2 >= fromIntegral width / 2 || x <= -fromIntegral width / 2 || x - (logoWidth logo)/2 <= -fromIntegral width / 2 
      then 
        --update the velocity
        -vx
        else 
          vx
    vy' = if withinBounds y height (logoHeight logo) --y + (logoHeight logo)/2 >= fromIntegral height / 2 || y <= -fromIntegral height / 2  || y - (logoHeight logo)/2 <= -fromIntegral height / 2
      then 
        --update the velocity 
        -vy
        else 
          vy 
    newState = if (vx' /= vx || vy' /= vy) then nextColor logo else logo {logoFile = logofile}

--Code retrieved from https://hackage.haskell.org/package/gloss-game-0.3.3.0/docs/src/Graphics-Gloss-Game.html#png because importing it did not work

---------- Fibonacci Visualization ------------
{-
data FibVisState = FibVis {
  cPos :: (Float, Float), 
  rPos :: (Float, Float), 
  count :: Int
}

updateFibVis :: Float -> FibVisState -> FibVisState
updateFibVis seconds state = 
  let 
    n = count state 
    x = seconds * fromIntegral (memoized_fib n)
    circleX = x * cos (pi/3)
    circleY = x * sin (pi/3)
    rectX = -x * cos (pi/3)
    rectY = -x * sin (pi/3)
  in 
    state {cPos = (circleX, circleY), rPos = (rectX, rectY), count = n+1}
-}
-- Add a new field `timeAcc` to store the accumulated time
data FibVisState = FibVis
  { cPos :: (Float, Float)
  , rPos :: (Float, Float)
  , count :: Int
  , timeAcc :: Float
  }

updateFibVis :: (Int, Int) -> Float -> FibVisState -> FibVisState
updateFibVis s seconds fibstate =
  let
    updateTimeThreshold = 2 -- desired time threshold in seconds
    newTimeAcc = timeAcc fibstate + seconds
    --(width, height) = s
  in
    if newTimeAcc >= updateTimeThreshold
      then
        let
          n = count fibstate
          x = fromIntegral (memoized_fib n)
          circleX =  -x * cos (pi / 3)--if withinBounds (x * cos (pi / 3)) width 100 then x * cos (pi / 3) else -x * cos (pi / 3)
          circleY =  -x * sin (pi / 3)--if withinBounds (x * sin (pi / 3)) height 100 then x * sin (pi / 3) else -x * sin (pi / 3)
          rectX = -x * cos (pi / 3)--if withinBounds (-x * cos (pi / 3)) width 100 then x * cos (pi / 3) else -x * cos (pi / 3)
          rectY = -x * sin (pi / 3)-- if withinBounds (-x * sin (pi / 3)) height 100 then x * sin (pi / 3) else -x * sin (pi / 3)
        in
          fibstate {cPos = (circleX, circleY), rPos = (rectX, rectY), count = n + 1, timeAcc = 0}
      else
        fibstate {timeAcc = newTimeAcc}


withinBounds :: Float -> Int -> Float -> Bool
withinBounds pos screenDim objectDim = 
  pos + objectDim/2 >= fromIntegral screenDim/2 ||
  pos <= -fromIntegral screenDim/2 ||
  pos - objectDim/2 <= -fromIntegral screenDim/2


renderFibVis :: FibVisState -> Picture
renderFibVis fibstate = Pictures
  [translate cx cy $ color red $ circleSolid 50, 
  translate px py $ color blue $ rectangleSolid 100 100]
    where 
     (cx, cy) = cPos fibstate
     (px, py) = rPos fibstate

-- https://wiki.haskell.org/Memoizations
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

initialFibState :: FibVisState
initialFibState = FibVis {cPos = (-100, 0), rPos = (100, 0), count = 0, timeAcc = 0.0}
--------- Bouncing Balls ----------------
data BallState = Ball {
  ballPos :: (Float, Float),
  ballVel :: (Float, Float),
  ballRadius :: Float,
  ballColor :: Color
}

renderBalls :: BallState -> Picture
renderBalls balls = translate x y $ color cyan $ circleSolid (ballRadius balls)
  where 
    (x, y) = ballPos balls

updateBalls :: (Int, Int) -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds balls = balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where 
    (x, y) = ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') = clip x vx (((fromIntegral screenW)/2)-(ballRadius balls))
    (y', vy') = clip y vy (((fromIntegral screenH)/2)-(ballRadius balls))
    clip h dh max
          | h' > max = (max, -dh)
          | h' < -max= (-max, -dh)
          | otherwise = (h', dh)
          where h' = h + seconds*dh -- adjust gravity based on distance from center of screen
--beveger seg saktere jo nærmere høyre siden, altså jo større x er.
  


initialBallState :: BallState
initialBallState = Ball {ballPos = (500, -300), ballVel = (100, 220), ballRadius = 50, ballColor = red}


---------- functions for all screensavers ------------

eventFunc :: Event -> State AppState ()
eventFunc (EventResize (w, h))  = modify (\s -> s {screenSize = (w, h)})
eventFunc (EventKey (Char '1') Down _ _) = modify (\s -> s {screensaver = Just DVDlogo})
eventFunc (EventKey (Char '2') Down _ _) = modify (\s -> s {screensaver = Just BouncingBalls})
eventFunc (EventKey (Char '3') Down _ _) = modify (\s -> s {screensaver = Just Fibonacci})
eventFunc _ = return ()

--element av tilfeldighet, monad random, monad transformer med state og random kanskje? mulig stacke oppå
-- dvd logoen som ikke beveger seg riktig, random på veiene. 
updateScreensaver :: Float -> State AppState ()
updateScreensaver seconds = do 
  appState <- get 
  case screensaver appState of 
    Just DVDlogo -> put $ appState {dvdLogoState = updateDVD (screenSize appState) seconds (dvdLogoState appState)}
    Just BouncingBalls -> put $ appState {bouncingBallState = updateBalls (screenSize appState) seconds (bouncingBallState appState)}
    Just Fibonacci -> put $ appState {fibVisState = updateFibVis (screenSize appState) seconds (fibVisState appState)}
    Nothing -> put $ appState {screensaver = Nothing}

renderNothing :: Maybe Screensaver -> Picture 
renderNothing _ = color white(Text "No Screensaver Selected, press 1, 2 or 3")

renderScreensaver :: State AppState Picture 
renderScreensaver = do
  appState <- get 
  case screensaver appState of 
    Just DVDlogo -> return $ renderDVD (dvdLogoState appState)
    Just BouncingBalls -> return $ renderBalls (bouncingBallState appState)
    Just Fibonacci -> return $ renderFibVis (fibVisState appState)
    Nothing -> return $ renderNothing (Nothing :: Maybe Screensaver)

main :: IO ()
main = do 
    (width, height) <- getScreenSize
    let window = InWindow "Screensaver" (width, height) (offset, offset)
    let initialState = AppState (Just DVDlogo) initialLogoState initialFibState initialBallState (width, height)
    play window background fps initialState (evalState renderScreensaver) (execState . eventFunc) (execState . updateScreensaver)
{-  
changeDir :: LogoState -> LogoState
changeDir logo = 
  let (x, y) = logoPos logo
      (vx, vy) = logoVel logo
      (width, height) = screenSize logo
      logofile = logoFile logo
      vx' = if x + (logoWidth logo)*0.5 >= fromIntegral width / 2 || x <= -fromIntegral width / 2 || x - (logoWidth logo)*0.5 <= -fromIntegral width / 2 
              then 
                -- update the velocity
                let newVx = -vx
                in trace ("X direction changed! New position: " ++ show x ++", " ++ show y) newVx
              else 
                vx
      vy' = if y + (logoHeight logo)*0.5 >= fromIntegral height / 2 || y <= -fromIntegral height / 2 || y - (logoHeight logo)*0.5 <= -fromIntegral height / 2
              then 
                -- update the velocity 
                let newVy = -vy
                in trace ("Y direction changed! position: " ++ show x ++", " ++ show y) newVy
              else 
                vy
      newState = if (vx' /= vx || vy' /= vy) then let file = nextColor logo in trace ("new color"++ show (fileNum logo)) file else logo {logoFile = logofile} 
  in newState {logoVel = (vx', vy')}

updateBalls :: (Int, Int) -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds balls = balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where 
    (x, y) = ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') = clip x vx (((fromIntegral screenW)/2)-(ballRadius balls))
    (y', vy') = clip y vy (((fromIntegral screenH)/2)-(ballRadius balls))
    clip h dh max'
      | h' > max' = (max', -dh)
      | h' < -max' = (-max', -dh)
      | otherwise = (h', dh)
      where h' = h + seconds *dh

updateBalls :: (Int, Int) -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds balls = balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where 
    (x, y) = ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') = clip x vx (((fromIntegral screenW)/2)-(ballRadius balls))
    (y', vy') = clip y vy (((fromIntegral screenH)/2)-(ballRadius balls))
    clip h dh max'
      | h' > max' = (max', -dh)
      | h' < -max' = (-max', -dh*energy_loss)
      | otherwise = (h', dh+gravity)
      where 
        h' = h + seconds *dh
        energy_loss = 0.95
        gravity = -0.005



bunke = {'A':[], 'B':['1',2,3], 'C':[4,5,6]}
bunke.txt:
  [A : ],["B","1 2 3"],[C : [4 5 6]]

"1 2 3".split(" ")
verdier = ["1", "2", "3"]
dict['B'] = verdier
-}  