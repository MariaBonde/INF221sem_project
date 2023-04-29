module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Graphics.Gloss.Juicy
import System.IO.Unsafe
--import Control.Monad.State 
import Control.Monad.Random
import System.Random.TF
import Control.Monad.Trans.State 

--element av tilfeldighet, monad random, monad transformer med state og random kanskje? mulig stacke oppå
-- dvd logoen som ikke beveger seg riktig, random på veiene. 

--type AppStateT m = StateT AppState (RandT StdGen m)
type AppStateT m = RandT StdGen (StateT AppState m)
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

changeDir :: (Int, Int)-> LogoState -> LogoState
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

updateFibVis :: (Int, Int) -> Float -> Float -> FibVisState -> FibVisState
updateFibVis s seconds randomNum fibstate = fibstate {cPos = (cx', cy'), rPos = (rx', ry')}
  where 
    (cx, cy) = cPos fibstate
    (rx, ry) = rPos fibstate
    (width, height) = s
    cx' = if withinBounds (cx + seconds * randomNum) width 50 then randomNum else cx - seconds * randomNum
    cy' = if withinBounds (cy + seconds * randomNum) height 50 then randomNum else cy - seconds * randomNum
    rx' = if withinBounds (rx + seconds * randomNum) width 100 then randomNum else rx - seconds * randomNum
    ry' = if withinBounds (ry + seconds * randomNum) height 100 then randomNum else ry - seconds * randomNum
    
updateFib :: (Int, Int) -> Float -> Float -> FibVisState -> FibVisState
updateFib s seconds randomNum fibstate = fibstate


withinBounds :: Float -> Int -> Float -> Bool
withinBounds pos screenDim objectDim = 
  pos + objectDim/2 >= fromIntegral screenDim/2 ||
  pos <= -fromIntegral screenDim/2 ||
  pos - objectDim/2 <= -fromIntegral screenDim/2

hexagon :: Float -> Picture
hexagon sideLength = polygon vertices
  where
    angleStep = 2 * pi / 6
    angles = [i * angleStep | i <- [0..5]]
    vertices = [(sideLength * cos angle, sideLength * sin angle) | angle <- angles]

hexagonGrid :: FibVisState -> Picture
hexagonGrid fib = pictures
    [ translate (xOffset + x * 1.5 * sideLength) (yOffset + y * verticalSpacing) $
      color (if even $ round (x + y) then blue else red) $
      hexagon sideLength
    | x <- [0..cols - 1], y <- [0..rows - 1] ]
  where
    rows = 100
    cols = 100
    sideLength = 50
    yOffset = -rows * sideLength * 0.5
    xOffset = -cols * sideLength * 0.75
    verticalSpacing = sqrt 3 * sideLength

renderFibVis :: FibVisState -> Picture
renderFibVis fibstate = Pictures 
  [translate cx cy $ color red $ hexagon 50,
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

updateBalls :: (Int, Int) -> Float -> Float -> BallState -> BallState
updateBalls (screenW, screenH) seconds randomnum balls = balls {ballPos = (x', y'), ballVel = (vx', vy')}
  where 
    (x, y) = ballPos balls
    (vx, vy) = ballVel balls
    (x', vx') = clip x vx (((fromIntegral screenW)/2)-(ballRadius balls))
    (y', vy') = clip y vy (((fromIntegral screenH)/2)-(ballRadius balls))
    clip h dh max
          | h' > max = (max, -dh*randomnum)
          | h' < -max= (-max, -dh*randomnum)
          | otherwise = (h', dh)
          where h' = h + seconds*dh -- adjust gravity based on distance from center of screen
--beveger seg saktere jo nærmere høyre siden, altså jo større x er.
  


initialBallState :: BallState
initialBallState = Ball {ballPos = (500, -300), ballVel = (100, 220), ballRadius = 50, ballColor = red}


---------- functions for all screensavers ------------

eventFunc :: (Monad m) => Event -> AppStateT m ()
eventFunc (EventResize (w, h)) = do
    s <- lift get
    lift . put $ s {screenSize = (w, h)}
eventFunc (EventKey (Char '1') Down _ _) = lift $ modify (\s -> s {screensaver = Just DVDlogo})
eventFunc (EventKey (Char '2') Down _ _) = lift $ modify (\s -> s {screensaver = Just BouncingBalls})
eventFunc (EventKey (Char '3') Down _ _) = lift $ modify (\s -> s {screensaver = Just Fibonacci})
eventFunc _ = return ()


updateScreensaver :: (Monad m) => Float -> AppStateT m ()
updateScreensaver seconds = do 
  randomNumber <- getRandomR (0.7, 1.2) :: (MonadRandom m) => m Float
  (w, h) <- screenSize <$> lift get
  randomPos <- getRandomR (-fromIntegral w/2, fromIntegral w/2) :: (MonadRandom m) => m Float
  appState <- lift get 
  case screensaver appState of 
    Just DVDlogo -> lift . put $ appState {dvdLogoState = updateDVD (screenSize appState) seconds (dvdLogoState appState)}
    Just BouncingBalls -> do
      lift . put $ appState {bouncingBallState = updateBalls (screenSize appState) seconds randomNumber (bouncingBallState appState)}
    Just Fibonacci -> lift . put $ appState {fibVisState = updateFib (screenSize appState) seconds randomPos (fibVisState appState)}
    Nothing -> lift . put $ appState {screensaver = Nothing}

renderNothing :: Maybe Screensaver -> Picture 
renderNothing _ = color white(Text "No Screensaver Selected, press 1, 2 or 3")

renderScreensaver :: (Monad m) => AppStateT m Picture 
renderScreensaver = do
  appState <- lift get 
  case screensaver appState of 
    Just DVDlogo -> return $ renderDVD (dvdLogoState appState)
    Just BouncingBalls -> return $ renderBalls (bouncingBallState appState)
    Just Fibonacci -> return $ hexagonGrid (fibVisState appState)
    Nothing -> return $ renderNothing (Nothing :: Maybe Screensaver)

runAppStateT :: AppStateT IO a -> AppState -> StdGen -> IO (a, AppState)
runAppStateT action state gen = runStateT (evalRandT action gen) state

main :: IO ()
main = do
    (width, height) <- getScreenSize
    let window = InWindow "Screensaver" (width, height) (offset, offset)
    let initialState = AppState (Just DVDlogo) initialLogoState initialFibState initialBallState (width, height)
    gen <- newStdGen

    play window background fps initialState
      (\state -> evalState (evalRandT renderScreensaver gen) state)
      (\event state -> execState (runRandT (eventFunc event) gen) state)
      (\seconds state -> execState (runRandT (updateScreensaver seconds) gen) state)

{- 

main :: IO ()
main = do 
    (width, height) <- getScreenSize
    let window = InWindow "Screensaver" (width, height) (offset, offset)
    let initialState = AppState (Just DVDlogo) initialLogoState initialFibState initialBallState (width, height)
    play window background fps initialState (evalState renderScreensaver) (execState . eventFunc) (execState . updateScreensaver)

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