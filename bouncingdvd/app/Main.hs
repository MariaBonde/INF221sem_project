module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.Random
import Control.Monad.Trans.State 
import HexagonFile
import DVDFile
import BouncingBall

type AppStateT m = RandT StdGen (StateT AppState m)

data Screensaver = DVDlogo | BouncingBalls | HexagonalTiles

data AppState = AppState {
  screensaver :: Maybe Screensaver, 
  dvdLogoState :: LogoState,
  hexagonalState:: HexagonalState,
  bouncingBallState :: BallState,
  screenSize :: (Int, Int)
  }

offset,fps :: Int 
background :: Color
offset = 100 
background = black
fps = 60

---------- functions for all screensavers ------------

eventFunc :: (Monad m) => Event -> AppStateT m ()
eventFunc (EventResize (w, h)) = do
    s <- lift get
    lift . put $ s {screenSize = (w, h)}
eventFunc (EventKey (Char '1') Down _ _) = lift $ modify (\s -> s {screensaver = Just DVDlogo})
eventFunc (EventKey (Char '2') Down _ _) = lift $ modify (\s -> s {screensaver = Just BouncingBalls})
eventFunc (EventKey (Char '3') Down _ _) = lift $ modify (\s -> s {screensaver = Just HexagonalTiles})
eventFunc _ = return ()


updateScreensaver :: (Monad m) => Float -> AppStateT m ()
updateScreensaver seconds = do 
  appState <- lift get 
  case screensaver appState of 
    Just DVDlogo -> lift . put $ appState {dvdLogoState = updateDVD (screenSize appState) seconds (dvdLogoState appState)}
    Just BouncingBalls -> do
      randomnum <- getRandomR (0.7, 1.2)
      lift . put $ appState {bouncingBallState = updateBalls (screenSize appState) seconds randomnum (bouncingBallState appState)}  
    Just HexagonalTiles -> do
      lift . put $ appState {hexagonalState = updateHexState seconds (hexagonalState appState)}
    Nothing -> lift . put $ appState {screensaver = Nothing}

renderNothing :: (Int, Int) -> Maybe Screensaver -> Picture 
renderNothing (w,_) _ = scale 0.5 0.5 $ translate (-fromIntegral w+20) 0 $ color white $ Text "No Screensaver Selected, press 1, 2 or 3"

renderScreensaver :: (Monad m) => AppStateT m Picture 
renderScreensaver = do
  appState <- lift get 
  case screensaver appState of 
    Just DVDlogo -> return $ renderDVD (dvdLogoState appState)
    Just BouncingBalls -> return $ renderBalls (bouncingBallState appState)
    Just HexagonalTiles -> return $ hexagonGrid (screenSize appState) (hexagonalState appState)
    Nothing -> return $ renderNothing (screenSize appState) (Nothing :: Maybe Screensaver)

main :: IO ()
main = do
    (width, height) <- getScreenSize
    let window = InWindow "Screensaver" (width, height) (offset, offset)
    let initialState = AppState (Nothing) initialLogoState initialHexState initialBallState (width, height)
    gen <- newStdGen
    play window background fps (initialState, gen)
      (\(state',_) -> evalState (evalRandT renderScreensaver gen) state')
      (\event (state'', gen'') -> let ((_, gen'), state') = runState (runRandT (eventFunc event) gen'') state''
                            in (state', gen'))
      (\seconds (state'', gen'') -> let ((_, gen'), state') = runState (runRandT (updateScreensaver seconds) gen'') state''
                            in (state', gen'))