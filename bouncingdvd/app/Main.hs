module Main (main) where

import BouncingBall
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.State
import DVDFile
import Graphics.Gloss 
import Graphics.Gloss.Interface.Environment(getScreenSize)
import Graphics.Gloss.Interface.Pure.Game 
import Graphics.Gloss.Juicy (loadJuicyPNG)
import HexagonFile

type AppStateT m = RandT StdGen (StateT AppState m)

data Screensaver = DVDlogo | BouncingBalls | HexagonalTiles

data AppState = AppState
  { screensaver :: Maybe Screensaver,
    dvdLogoState :: LogoState,
    hexagonalState :: HexagonalState,
    bouncingBallState :: BallState,
    screenSize :: (Int, Int)
  }

offset, fps :: Int
background :: Color
offset = 100

background = black

fps = 60

---------- functions for all screensavers ------------

eventFunc :: (Monad m) => Event -> AppStateT m ()
eventFunc (EventResize (w, h)) = do
  s <- lift get
  lift . put $ s {screenSize = (w, h)}
eventFunc (EventKey (Char '1') Down _ _) = modifyState DVDlogo
eventFunc (EventKey (Char '2') Down _ _) = modifyState BouncingBalls
eventFunc (EventKey (Char '3') Down _ _) = modifyState HexagonalTiles
eventFunc _ = return ()

modifyState :: (Monad m) => Screensaver -> AppStateT m ()
modifyState chosenScreen = lift $ modify (\s -> s {screensaver = Just chosenScreen})

updateScreensaver :: (Monad m) => Float -> AppStateT m ()
updateScreensaver seconds = do
  appState <- lift get
  case screensaver appState of
    Just DVDlogo -> lift . put $ appState {dvdLogoState = execState (updateDVD (screenSize appState) seconds) (dvdLogoState appState)}
    Just BouncingBalls -> do
      randomnum <- getRandomR (0.7, 1.2)
      lift . put $ appState {bouncingBallState = updateBalls (screenSize appState) seconds randomnum (bouncingBallState appState)}
    Just HexagonalTiles -> lift . put $ appState {hexagonalState = updateHexState seconds (hexagonalState appState)}
    Nothing -> lift . put $ appState {screensaver = Nothing}

renderNothing :: (Int, Int) -> Maybe Screensaver -> Picture
renderNothing (w, _) _ = scale 0.5 0.5 $ translate (-fromIntegral w + 20) 0 $ color white $ Text "No Screensaver Selected, press 1, 2 or 3"

renderScreensaver :: (Monad m) => AppStateT m Picture
renderScreensaver = do
  appState <- lift get
  case screensaver appState of
    Just DVDlogo -> return $ runReader renderDVD (dvdLogoState appState)
    Just BouncingBalls -> return $ runReader renderBalls (bouncingBallState appState)
    Just HexagonalTiles -> return $ runReader (hexagonGrid (screenSize appState)) (hexagonalState appState)
    Nothing -> return $ renderNothing (screenSize appState) (Nothing :: Maybe Screensaver)

main :: IO ()
main = do
  (width, height) <- getScreenSize 
  images <- mapM loadJuicyPNG dvdColors -- load all images, using mapM because have monadic values applied to the Picture type
  let images' = case sequence images of
        Just imgs -> imgs
        Nothing -> [color white (text "ERROR")]
  let window = InWindow "Screensaver" (width, height) (offset, offset)
  let initialState = AppState Nothing (initialLogoState images') initialHexState initialBallState (width, height) -- start with "Nothing" as the screensaver
  gen <- newStdGen
  play
    window
    background
    fps
    (initialState, gen) -- sending both the initial state and the generator to the play function
    (\(state', _) -> evalState (evalRandT renderScreensaver gen) state') -- extract only the Picture from renderScreensaver
    ( \event (state'', gen'') ->
        let ((_, gen'), state') = runState (runRandT (eventFunc event) gen'') state'' 
         in (state', gen')
    )
    ( \seconds (state'', gen'') ->
        let ((_, gen'), state') = runState (runRandT (updateScreensaver seconds) gen'') state'' -- extract both the updated state and the generator from updateScreensaver
         in (state', gen')
    )