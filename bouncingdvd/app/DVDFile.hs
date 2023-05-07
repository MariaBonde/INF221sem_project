module DVDFile where
import Graphics.Gloss
import Control.Monad.State
import Control.Monad.Reader

dvdColors :: [FilePath]
dvdColors = ["app/logos/04dvdlogo-red.png", "app/logos/09dvdlogo-lightblue.png", "app/logos/02dvdlogo-green.png", "app/logos/03dvdlogo-pink.png", "app/logos/05dvdlogo-yellow.png", "app/logos/01dvdlogo-purple.png", "app/logos/08dvdlogo-orange.png", "app/logos/07dvdlogo-darkblue.png"]

data LogoState = Logo {
  logoPos :: (Float, Float), 
  logoVel :: (Float, Float),
  logoWidth :: Float, logoHeight :: Float,
  fileNum:: Int,
  logoPictures :: [Picture],
  currentPicture :: Picture} 

initialLogoState :: [Picture] -> LogoState
initialLogoState pics = Logo {logoPos = (0, 0), logoVel = (100,100), logoWidth = 185.0, logoHeight = 82.0, fileNum = 0, logoPictures =pics, currentPicture = head pics}

updateDVD :: (Int, Int) -> Float -> State LogoState ()
updateDVD s seconds = changeDir s >> moveLogo seconds

moveLogo :: Float -> State LogoState ()
moveLogo seconds = do
  (x, y) <- gets logoPos
  (vx, vy) <- gets logoVel
  let x' = x + vx * seconds
  let y' = y + vy * seconds
  modify (\logo -> logo { logoPos = (x', y') })

changeDir :: (Int, Int) -> State LogoState ()
changeDir s = do 
  (x, y) <- gets logoPos
  (vx, vy) <- gets logoVel
  logoW <- gets logoWidth
  logoH <- gets logoHeight
  let (width, height) = s
  let vx' = if withinBounds x width logoW
      then 
        --update the velocity
        -vx
        else 
          vx
  let vy' = if withinBounds y height logoH
      then 
        --update the velocity 
        -vy
        else 
          vy 
  when (vx' /= vx || vy' /= vy) nextColor -- if the velocity has changed, change the color
  modify (\logo -> logo { logoVel = (vx', vy') })

withinBounds :: Float -> Int -> Float -> Bool
withinBounds pos screenDim objectDim = 
  pos + objectDim/2 >= fromIntegral screenDim/2 ||
  pos <= -fromIntegral screenDim/2 ||
  pos - objectDim/2 <= -fromIntegral screenDim/2

nextColor :: State LogoState ()
nextColor = do 
  num <- gets fileNum
  images <- gets logoPictures
  let num' = if num < length images - 1 then num + 1 else 0
  modify (\logo -> logo { fileNum = num', currentPicture = images !! num' })


renderDVD :: Reader LogoState Picture
renderDVD = do
  currentPic <- asks currentPicture
  (x, y) <- asks logoPos
  return $ translate x y currentPic



