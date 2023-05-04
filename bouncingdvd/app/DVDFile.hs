module DVDFile where
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe
dvdColors :: [FilePath]
dvdColors = ["app/logos/04dvdlogo-red.png", "app/logos/09dvdlogo-lightblue.png", "app/logos/02dvdlogo-green.png", "app/logos/03dvdlogo-pink.png", "app/logos/05dvdlogo-yellow.png", "app/logos/01dvdlogo-purple.png", "app/logos/08dvdlogo-orange.png", "app/logos/07dvdlogo-darkblue.png"]


data LogoState = Logo {
  logoPos :: (Float, Float), 
  logoVel :: (Float, Float),
  logoWidth :: Float, logoHeight :: Float,
  fileNum:: Int,
  logoFile :: FilePath} 

initialLogoState :: LogoState
initialLogoState = Logo {logoPos = (0, 0), logoVel = (100,100), logoWidth = 185.0, logoHeight = 82.0, fileNum = 0, logoFile ="app/logos/04dvdlogo-red.png"}

nextColor :: LogoState -> LogoState
nextColor logo = logo {fileNum = num', logoFile = dvdColors !! num'} 
  where 
    num = fileNum logo
    num' = if num < length dvdColors - 1 then num + 1 else 0


--Code retrieved from https://hackage.haskell.org/package/gloss-game-0.3.3.0/docs/src/Graphics-Gloss-Game.html#png because importing it did not work
png :: FilePath -> Picture
png fname = maybe (color white(text "ERROR")) id (unsafePerformIO $ loadJuicyPNG fname)

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
    vx' = if withinBounds x width (logoWidth logo) 
      then 
        --update the velocity
        -vx
        else 
          vx
    vy' = if withinBounds y height (logoHeight logo) 
      then 
        --update the velocity 
        -vy
        else 
          vy 
    newState = if (vx' /= vx || vy' /= vy) then nextColor logo else logo {logoFile = logofile}

withinBounds :: Float -> Int -> Float -> Bool
withinBounds pos screenDim objectDim = 
  pos + objectDim/2 >= fromIntegral screenDim/2 ||
  pos <= -fromIntegral screenDim/2 ||
  pos - objectDim/2 <= -fromIntegral screenDim/2