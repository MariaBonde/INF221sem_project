module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Data.List


offset :: Int 
offset = 100 
background = black 
fps = 600

colors = ["app/logos/04dvdlogo-red.png", "app/logos/09dvdlogo-lightblue.png", "app/logos/02dvdlogo-green.png", "app/logos/03dvdlogo-pink.png", "app/logos/05dvdlogo-yellow.png", "app/logos/01dvdlogo-purple.png", "app/logos/08dvdlogo-orange.png", "app/logos/07dvdlogo-darkblue.png"]

data LogoState = Logo {
  logoPos :: (Float, Float), 
  logoVel :: (Float, Float),
  logoWidth :: Float, logoHeight :: Float,
  screenSize ::(Int, Int),
  fileNum:: Int, 
  logoFile :: FilePath} 

nextColor :: LogoState -> LogoState
nextColor state = state {fileNum = num', logoFile = colors !! num'} 
  where 
    num = fileNum state
    num' = if num < length colors - 1 then num + 1 else 0


initialLogoState :: LogoState
initialLogoState = Logo {logoPos = (0, 0), logoVel = (100,100), logoWidth = 185.0, logoHeight = 82.0, fileNum = 0, logoFile ="app/logos/04dvdlogo-red.png"}

update :: Float -> LogoState -> LogoState
update seconds = changeDir . moveLogo seconds  

moveLogo :: Float -> LogoState -> LogoState
moveLogo seconds state = state { logoPos = (x', y')}
  where 
    (x,y) = logoPos state
    (vx,vy) = logoVel state

    x' = x + vx * seconds
    y' = y + vy * seconds



changeDir :: LogoState -> LogoState
changeDir logo = newState {logoVel = (vx', vy')}
  where  
    (width, height) = screenSize logo
    logofile = logoFile logo
    (x, y) = logoPos logo
    (vx, vy) = logoVel logo
    vx' = if x + (logoWidth logo)/2 >= fromIntegral width / 2 || x <= -fromIntegral width / 2 ||x - (logoWidth logo)/2 <= -fromIntegral width / 2 
      then 
        --update the velocity
        -vx
        else 
          vx
    vy' = if y + (logoHeight logo)/2 >= fromIntegral height / 2 || y <= -fromIntegral height / 2  || y - (logoHeight logo)/2 <= -fromIntegral height / 2
      then 
        --update the velocity 
        -vy
        else 
          vy 
    newState = if (vx' /= vx || vy' /= vy) then nextColor logo else logo {logoFile = logofile}

renderLogo :: LogoState -> Picture 
renderLogo logo = translate x y $ color red $ rectangleSolid (logoWidth logo)  (logoHeight logo)
  where
    (x, y) = logoPos logo

png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

renderDVD :: LogoState -> Picture 
renderDVD logo = translate x y $ png (logoFile logo)
  where 
    (x, y) = logoPos logo

eventFunc :: Event -> LogoState -> LogoState
eventFunc (EventResize (w, h)) logo = logo {screenSize = (w, h)}
eventFunc _ logo = logo
  

main :: IO ()
main = do 
    (width, height) <- getScreenSize
    let window = InWindow "Bouncing Ball" (width, height) (offset, offset)
    play window background fps initialLogoState renderDVD eventFunc update
    

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


-}  