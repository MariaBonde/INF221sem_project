module Main(main) where
import Graphics.Gloss
import Codec.Picture
import Graphics.Gloss.Data.Bitmap 
import Codec.Picture.Types
import Graphics.Gloss.Data.ViewPort
import Debug.Trace (trace)
import Graphics.Gloss.Interface.Environment
import Debug.Trace


width, height, offset :: Int 
width = 800
height = 800
background = black 
fps = 600
offset = 100 
data LogoState = Logo {
  logoPos :: (Float, Float), 
  logoVel :: (Float, Float),
  logoWidth :: Float, logoHeight :: Float} 

initialLogoState :: LogoState
initialLogoState = Logo {logoPos = (0, 0), logoVel = (100,100), logoWidth = 90.0, logoHeight = 60.0}

update :: ViewPort -> Float -> LogoState -> LogoState
update _ seconds = changeDir . moveLogo seconds  

moveLogo :: Float -> LogoState -> LogoState
moveLogo seconds state = state { logoPos = (x', y')}
  where 
    (x,y) = logoPos state
    (vx,vy) = logoVel state

    x' = x + vx * seconds
    y' = y + vy * seconds
changeDir :: LogoState -> LogoState
changeDir logo = 
  let (x, y) = logoPos logo
      (vx, vy) = logoVel logo
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
  in logo {logoVel = (vx', vy')}
{-

changeDir :: LogoState -> LogoState
changeDir logo = logo {logoVel = (vx', vy')}
  where  
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
          vy -}
renderLogo :: LogoState -> Picture 
renderLogo logo = translate x y $ color red $ rectangleSolid (logoWidth logo)  (logoHeight logo)
  where
    (x, y) = logoPos logo
  

main :: IO ()
main = do 
    --let(width, heigth) <- getScreenSize
    let window = InWindow "Bouncing Ball" (width, height) (offset, offset)
    simulate window background fps initialLogoState renderLogo update
    
