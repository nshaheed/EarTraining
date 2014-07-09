{-# LANGUAGE OverloadedStrings #-}
--writeMidi
module Main where

import Codec.Midi
import Control.Concurrent
import Control.Monad
import Data.Maybe
import qualified Data.Text as Text
import Euterpea
import Foreign.Marshal.Utils
import Graphics.Blank
import System.Random

main = blankCanvas 3000 { static = ["Treble_Clef.svg","notehead.svg"], events = ["mousedown"]}$ \ context -> do
     looper context ((-1,-1)::(Float, Float)) (0,0) True

looper context (x',y') (a,b) pn = do

        (a',b') <- getNotes pn (a,b)
        let intButtIdx = intToPitchToButtIdx (a',b')
            pn'        = isButtPressed context (x',y')
            
        when pn' $ playNotes (a',b')
        --pn' <- send context $ 

        putStrLn ""
        putStrLn $ show pn'
        putStrLn $ show (a',b')
        putStrLn $ show $ isButtPressed context (x',y')
        putStrLn $ show (x',y')
        
        send context (do
                --let (wdt, hgt) = (height context, width context)::(Float,Float)
                let (wdt, hgt) = (width context, height context)::(Float,Float)                         
                save()
                let hRat = 0.4 --3/5  --height ratio
                    wRat = 0.4 --3/10 --width ratio
                translate (wdt * wRat, hgt * hRat)
                
                fillText(Text.pack $ show $ wdt * wRat,-200,-200)
                fillText(Text.pack $ show $ hgt * hRat,-200,-250)              
                -- fillText(Text.pack $ show (x',y'),-200,-200)
                fillText(Text.pack $ show (wdt, hgt),-150,-150)

                stroke()
                
                let ntIdx  = getNoteIdx $ getNoteIdxHelper y' hgt hRat
                let redraw = shouldRedraw ntIdx y'
       
                -- when (not redraw) (
                  
                --       do clearRect (-(wdt * wRat), -(hgt * hRat), wdt, hgt)
                --          let buttCoord = [ drawButton (x*55,150) y | (x,y) <- [(0,"m2"),(1,"m3"),(2,"P4"),(3,"m6"),(4,"m7"),(5,"8ve")]]
                --          buttCoord' <- sequence buttCoord
                --          drawNote (wdt, hgt) ntIdx
                --           img2 <- newImage "Treble_Clef.svg"
                --       let proportion = (width img2) / (height img2)
                --       drawImage(img2, [0,-13,70*proportion,70])
                  
                if not redraw
                  then do
                      restore()
                  else do
                      -- do
                      clearRect (-(wdt * wRat), -(hgt * hRat), wdt, hgt)
                      let buttCoord = [ drawButton (x*55 - 193,150) y |
                                        (x,y) <- [(0,"u"),(1,"m2"),(2,"M2"),(3,"m3"),(4,"M3"),(5,"P4"),(6,"TT"),
                                                  (7,"P5"),(8,"m6"),(9,"M6"),(10,"m7"),(11,"8ve")]]
                      buttCoord' <- sequence buttCoord
                      let buttIdx = buttonPress buttCoord' (x' - wdt * wRat, y' - hgt * hRat)

                      drawNote (wdt, hgt) (Just $ a' - 4) 0
                      drawNote (wdt, hgt) ntIdx 50
                      sequence_ $ map drawStaffLines [0,10..40]

                      img2 <- newImage "Treble_Clef.svg"
                      let proportion = (width img2) / (height img2)
                      drawImage(img2, [0,-13,70*proportion,70])
                      restore())
          
        event <- wait context
        case ePageXY event of
          Nothing -> looper context (x',y') (a' ,b') pn'
          Just x -> looper context x (a' ,b') pn'

shouldRedraw :: Maybe a -> Float -> Bool
shouldRedraw Nothing (-1)  = True
shouldRedraw Nothing  _    = False
shouldRedraw (Just _) _    = True

isInBounds :: Float -> (Float,Float) -> Bool
isInBounds n (w,h)
  | n > 5     = False
  | otherwise = if n < -3 then False
                          else True
                              
getNoteIdx (q,r)
  | q > 5     = Nothing
  | otherwise = if q < -3 then Nothing
                else Just (q + (quot r 3))
  
getNoteIdxHelper n h r = let n' = floor $ n - h * r - 13
                         in (quot n' 5, rem n' 5)

--TODO: Make notehead using html5 instead of svg
drawNote :: (Float, Float) -> Maybe Int -> Float -> Canvas ()
drawNote _          (Nothing) _ = do return ()
drawNote (wdt, hgt) (Just x)  i = do
  img1 <- newImage "notehead.svg"
--  let (wdt, hgt) = (1024, 768)::(Float,Float)
  drawImage(img1, [30 + i,5*fromIntegral x,30,30])
  
drawStaffLines n  = do
  beginPath()
  moveTo(0,n)
  lineTo(250,n)
  stroke()

drawButton :: (Float, Float) -> Text.Text -> Canvas (Float,Float)
drawButton (x,y) s = do
  beginPath()
  rect(x,y,45,30)
  textAlign "center"
  fillText(s,x + 45/2,y + 30/2)
  stroke()
  return (x,y)

buttonPress     :: [(Float, Float)] -> (Float, Float) -> Maybe Float
buttonPress x y = let z = buttonPressWorker x y
                  in if z >= fromIntegral (length x) then Nothing
                                                     else Just z

buttonPressWorker :: [(Float, Float)] -> (Float, Float) -> Float
buttonPressWorker ((x,y):z) c@(a,b) = if a >= x && a <= x + 45 &&
                                         b >= y && b <= y + 30
                                      then 0
                                      else 1 + buttonPressWorker z c
buttonPressWorker [] _              = 1
buttonPressWorker _ _               = -1                                           

-- getNotes :: Bool -> (Int, Int) -> IO (Int, Int)
-- getNotes True _  = do g <- getStdGen
--                       let x = Prelude.head $ randomRs (1,11) g
--                       let y = Prelude.head $ Prelude.tail $ randomRs (max 1 (x - 7), min 11 (x + 7)) g
--                       return (x,y)
-- getNotes False a = do return a

getNotes :: Bool -> (Int, Int) -> IO (Int, Int)
getNotes True _  = do g <- getStdGen
                      let (x,g') = randomR (1,11) g
                      let (y,_ ) = randomR (max 1 (x - 7), min 11 (x + 7)) g'
                      return (x,y)
getNotes False a = do return a

--Converts int to Music Pitch in proper octave with a quarter note length
intToPitch :: Int -> Music Pitch
intToPitch i = let notes = [c, d, e, f, g, a, b, c, d, e, f, g]
               in (notes!!i) ((floor ((fromIntegral i)/7)) + 4) qn

                                     --change from writeMidi to play when publish
playNotes :: (Int,Int) -> IO ()
playNotes (x,y) = do let z = intToPitch x :+: intToPitch y
                     writeMidi "randomTest.mid" z --play z

intToPitchToButtIdx :: (Int, Int) -> Int
intToPitchToButtIdx (x,y) = let a = getAbsPitch $ intToPitch x
                                b = getAbsPitch$ intToPitch y
                            in abs $ (a - b)

getAbsPitch :: Music Pitch -> Int
getAbsPitch (Prim (Note _ m)) = absPitch m
                                                                                                                                           
-- isButtPressed :: DeviceContext -> (Float, Float) -> ([(Float, Float)], Maybe Float)
-- isButtPressed context (x',y') = let buttCoord = [ (x*55 - 193 + wdt * wRat, 150 + hgt * hRat) | x <- [0..11]]
--                                 -- let buttCoord = [(150 + hgt * hRat, x*55 + wdt * wRat) | x <- [0..4]]
--                                     wdt = width context
--                                     hgt = height context
--                                     hRat = 0.4--3/5
--                                     wRat = 0.4--3/10
--                                     buttIdx = buttonPress buttCoord (x', y')
--                                 -- in if isJust buttIdx then False else True
--                                 in (buttCoord, buttIdx)

isButtPressed :: DeviceContext -> (Float, Float) -> Bool
isButtPressed context (x',y') = let buttCoord = [ (x*55 - 193 + wdt * wRat, 150 + hgt * hRat) | x <- [0..11]]
                                    wdt = width context
                                    hgt = height context
                                    hRat = 0.4--3/5
                                    wRat = 0.4--3/10
                                    buttIdx = buttonPress buttCoord (x', y')
                                in if isJust buttIdx then True else False
                                -- in (buttCoord, buttIdx)
                                                                                                                                                                                                                                                                                                                         
