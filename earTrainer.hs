{-# LANGUAGE OverloadedStrings #-}
--writeMidi
module Main where

import Codec.Midi
import Control.Concurrent
import Control.Monad
import qualified Data.Text as Text
import Euterpea
import Foreign.Marshal.Utils
import Graphics.Blank

main = blankCanvas 3000 { static = ["Treble_Clef.svg","notehead.svg"], events = ["mousedown"]}$ \ context -> do
     looper context ((-1,-1)::(Float, Float))

looper context (x',y') = do
        send context (do
                let (wdt, hgt) = (height context, width context)::(Float,Float)
                save()
                let wRat = 3/5  --width ratio
                    hRat = 3/10 --height ratio
                translate (wdt * wRat, hgt * hRat)
                let buttCoord = [ drawButton (x*55,150) y | (x,y) <- [(0,"m2"),(1,"m3"),(2,"P4"),(3,"m6"),(4,"m7"),(5,"8ve")]]
                buttCoord' <- sequence buttCoord
                let buttIdx = buttonPress buttCoord' (x' - wdt * wRat, y' - hgt * hRat)
                fillText(Text.pack $ show buttCoord',-250,-250)
                fillText(Text.pack $ show (x',y'),-200,-200)
                fillText(Text.pack $ show buttIdx,-150,-150)
                stroke()
                let ntIdx  = getNoteIdx $ getNoteIdxHelper y' hgt hRat
                let redraw = shouldRedraw ntIdx y'
                if not redraw
                   then do
                      restore()
                   else do
                      clearRect (-(wdt * wRat), -(hgt * hRat), wdt, hgt)
                      let buttCoord = [ drawButton (x*55,150) y | (x,y) <- [(0,"m2"),(1,"m3"),(2,"P4"),(3,"m6"),(4,"m7"),(5,"8ve")]]
                      buttCoord' <- sequence buttCoord
                      drawNote (wdt, hgt) ntIdx
                      sequence_ $ map drawStaffLines [0,10..40]
                      img2 <- newImage "Treble_Clef.svg"
                      let proportion = (width img2) / (height img2)
                      drawImage(img2, [0,-13,70*proportion,70])
                      restore())
          
        event <- wait context
        case ePageXY event of
          Nothing -> looper context (x',y')
          Just x -> looper context x

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
drawNote :: (Float, Float) -> Maybe Int -> Canvas ()
drawNote _          (Nothing)= do return ()
drawNote (wdt, hgt) (Just x) = do
  img1 <- newImage "notehead.svg"
  let (wdt, hgt) = (1024, 768)::(Float,Float)
  drawImage(img1, [30,5*fromIntegral x,30,30])
  
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

testButt :: Show a => Maybe a -> Canvas ()
testButt Nothing  = do fillText("Nothing",50, 50)
                       stroke()
testButt (Just a) = do fillText(Text.pack $ show a, 50, 50)
                       stroke()


