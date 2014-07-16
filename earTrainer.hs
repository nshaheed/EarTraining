{-# LANGUAGE OverloadedStrings #-}
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
     looper context ((-1,-1)::(Float, Float)) (0,0) True True

looper context (x',y') (a,b) ppn pn = do

        putStrLn ""
        putStrLn $ show pn
        (a',b') <- getNotes pn (a,b)
        let intButtIdx = intToPitchToButtIdx (a',b') -- interval of the random notes
            -- pn'        = isButtPressed context (x',y') || isNotePlaced context y' -- determine whether to play note in next call
            pn'        = ppn
            usrButtIdx = getButtPressed context (x',y')  -- index of button pressed
            ntIdx1     = getNoteIdx (getNoteIdxHelper y' (height context) 0.4) -- index for drawing notes

            --usrNtIdx converts ntIdx1 to the same number system as usrButtIdx, allowing comparisons
            usrNtIdx   = if isJust ntIdx1 then Just $ intToPitchToButtIdx (a',(fromJust ntIdx1) + 4) else Nothing

            -- usrIdx is assigned the index of whichever, if any, note guessing system was used: button presses or note clicking
            usrIdx     = if isJust usrButtIdx then usrButtIdx else usrNtIdx
            guessed    = correctGuess usrIdx intButtIdx
            ppn'       = isJust guessed -- preliminary play note'

        when pn' $ playNotes (a',b')

        putStrLn ""
        -- if guessed
        --    then if (abs (intButtIdx) == ((floor $ fromJust usrButtIdx)::Int)) then putStrLn "Correct Guess"
        --                                                                       else putStrLn "Incorrect Guess"
        --    else putStr ""

        putStrLn $ show guessed
        putStrLn $ show intButtIdx
        putStrLn $ show pn'
        putStrLn $ show usrButtIdx
        putStrLn $ show usrNtIdx
        putStrLn $ show (a',b')
        putStrLn $ show ntIdx1
        putStrLn $ show usrIdx
        putStrLn ""
        
        send context (do
                let (wdt, hgt) = (width context, height context)::(Float,Float)                         
                save()
                let hRat = 0.4 --height ratio
                    wRat = 0.4 --width ratio
                translate (wdt * wRat, hgt * hRat)
                
                stroke()

                -- get what note the mouse click equates to
                let ntIdx  = getNoteIdx $ getNoteIdxHelper y' hgt hRat
                let redraw = shouldRedraw ntIdx y' (ppn' || pn')

                --TODO: Replace if with when


                -- when (not redraw) (
                --   do clearRect (-(wdt * wRat), -(hgt * hRat), wdt, hgt)
                --      let x = 1
                --      fillText("fjkdls",50,50))

                -- if not redraw
                when (redraw) (
                  do
                    -- draw the buttons, return a list of their locations
                    clearRect (-(wdt * wRat), -(hgt * hRat), wdt, hgt)
                    let buttCoord = [ drawButton (x*55 - 193,150) y |
                                      (x,y) <- [(0,"u"),(1,"m2"),(2,"M2"),(3,"m3"),(4,"M3"),(5,"P4"),(6,"TT"),
                                                (7,"P5"),(8,"m6"),(9,"M6"),(10,"m7"),(11,"M7"),(12,"8ve")]]
                    buttCoord' <- sequence buttCoord
                    -- determine whether button was pressed
                    let buttIdx = buttonPress buttCoord' (x' - wdt * wRat, y' - hgt * hRat)

                    -- draw note based on mouse click
                    drawNote (wdt, hgt) (Just $ a' - 4) 0
                    drawNote (wdt, hgt) ntIdx 110
                    when (isJust guessed) (drawNote (wdt, hgt) (Just $ b' -4) 50)
                    sequence_ $ map drawStaffLines [0,10..40]


                    -- rework with usrButtIdx such that it accounts for usrButtIdx::Just Int
                    when (isJust guessed) (do font "15pt Calibri"
                                              if fromJust guessed == True then do fillText("Correct Guess",65,-20)
                                                                          else do fillText("Incorrect Guess",65,-20)
                                              fillText("Click to",212, -39)
                                              fillText("Continue",212, -20))
                      
                    -- fiddling to get the treble clef to look good
                    img2 <- newImage "Treble_Clef.svg"
                    let proportion = (width img2) / (height img2)
                    drawImage(img2, [0,-13,70*proportion,70]))
                  
                restore())
                  
                

        -- loop function
        event <- wait context
        case ePageXY event of
          Nothing -> looper context (x',y') (a' ,b') ppn' pn'
          Just x -> looper context x (a' ,b') ppn' pn'

shouldRedraw :: Maybe a -> Float -> Bool -> Bool
shouldRedraw Nothing (-1) _  = True
shouldRedraw Nothing  _   b  = b
shouldRedraw (Just _) _   _  = True

isInBounds :: Float -> (Float,Float) -> Bool
isInBounds n (w,h)
  | n > 5     = False
  | otherwise = if n < -3 then False
                          else True

-- returns the
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

-- Returns whether the mouse click is within the button
buttonPress     :: [(Float, Float)] -> (Float, Float) -> Maybe Int
buttonPress x y = let z = buttonPressWorker x y
                  in if z >= fromIntegral (length x) then Nothing
                                                     else Just z

buttonPressWorker :: [(Float, Float)] -> (Float, Float) -> Int
buttonPressWorker ((x,y):z) c@(a,b) = if a >= x && a <= x + 45 &&
                                         b >= y && b <= y + 30
                                      then 0
                                      else 1 + buttonPressWorker z c
buttonPressWorker [] _              = 1
buttonPressWorker _ _               = -1                                           

-- Generates tuple of two random notes within the range of D4 - G5
getNotes :: Bool -> (Int, Int) -> IO (Int, Int)
getNotes True _  = do g <- newStdGen
                      let (x,g') = randomR (1,10) g
                      let (y,_ ) = randomR (max 1 (x - 7), min 10 (x + 7)) g'
                      return (x,y)
getNotes False a = do return a

--Converts int to Music Pitch in proper octave with a quarter note length
intToPitch :: Int -> Music Pitch
intToPitch i = let notes = [c, d, e, f, g, a, b, c, d, e, f, g]
               in (notes!!(11- i)) ((floor ((fromIntegral (11-i))/7)) + 4) qn

--change from writeMidi to play when publish
playNotes :: (Int,Int) -> IO ()
playNotes (x,y) = do let z = intToPitch x :+: intToPitch y
                     writeMidi "randomTest.mid" z
                     --play z

-- Converts the arbitrary index of the notes to a music pitch
-- which is then converted to the absolute pitch (a standard
-- Int representation of the pitch)
intToPitchToButtIdx :: (Int, Int) -> Int
intToPitchToButtIdx (x,y) = let a = getAbsPitch $ intToPitch x
                                b = getAbsPitch $ intToPitch y
                            in a - b

getAbsPitch :: Music Pitch -> Int
getAbsPitch (Prim (Note _ m)) = absPitch m

getButtPressed :: DeviceContext -> (Float, Float) -> Maybe Int
getButtPressed context (x',y') = let buttCoord = [ (x*55 - 193 + wdt * wRat, 150 + hgt * hRat) | x <- [0..12]]
                                     wdt = width context
                                     hgt = height context
                                     hRat = 0.4
                                     wRat = 0.4
                                 in  buttonPress buttCoord (x', y')
              
                                                                    
isButtPressed :: DeviceContext -> (Float, Float) -> Bool
isButtPressed context (x',y') = isJust $ getButtPressed context (x',y')

isNotePlaced :: DeviceContext -> Float -> Bool
isNotePlaced context y' = if isJust $ getNoteIdx $ getNoteIdxHelper y' (height context) 0.4 then True else False

correctGuess :: Maybe Int -> Int -> Maybe Bool
correctGuess Nothing _  = Nothing
correctGuess (Just a) b = Just $ a == b
