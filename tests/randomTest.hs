module Main where

import Codec.Midi
import Control.Concurrent
import Control.Monad
import qualified Data.Text as Text
import Euterpea
import Foreign.Marshal.Utils
import Graphics.Blank
import System.Random

-- main = do (x,y) <- getNotes
--           let z = intToPitch x
--           print (x,y)
--           print z
--           playNotes (x,y)
                   
--make sure distance isn't great than 7 (?)
getNotes :: IO (Int, Int)
getNotes = do g <- getStdGen
              let x = Prelude.head $ randomRs (1,11) g
              let y = Prelude.head $ Prelude.tail $ randomRs (max 1 (x - 7), min 11 (x + 7)) g
              return (x,y)

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
                                b = getAbsPitch $ intToPitch y
                            in abs $ (a - b)

getAbsPitch :: Music Pitch -> Int
getAbsPitch (Prim (Note _ m)) = absPitch m
