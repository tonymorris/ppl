#!/usr/bin/env runhaskell

import System.Random
import Text.Printf

main ::
  IO ()
main =
  do  q <- randomquestion
      if null q
        then
          main
        else
          return ()

format ::
  (PrintfArg a, PrintfType b) =>
  a
  -> b
format x =
  printf "%.2f" x

randomquestion ::
  IO String
randomquestion =
  do  g <- newStdGen
      let (a, g1) = randomR (1, 72) g
          (d, g2) = randomR (1, 50) g1
          (z, g3) = randomR (1, d - 1) g2
          (t, _)  = randomR (1, d - 1) g3
      question
        (fromIntegral (a :: Int) * 5)
        (fromIntegral (d :: Int) * 20)
        (fromIntegral z * 20)
        (fromIntegral t :: Double)

question ::
  (PrintfArg a, Ord a, Fractional a) =>
  a -- ^ original course
  -> a -- ^ distance to destination
  -> a -- ^ distance travelled
  -> a -- ^ distance off-track
  -> IO String
question a d z t =
  let (x, y) = navigate d z t
  in  do  putStrLn . concat $
            [
              "A pilot flying to a destination on a track to "
            , format a
            , " degrees at a distance of "
            , format d
            , " nautical miles, after "
            , format z
            , " nautical miles finds themselves "
            , format (abs t)
            , " nautical miles "
            , if t < 0 then "left" else "right"
            , " of an expected intermediate waypoint. Navigate."
            ]
          l <- getLine
          mapM_
            putStrLn
            [
              concat
                [
                  "* Correct to intended track "
                , format x
                , " degrees"
                ]
            , concat
                [
                  "* Correct to destination "
                , format y
                , " degrees"
                ]
            , concat
                [
                  "* Total correction "
                , format (x + y)
                , " degrees"
                ]
            , concat
                [
                  "* New course "
                , format (over360 (a - x - y))
                ]
            ]
          return l

over360 ::
  (Ord a, Num a) =>
  a
  -> a
over360 n =
  if
    n >= 360
      then
        n - 360
      else if n <= -360
        then
          n + 360
        else
          n

navigate ::
  Fractional a =>
  a -- ^ distance to destination
  -> a -- ^ distance travelled
  -> a -- ^ distance off-track
  -> (a, a)
navigate d z t =
  let r = t * 60 / z
      s = t * 60 / (d - z)
  in  (r, s)

sinr :: 
  Double
  -> Double
sinr =
  sin . toradians

toradians ::
  Floating a =>
  a
  -> a
toradians x =
  x * pi / 180
  
todegrees ::
  Floating a =>
  a
  -> a
todegrees x =
  x * 180 / pi
  