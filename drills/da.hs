#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad.State
import System.Random

main ::
  IO ()
main =  
  do  q <- question
      when (null q) main

randomtemp ::
  (Random a, RandomGen g, Num a) =>
  State g a
randomtemp =
  state (randomR (-15, 45))

randomele ::
  (Random a, RandomGen g, Num a) =>
  State g a
randomele =
  state (\g ->  let (p, h) = randomR (-10, 240) g
                in  (p * 50, h))

randomqnh ::
  (Random a, RandomGen g, Num a) =>
  State g a
randomqnh =
  state (randomR (990, 1030))

question ::
  IO String
question =
  let teq ::
        State StdGen (Int, Int, Int)
      teq =
        (,,) <$> randomtemp <*> randomele <*> randomqnh      
  in  do  g <- newStdGen
          let (t, e, q) =
                evalState teq g
              isatemp =
                15              
              pa =
                let isapressure = 1013.25
                in  fromIntegral e + (isapressure - fromIntegral q) * 30
              pa' =
                let isapressure = 1013
                in  fromIntegral e + (isapressure - fromIntegral q) * 30
              da =
                let temp_rate = 100000 / 198
                    pa_rate = 118.8
                in  (fromIntegral t - isatemp + (pa / temp_rate)) * pa_rate + pa
              da' =
                let temp_rate = 500
                    pa_rate = 120
                in  (fromIntegral t - isatemp + (pa / temp_rate)) * pa_rate + pa
          putStrLn . concat $
            [
              "Temperature: "
            , show t
            , "C\nElevation: "
            , show e
            , "ft\nQNH: "
            , show q
            , "hPa"
            ]
          l <- getLine
          putStrLn . concat $
            [
              "Pressure Altitude: "
            , show pa
            , "\nDensity Altitude: "
            , show da
            , "\nPressure Altitude (approx): "
            , show pa'
            , "\nDensity Altitude (approx): "
            , show da'
            , "\n\n-----------------\n"
            ]
          return l
          
