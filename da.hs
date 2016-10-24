#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Monad.State
import System.Random

main ::
  IO ()
main =
  question

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
  IO ()
question =
  let teq ::
        State StdGen (Int, Int, Int)
      teq =
        (,,) <$> randomtemp <*> randomele <*> randomqnh      
  in  do  g <- newStdGen
          let (t, e, q) =
                evalState teq g
              isapressure =
                1013
              isatemp =
                15              
              pa =
                fromIntegral e + (isapressure - fromIntegral q) * 30
              da =
                (fromIntegral t - isatemp + (pa / 500)) * 120 + fromIntegral e
          putStrLn . concat $
            [
              "Temperature: "
            , show t
            , "C\nElevation: "
            , show e
            , "ft\nQNH: "
            , show q
            , "hPa\n-----------------\n?Density Altitude"
            ]
          l <- getLine
          putStrLn . concat $
            [
              "Pressure Altitude: "
            , show pa
            , "\nDensity Altitude: "
            , show da
            ]
          
