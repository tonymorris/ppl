{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aviation.Casr.Logbook
import System.Environment
import System.IO

vhwkm ::
  Aircraft
vhwkm =
  Aircraft
    "1980 American Champion Citabria 7GCBC"
    "VH-WKM"
    Single

vhldo ::
  Aircraft
vhldo =
  Aircraft
    "2011 Cessna 162 Skycatcher"
    "VH-LDO"
    Single

vhafr ::
  Aircraft
vhafr =
  Aircraft
    "2000 Cessna 172S Skyhawk"
    "VH-AFR"
    Single

vhvvo ::
  Aircraft
vhvvo =
  Aircraft
    "2000 Cessna 172R Skyhawk"
    "VH-VVO"
    Single

vhzwy ::
  Aircraft
vhzwy =
  Aircraft
    "2000 Cessna 172S Skyhawk"
    "VH-ZWY"
    Single

ybaf2ybaf ::
  FlightPath
ybaf2ybaf =
  directPath
    "YBAF"
    "YBAF"

entries ::
  Entries
entries =
  Entries
    [
      flight
        "Effects of Controls"
        "20151214"
        vhldo
        (
          Hours
            1
            2
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        mempty
        mempty
        mempty
        mempty
    , flight
        "Straight & Level"
        "20151218"
        vhldo
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151218-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151218-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/595690"
              Nothing
              (Doarama "6rAdypE")
          ]
        )
        mempty
        (
          Videos
          [
            Video
              "13BVior4VmY"
              (Just "Head camera")
              YouTube
          ]
        )
    , flight
        "Climbing & Descending"
        "20151220"
        vhldo
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "Michael Ward"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20151220-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20151220-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/596790"
              Nothing
              (Doarama "6rZrPp6")
          ]
        )
        mempty
        (
          Videos
          [
            Video
              "8tZ8kxsVz6E"
              (Just "Head camera")
              YouTube
          ]      
        )
    , flight
        "Turning"
        "20160104"
        vhldo
        (
          Hours
            1
            3
        )
        dual
        ybaf2ybaf
        Day
        "Ryan Low"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160104-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160104-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/607004"
              Nothing
              (Doarama "6x5Az8e")
          ]
        )
        mempty
        mempty
    , flight
        "Stalling"
        "20160108"
        vhldo
        (
          Hours
            1
            3
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160108-vh-ldo.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160108-vh-ldo.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/611980"
              Nothing
              (Doarama "eYQA7ve")
          ]
        )
        mempty
        mempty
    , flight
        "Consolidation"
        "20160115"
        vhafr
        (
          Hours
            1
            4
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160115-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160115-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/617610"
              Nothing
              (Doarama "k8p38wk")
          ]
        )
        mempty
        mempty
    , flight
        "Circuits"
        "20160122"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160122-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160122-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/622258"
              Nothing
              (Doarama "k0nXwd6")
          ]
        )
        (
          Images
          [
            Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082411.jpg"
              Nothing
              Jpg
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082856.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082902.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_082905.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083348.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083352.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_083353.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084902.jpg"
              Nothing
              Jpg 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084906.jpg"
              Nothing
              Jpg 
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160122-vh-afr/20160122_084908.jpg"
              Nothing
              Jpg                                     
          ]
        )
        (
          Videos
          [
            Video
              "gz8Ivcjas9o"
              (Just "Head camera")
              YouTube
          ]
        )     
    , flight
        "Circuits"
        "20160129"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160129-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160129-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/626694"
              Nothing
              (Doarama "6b7RKAe")
          ]
        )
        mempty
        (
          Videos
          [
            Video
              "k9c1GdxkdQY"
              (Just "Head camera")
              YouTube
          ]       
        )
    , flight
        "Circuits"
        "20160205"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160205-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160205-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/635027"
              Nothing
              (Doarama "eBdaGlE")
          ]
        )
        mempty
        mempty
    , flight
        "Circuits"
        "20160212"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160212-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160212-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/640001"
              Nothing
              (Doarama "E2y3mnk")
          ]
        )
        mempty
        (
          Videos
          [
            Video
              "e7UcjgxOmDw"
              (Just "Head camera")
              YouTube
          ]
        )                                                            
    , flight
        "Circuit Emergencies"
        "20160218"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160218-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160218-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/644663"
              Nothing
              (Doarama "Ep1r9P6")
          ]
        )
        (
          Images
          [
            Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090347.jpg"
              Nothing
              Jpg
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090348.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090351.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090409.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090411.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090421.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090423.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090432.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090436.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090441.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090446.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090452.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090454.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090459.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090501.jpg"
              Nothing
              Jpg            
          , Image
              "https://raw.githubusercontent.com/tonymorris/ppl/master/images/20160218-vh-afr/20160218_090502.jpg"
              Nothing
              Jpg            
          ]
        )
        (
          Videos
          [
            Video
              "EO4D4zFnpIU"
              (Just "Head camera")
              YouTube
          ]  
        )         
    , exam
        "20160225"
        "First Solo Theory Exam"
        "David Schofield"
        "FIR II"
        (ARN "589522")
        31
        40
    , flight
        "Circuit Emergencies"
        "20160304"
        vhafr
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160304-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160304-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/674600"
              Nothing
              (Doarama "6XbQDjk")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "jWUijREg4aE"
              (Just "Head camera")
              YouTube
          ]
        )      
    , exam
        "20160309"
        "Area Solo Theory Exam"
        "David Schofield"
        "FIR II"
        (ARN "589522")
        38
        40                  
    , flight
        "Circuit Emergencies"
        "20160324"
        vhzwy
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160324-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160324-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/691106"
              Nothing
              (Doarama "6bPVnnk")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "RNiYu3ZsfZw"
              (Just "Forward camera")
              YouTube
          ]
        )                        
    , flight
        "Circuits"
        "20160330"
        vhzwy
        (
          Hours
            0
            9
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/696748"
              Nothing
              (Doarama "6xV5dVk")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )
    , flight
        "Circuits"
        "20160330"
        vhzwy
        (
          Hours
            0
            4
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160330-vh-zwy.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160330-vh-zwy.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/696748"
              Nothing
              (Doarama "6xV5dVk")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )           
    , flight
        "Circuits (Crosswind)"
        "20160415"
        vhafr
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160415-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160415-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/711553"
              Nothing
              (Doarama "k9nWdbE")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )
    , flight
        "Circuits (Solo check)"
        "20160422"
        vhafr
        (
          Hours
            0
            5
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/719248"
              Nothing
              (Doarama "6xVbR8k")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "UR1IkF5RSh4"
              (Just "Forward camera")
              YouTube
          ]
        )                                     
    , flight
        "Circuits (Solo)"
        "20160422"
        vhafr
        (
          Hours
            1
            0
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160422-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160422-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/719248"
              Nothing
              (Doarama "6xVbR8k")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "UR1IkF5RSh4"
              (Just "Forward camera")
              YouTube
          ]
        )                                     
    , flight
        "Circuits (Dual)"
        "20160427"
        vhvvo
        (
          Hours
            0
            6
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/724934"
              Nothing
              (Doarama "eo1yvBE")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
          ]
        )                                     
    , flight
        "Circuits (Solo)"
        "20160427"
        vhvvo
        (
          Hours
            0
            8
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160427-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160427-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/724934"
              Nothing
              (Doarama "eo1yvBE")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "2QNp74n2Lx8"
              (Just "Forward camera")
              YouTube
          ]
        ) 
    , flight
        "Practice Forced Landings"
        "20160509"
        vhafr
        (
          Hours
            1
            0
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160509-vh-afr.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160509-vh-afr.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/741813"
              Nothing
              (Doarama "edWMLO6")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "o1dx3hZuov0"
              (Just "Forward camera")
              YouTube
          , Video
              "6250595"
              (Just "Live stream")
              Bambuser
          ]
        )          
      , flight
        "Practice Forced Landings"
        "20160514"
        vhvvo
        (
          Hours
            1
            2
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160514-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160514-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/748638"
              Nothing
              (Doarama "6jAODJE")
          ]
        )
        (
          Images
          [
          ]
        )
        (
          Videos
          [
            Video
              "vw9KjjqkOEg"
              (Just "Forward camera")
              YouTube
          , Video
              "6258610"
              (Just "Live stream")
              Bambuser
          ]
        )                                             
      , flight
        "Steep Turns"
        "20160519"
        vhvvo
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "Ryan Low"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160519-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160519-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/756397"
              Nothing
              (Doarama "k9nNR4E")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [
            Video
              "ZEABSxrN4xM"
              (Just "Forward camera")
              YouTube
          , Video
              "6267124"
              (Just "Live stream")
              Bambuser
          ]
        )
        , flight
        "Sydney Circuit, Stalls"
        "20160528"
        vhwkm
        (
          Hours
            1
            8
        )
        dual
        (FlightPath "YSCN" ["PSP", "PAR", "LRF", "V1", "JIBN", "SECF"] "YSCN")
        Day
        "Ken Osbourne"
        (
          TrackLogs
            [
            ]
        )
        (
          Visualisations
          [
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [
          ]
        )          
      , flight
        "Steep Turns and Sideslipping"
        "20160530"
        vhvvo
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "Damien Boyer"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160530-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160530-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/770143"
              Nothing
              (Doarama "65oMwXk")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [ 
           Video
              "6288135"
              (Just "Live stream")
              Bambuser
          ]
        )      
      , flight
        "Circuits (Crosswind)"
        "20160606"
        vhvvo
        (
          Hours
            0
            9
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160606-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160606-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/778822"
              Nothing
              (Doarama "eOrqL86")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [
            Video
              "OYuR7E1xjyg"
              (Just "Forward camera")
              YouTube            
          , Video
              "6300417"
              (Just "Live stream")
              Bambuser
          ]
        )                                                 
      , flight
        "Area Solo Check"
        "20160616"
        vhvvo
        (
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160616-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160616-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/798173"
              Nothing
              (Doarama "67W2vz6")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [
            Video
              "6318524"
              (Just "Live stream")
              Bambuser
          ] 
        )                                           
      , flight
        "Area Solo"
        "20160617"
        vhvvo
        (
          Hours
            1
            5
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160617-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160617-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/801102"
              Nothing
              (Doarama "6rabNGE")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [ 
            Video
              "zF51UQ377E8"
              (Just "Forward camera -- Eastern Departure")
              YouTube            
          , Video
              "AWLegaBJGFs"
              (Just "Forward camera -- Practice Forced Landings")
              YouTube            
          , Video
              "6320415"
              (Just "Live stream")
              Bambuser
          ] 
        )    
      , flight
        "Area Solo"
        "20160626"
        vhvvo
        (
          Hours
            1
            1
        )
        solo
        ybaf2ybaf
        Day
        "Tony Morris"
        (
          TrackLogs
            [
              TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160626-vh-vvo.gpx"
              Nothing
              Gpx      
            , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160626-vh-vvo.png"
              Nothing
              (ImageLog Png)                    
            ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/815958"
              Nothing
              (Doarama "k0zbK26")
          ]
        )
        (
          Images
          [
          ]
        )
        ( 
          Videos
          [ 
            Video
              "G0-RoTxRsGw"
              (Just "Eastern Arrival, fly-over")
              YouTube          ] 
        )
    , exam
        "20160626"
        "Flight Radiotelephone Operator Licence"
        "David Schofield"
        "FIR II"
        (ARN "589522")
        38
        40
    -- todo 0.5 Basic IF simulator 20160706
    , flight
        "Basic Instrument Flight"
        "20160714"
        vhafr
        ( -- 0.8 under the hood
          Hours
            1
            1
        )
        dual
        ybaf2ybaf
        Day
        "David Schofield"
        (
          TrackLogs
          [
            TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/20160714-vh-afr.gpx"
              Nothing
              Gpx
          , TrackLog
              "https://raw.githubusercontent.com/tonymorris/ppl/master/tracks/png/20160714-vh-afr.png"
              Nothing
              (ImageLog Png)
          ]
        )
        (
          Visualisations
          [
            Visualisation
              "http://doarama.com/view/841382"
              Nothing
              (Doarama "6jr3Rb6")
          ]
        )
        mempty
        mempty

    ]

logbook ::
  Log
logbook =
  Log
    "Tony John Morris"
    "19771102"
    "1007036"
    entries

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        [md, h] ->
          do  writeMarkdownFile md logbook
              writeHtmlFile h logbook
        _ -> 
          hPutStrLn stderr "args: <markdown-file> <html-file>"
